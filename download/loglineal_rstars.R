#### Modelos log-lineales para Tablas de Contingencia Multidimensionales #######
################################################################################

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes

library (readxl)
library (dplyr)
library (ggplot2)
library (gtExtras)
library (visdat)
library (pander)     # Tablas multifactoriales compactas en Rmarkdown
library (vcd)        # Visualización tablas de contingencia
library (MASS)       # Estimación log-lineales

# Paquete MATrstars: funciones auxiliares del libro R-Stars. Contiene, entre
# otras, las funciones kable_rstars(), detect_zeros_any(), impute_zeros_with_one(),
# extraer_coeficientes(), generar_solucion() y step_loglm_backward(), utilizadas
# más adelante en el flujo principal.
# Si el paquete no está instalado, se instala desde GitHub (una sola vez).
if (!requireNamespace("MATrstars", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("teckel71/MATrstars")
}
library(MATrstars)

# DATOS

## Importando datos

library (readxl)
interestelar_300 <- read_excel("interestelar_300.xlsx", sheet ="Datos")
interestelar_300 <- data.frame(interestelar_300, row.names = 1)

# Seleccionando variables para el analisis.
seleccion <- interestelar_300 %>%
  dplyr::select(GALAXIA,
                FJUR,
                EFLO)
seleccion_df_graph <- gt_plt_summary(seleccion)
seleccion_df_graph

# Localizando missing values.
seleccion %>%
  vis_miss() +
  labs(title = "Tablas de contingencia.",
       subtitle = "Transporte de mercancías interestelar",
       y = "Observación",
       fill = NULL) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "grey"),
    labels = c("TRUE" = "NA", "FALSE" = "Presente")) +
  theme(
    plot.title = element_text(face = "bold", size = 14))

seleccion %>% filter(is.na(GALAXIA) |
                       is.na(FJUR) |
                       is.na(EFLO))%>%
              dplyr::select(GALAXIA,
                            FJUR,
                            EFLO)

seleccion <- seleccion %>%
  filter(! is.na(GALAXIA) &
           ! is.na(FJUR) &
           ! is.na(EFLO)) %>%
  mutate(GALAXIA = recode(GALAXIA,
                          "Gran Nube de Magallanes" = "GN Mag.",
                          "Pequeña Nube de Magallanes" = "PN Mag.",
                          "Galaxia de Andrómeda" = "Andrómeda",
                          "Galaxia del Triángulo" = "Triángulo",
                          "Vía Láctea" = "V. Láctea"))

# Pasar variables categóricas a factores
seleccion <- seleccion %>% mutate(across(everything(), as.factor))

# Construir tabla original
tab0 <- xtabs(~ GALAXIA + FJUR + EFLO, data = seleccion)

# Reportar celdas 0 (si existen) — es puramente informativo.
# NO se imputa por defecto: los ceros muestrales llevan a que el MLE del
# modelo saturado no exista en sentido estricto (coeficientes Inf/-Inf/NaN),
# pero step() opera correctamente sin necesidad de imputar. Véase Agresti
# (2013), cap. 9. Las funciones detect_zeros_any() e impute_zeros_with_one()
# quedan disponibles en MATrstars por si el usuario desea inspeccionar o
# manipular las celdas nulas en otros contextos.
zeros_report <- detect_zeros_any(tab0)

if (is.null(zeros_report)) {
  message("No se han detectado celdas con frecuencia 0 en la tabla original.")
} else {
  zeros_report %>%
    kable_rstars(caption = "Combinaciones con frecuencia 0 detectadas")
}

# Mostrar tabla

pander("Tabla de contingencia GALAXIA × FJUR × EFLO")
pander(tab0, style= "rmarkdown")

# Representación gráfica de la tabla con mosaico
mosaic(tab0,
       main = "Galaxia, Forma Jurídica y Antigüedad media de la flota",
       sub = "Empresas TMI",
       gp = shading_Marimekko(tab0),
       labeling_args = list(rot_labels = c(0, 45),
                            gp_labels = gpar(fontsize = 8)),
       main_gp = gpar(fontsize = 16),
       sub_gp = gpar(fontsize = 14))

# ESTIMACIÓN

# Modelo Independencia.

modelo_indep <- MASS::loglm(~ GALAXIA + EFLO + FJUR,
                            data= tab0)

solucion_modelo_indep <- generar_solucion(modelo_indep)

solucion_modelo_indep$Informacion
solucion_modelo_indep$Coeficientes

# Modelo Saturado.

modelo_sat <- MASS::loglm(~ GALAXIA * EFLO * FJUR,
                          data= tab0)

solucion_modelo_sat <- generar_solucion(modelo_sat)
solucion_modelo_sat$Informacion
solucion_modelo_sat$Coeficientes

# Elección del modelo final.

modelo_def <- step_loglm_backward(~ GALAXIA * EFLO * FJUR,
                                  data  = tab0,
                                  trace = TRUE, steps = 1000)

solucion_modelo_def <- generar_solucion(modelo_def)

solucion_modelo_def$Informacion
solucion_modelo_def$Coeficientes

# Fin del script :)