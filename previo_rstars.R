## =============================================================================
## Capítulo 5. Tratamiento y análisis previo de datos.
##
## VERSIÓN CON {MATrstars}
##
## Este script y su gemelo "previo_puro_rstars.R" recorren exactamente la misma
## práctica y producen los mismos data frames, objetos, gráficos y resultados.
## La única diferencia está en el modo de escribir tres tareas recurrentes:
##
##   - aquí se resuelven con las funciones auxiliares del paquete {MATrstars}
##     (explora_na(), explora_outliers() y kable_rstars());
##   - en "previo_puro_rstars.R" se resuelven "a mano", con código de {visdat},
##     {dplyr}, {ggplot2} y {kableExtra}, sin cargar {MATrstars}.
##
## Ambos scripts comparten los nombres de los objetos (muestra, muestra_so,
## muestra2, muestra2_so, k, estadisticos, g1...g4, resumen, corr_plot_so...),
## de modo que se puede pasar de uno a otro sin perder el hilo de la práctica.
## =============================================================================


## ---- prep-limpieza ----------------------------------------------------------

# Limpiando el Global Environment
rm(list = ls())


## ---- prep-paquetes ----------------------------------------------------------

# Cargando paquetes
library (readxl)
library (gtExtras)
library (dplyr)
library (visdat)
library (ggplot2)
library (knitr)
library (kableExtra)
library (moments)   # paquete necesario para calcular la curtosis.
library (patchwork)
library (GGally)    # cálculo de la matriz de correlaciones.
library (gridExtra)


## ---- prep-matrstars ---------------------------------------------------------

# Paquete MATrstars: funciones auxiliares del libro R-Stars.
# No está en CRAN, sino en GitHub. Si no está instalado, se instala una sola
# vez y después basta con activarlo como cualquier otro paquete.
if (!requireNamespace("MATrstars", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("teckel71/MATrstars")
}
library(MATrstars)


## ---- datos-importacion ------------------------------------------------------

## DATOS

# Importando datos desde Excel
interestelar_100 <- read_excel("interestelar_100.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_100 <- data.frame(interestelar_100, row.names = 1)

# Visualizando el data frame de modo elegante con {gtExtras}
datos_df_graph <- gt_plt_summary(interestelar_100)
datos_df_graph


## =============================================================================
## ANÁLISIS DE UNA VARIABLE: RENECO
## =============================================================================

## ---- uni-copia --------------------------------------------------------------

## Análisis de una variable.

# Copia del df original, para preservar su integridad.
muestra <- interestelar_100


## ---- uni-na -----------------------------------------------------------------

# Missing values: diagnóstico gráfico, listado de casos afectados y
# eliminación, todo en una sola llamada.
muestra <- explora_na(
  muestra,
  variables = RENECO,
  accion    = "eliminar",
  titulo    = "Rentabilidad Económica: valores ausentes",
  subtitulo = "Transporte de mercancías interestelar"
)

# "muestra" conserva las 27 variables de "interestelar_100" y pierde el único
# caso sin dato en RENECO: 103 empresas.


## ---- uni-outliers -----------------------------------------------------------

# Outliers: box-plot, listado de casos atípicos (regla de 1,5·IQR) y
# eliminación, también en una sola llamada.
muestra_so <- explora_outliers(
  muestra,
  variables = RENECO,
  accion    = "eliminar",
  titulo    = "Rentabilidad Económica",
  subtitulo = "Empresas de transporte interestelar"
)

# "muestra_so" tiene las mismas variables que "muestra" y dos casos menos: 101.


## ---- uni-tabla-frecuencias --------------------------------------------------

## Descripción de la variable.

# Tabla de datos (distribución de frecuencias agrupadas en intervalos)
muestra_so <- muestra_so %>% arrange(RENECO, row.names(muestra_so))

# Número de intervalos por el método de Sturges
k <- nclass.Sturges(muestra_so$RENECO)

# Crear los intervalos
muestra_so$intervalos <- cut(muestra_so$RENECO, breaks = k, include.lowest = TRUE)

# Contar frecuencias y construir el data frame de la distribución
conteo_intervalos_df <- as.data.frame(table(muestra_so$intervalos))
colnames(conteo_intervalos_df) <- c("Intervalo", "Frecuencia")
N_agre <- sum(conteo_intervalos_df$Frecuencia)
conteo_intervalos_df$Frecuencia_acum   <- cumsum(conteo_intervalos_df$Frecuencia)
conteo_intervalos_df$Frecuencia_R      <- conteo_intervalos_df$Frecuencia / N_agre
conteo_intervalos_df$Frecuencia_R_acum <- cumsum(conteo_intervalos_df$Frecuencia_R)

# Mostrar la tabla con el estilo tipográfico del libro
conteo_intervalos_df %>%
  kable_rstars(caption   = "Dist. frecuencias agrupadas: Rentabilidad Económica",
               col.names = c("Intervalo", "n(i)", "N(i)", "f(i)", "F(i)"),
               digits    = c(NA, 0, 0, 2, 2))


## ---- uni-g1-histograma ------------------------------------------------------

## Descriptivos básicos: gráficos

g1 <-
ggplot(data = muestra_so, aes(x = RENECO)) +
  geom_histogram(bins = k,
                 colour = "red",
                 fill = "orange",
                 alpha = 0.7) +
  geom_vline(xintercept = mean(muestra_so$RENECO),
             color = "dark blue",
             linewidth = 1.2,
             alpha = 0.8) +
  ggtitle("Histograma") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Frecuencias")

g1


## ---- uni-g2-densidad --------------------------------------------------------

g2 <-
ggplot(data = muestra_so, aes(x = RENECO)) +
  geom_density(colour = "red",
               fill = "orange",
               alpha = 0.7) +
  geom_vline(xintercept = mean(muestra_so$RENECO),
             color = "dark blue",
             linewidth = 0.8,
             alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(muestra_so$RENECO),
                                         sd = sd(muestra_so$RENECO)),
                geom = "area",
                color = "darkblue",
                fill = "yellow",
                alpha = 0.2) +
  ggtitle("Gráfico de densidad vs curva normal") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Densidad")

g2


## ---- uni-g3-boxplot ---------------------------------------------------------

g3 <-
ggplot(data = muestra_so, aes(x = "", y = RENECO)) +
  geom_boxplot(color = "red",
               fill = "orange",
               outlier.shape = NA) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  geom_jitter(width = 0.1,
              size = 1,
              col = "darkred",
              alpha = 0.50) +
  ggtitle("Box-Plot") +
  ylab("Rentabilidad Económica (%)")

g3


## ---- uni-estadisticos -------------------------------------------------------

# Calcular estadísticos
estadisticos <- muestra_so %>% summarise( Media     = mean(RENECO),
                                          DT        = sd(RENECO),
                                          Mínimo    = min(RENECO),
                                          Mediana   = median(RENECO),
                                          Maximo    = max(RENECO),
                                          Asimetria = skewness(RENECO),
                                          Curtosis  = kurtosis(RENECO) - 3)

# Mostrar estadísticos
estadisticos %>%
  kable_rstars(caption   = "Principales Estadísticos de la Rentabilidad Económica",
               col.names = c("Media", "Desviación Típica",
                             "Valor mínimo", "Mediana",
                             "Valor Máximo", "C. Asimetría Fisher",
                             "C. Curtosis Fisher"),
               digits    = c(2, 2, 2, 2, 2, 2, 2))


## ---- uni-g4-qq --------------------------------------------------------------

## Normalidad

# Gráfico QQ
g4 <-
ggplot(data = muestra_so, aes(sample = RENECO)) +
  stat_qq(colour = "red") +
  stat_qq_line(colour = "dark blue") +
  ggtitle("QQ-Plot")

g4


## ---- uni-shapiro ------------------------------------------------------------

# Prueba de Shapiro-Wilk
shapiro.test(x = muestra_so$RENECO)


## ---- uni-resumen ------------------------------------------------------------

## Resumen gráfico: los 4 gráficos básicos en una sola imagen.

resumen <- (g1 | g2)/(g3 | g4)
resumen <- resumen +
  plot_annotation(
    title = "Rentabilidad Económica",
    subtitle = "Empresas TMI (sin outliers)",
    theme = theme(
      # TÍTULO de la composición
      plot.title = element_text(
        size = 16,          # tamaño
        face = "bold"       # negrita
      ),
      # SUBTÍTULO de la composición
      plot.subtitle = element_text(
        size = 12
      )))
resumen


## =============================================================================
## ANÁLISIS DE MÚLTIPLES VARIABLES: RENECO, ACTIVO, MARGEN, RES
## =============================================================================

## ---- multi-copia ------------------------------------------------------------

## Trabajando con múltiples variables.

# Copia del df original.
muestra2 <- interestelar_100


## ---- multi-na ---------------------------------------------------------------

# Missing values de las cuatro variables del análisis.
muestra2 <- explora_na(
  muestra2,
  variables = c(RENECO, ACTIVO, MARGEN, RES),
  accion    = "eliminar",
  titulo    = "Variables económico-financieras: valores ausentes",
  subtitulo = "Transporte de mercancías interestelar"
)

# "muestra2" pierde los 3 casos con algún valor ausente: 101 empresas.


## ---- multi-outliers ---------------------------------------------------------

# Outliers multivariantes. Al recibir varias variables, explora_outliers()
# calcula internamente la distancia de Mahalanobis y le aplica la regla
# de 1,5·IQR.
muestra2_so <- explora_outliers(
  muestra2,
  variables = c(RENECO, ACTIVO, MARGEN, RES),
  accion    = "eliminar",
  titulo    = "DISTANCIA DE MAHALANOBIS",
  subtitulo = "RENECO, ACTIVO, MARGEN, RES. Empresas TMI."
)

# "muestra2_so" conserva 91 empresas.


## ---- multi-correlaciones ----------------------------------------------------

## Correlaciones entre variables.

# Matriz de correlaciones SIN outliers
temporal <- muestra2_so %>% select(RENECO, ACTIVO, MARGEN, RES)
corr_plot_so <- ggpairs(temporal,
                        lower = list(continuous = wrap("cor",
                                                       size = 4.5,
                                                       method = "pearson",
                                                       stars = TRUE)),
                        title = "Matriz de Correlación (sin outliers)")
rm(temporal)
corr_plot_so


## ---- multi-correlaciones-comparacion ----------------------------------------

# Matriz de correlaciones CON outliers, para comparar
temporal <- muestra2 %>% select(RENECO, ACTIVO, MARGEN, RES)
corr_plot_co <- ggpairs(temporal,
                        lower = list(continuous = wrap("cor",
                                                       size = 4.5,
                                                       method = "pearson",
                                                       stars = TRUE)),
                        title = "Matriz de Correlación (con outliers)")
rm(temporal)

# Presentación conjunta de ambas matrices
corr_plot_so_gg <- ggmatrix_gtable(corr_plot_so)
corr_plot_co_gg <- ggmatrix_gtable(corr_plot_co)

grid.arrange(corr_plot_so_gg, corr_plot_co_gg, ncol = 2, top = "CORRELACIONES")


# Fin de script :)
