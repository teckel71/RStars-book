## Tratamiento y análisis previo de datos.

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library (readxl)
library (gtExtras)
library (dplyr)
library (visdat)
library (ggplot2)
library (knitr)
library (kableExtra)
library (moments) # paquete necesario para calcular la curtosis.
library (patchwork)
library (GGally) # Cáculo de matriz de correlaciones.

# Paquete MATrstars: funciones auxiliares del libro R-Stars.
# Contiene, entre otras, la función kable_rstars(), que utilizaremos por
# primera vez en este capítulo.
# Si el paquete no está instalado, se instala desde GitHub (una sola vez).
if (!requireNamespace("MATrstars", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("teckel71/MATrstars")
}
library(MATrstars)

## DATOS

# Importando datos desde Excel
interestelar_100 <- read_excel("interestelar_100.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_100 <- data.frame(interestelar_100, row.names = 1)

# visualizando el data frame de modo elegante con {gtExtras}
datos_df_graph <- gt_plt_summary(interestelar_100)
datos_df_graph

## Analisis de una variable.

# Copia de df
muestra <- interestelar_100

# Localizando missing values.
muestra %>%
  select (RENECO) %>%
  vis_miss() +
  labs(title = "Rentabilidad Económica: valores ausentes",
       subtitle = "Transporte de mercancías interestelar",
       y = "Observación",
       fill = NULL) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "grey"),
    labels = c("TRUE" = "NA", "FALSE" = "Presente")) +
  theme(
    plot.title = element_text(face = "bold", size = 14))

# Mostrar casos concretos de NAs
muestra %>% filter(is.na(RENECO)) %>% select(RENECO)

# Eliminar casos con NAs
muestra <- muestra %>%
             filter(! is.na(RENECO)) %>% select(RENECO)

# --- Nota: el mismo procedimiento con explora_na() del paquete {MATrstars} ---
# La función explora_na() encapsula el patrón vis_miss() + labs() +
# scale_fill_manual() + theme() (con la corrección estética del posicionamiento
# de los nombres de variable cuando son pocas) + filter(is.na(...)) para listar
# los casos afectados + filter(!is.na(...)) para eliminarlos. Todo ese bloque
# se reduce a una única llamada. Como demostración, la aplicamos a una copia
# limpia del df original con solo la variable RENECO:

muestra_demo <- select(interestelar_100, RENECO)
muestra_demo <- explora_na(
  muestra_demo,
  variables = RENECO,
  accion    = "eliminar",
  titulo    = "Rentabilidad Económica: valores ausentes",
  subtitulo = "Transporte de mercancías interestelar"
)
rm(muestra_demo)

# A partir de aquí, y en los capítulos siguientes, el tratamiento de missing
# values se realizará con explora_na(). La ficha completa de la función
# (argumentos, defaults, código fuente) se encuentra en el "Apéndice:
# Funciones auxiliares del paquete MATrstars" al final del libro.
# -----------------------------------------------------------------------------

## Localizando outliers
ggplot(data = muestra, aes(y = RENECO)) +
    geom_boxplot(fill = "orange") +
    ggtitle("Rentabilidad Económica",
            subtitle = "Empresas de transporte interestelar") +
            ylab("Rentabilidad Económica (%)") +
    theme(plot.title = element_text(face = "bold", size = 14))

# Mostrar casos concretos de outliers
Q1 <- quantile (muestra$RENECO, c(0.25))
Q3 <- quantile (muestra$RENECO, c(0.75))

muestra %>% 
  filter(RENECO > Q3 + 1.5*IQR(RENECO) |
         RENECO < Q1 - 1.5*IQR(RENECO)) %>%
  select(RENECO)

# Eliminar outliers
muestra_so <- muestra %>%
              filter(RENECO <= Q3 + 1.5*IQR(RENECO) &
                     RENECO >= Q1 - 1.5*IQR(RENECO))

# --- Nota: el mismo procedimiento con explora_outliers() del paquete {MATrstars}
# La función explora_outliers() encapsula el patrón boxplot + calculo de Q1/Q3
# + filter(...) para listar los outliers + filter(...) para eliminarlos. Todo el
# bloque anterior se reduce a una única llamada. Como demostración, la aplicamos
# a una copia limpia del df original con solo la variable RENECO (sin NAs):

muestra_demo <- select(interestelar_100, RENECO)
muestra_demo <- explora_na(muestra_demo, RENECO, accion = "eliminar")
muestra_demo <- explora_outliers(
  muestra_demo,
  variables = RENECO,
  accion    = "eliminar",
  titulo    = "Rentabilidad Económica",
  subtitulo = "Empresas de transporte interestelar"
)
rm(muestra_demo)
# ------------------------------------------------------------------------------


# Tabla de datos (distribución de frecuencias agrupadas en intervalos)
# Colocando casos
muestra_so <- muestra_so %>% arrange(RENECO, row.names(muestra_so))

# Fijar k como número de intervalos (método de Sturges)
k <- nclass.Sturges(muestra_so$RENECO)

# Crear los intervalos
muestra_so$intervalos <- cut(muestra_so$RENECO, breaks = k, include.lowest = TRUE)

# Contar las frecuencias de cada intervalo
conteo_intervalos <- table(muestra_so$intervalos)

# Convertir el resultado a un data frame para una mejor visualización
conteo_intervalos_df <- as.data.frame(conteo_intervalos)

# Renombrar las columnas para mayor claridad
colnames(conteo_intervalos_df) <- c("Intervalo", "Frecuencia")

# Calcular y guardar la frecuencia total
N_agre <- sum(conteo_intervalos_df$Frecuencia)

# Calcular frecuencias absolutas acumuladas
conteo_intervalos_df$Frecuencia_acum <- cumsum(conteo_intervalos_df$Frecuencia)

# Calcular frecuencias relativas
conteo_intervalos_df$Frecuencia_R <- conteo_intervalos_df$Frecuencia / N_agre

# Calcular frecuencias relativas acumuladas
conteo_intervalos_df$Frecuencia_R_acum <- cumsum(conteo_intervalos_df$Frecuencia_R)

# Mostrar el resultado
conteo_intervalos_df %>%
  kable(caption = "Distribución de frecuencias agrupadas en intervalos de Rentabilidad Económica",
        col.names = c("Intervalo rentabilidad", "Frecuencia absoluta n(i)",
                      "Frecuencia absoluta acum. N(i)", "Frecuencia relativa f(i)",
                      "Frecuencia relativa acum. F(i)"),
        digits    = c(NA, 0, 0, 2, 2),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width = F, bootstrap_options = "striped",
                "bordered", "condensed",
                position = "center", font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(conteo_intervalos_df)), bold= F, align = "c")

# --- Nota: la misma tabla con kable_rstars() del paquete {MATrstars} ---------
# La función kable_rstars() encapsula el patrón kable() + kable_styling() +
# row_spec() con los ajustes habituales del libro (encabezado en negrita
# centrado, filas rayadas, bordes, cuerpo centrado, fuente de 11 puntos, punto
# como separador decimal, sin notación científica). El código de arriba (12
# líneas) se reduce a 4-5 líneas y el resultado visual es idéntico:

conteo_intervalos_df %>%
  kable_rstars(caption   = "Distribución de frecuencias agrupadas en intervalos de Rentabilidad Económica",
               col.names = c("Intervalo rentabilidad", "Frecuencia absoluta n(i)",
                             "Frecuencia absoluta acum. N(i)", "Frecuencia relativa f(i)",
                             "Frecuencia relativa acum. F(i)"),
               digits    = c(NA, 0, 0, 2, 2))

# A partir de aquí, y en los capítulos siguientes, las tablas se generarán
# mayoritariamente con kable_rstars(). La ficha completa de la función
# (argumentos, defaults, código fuente) se encuentra en el "Apéndice:
# Funciones auxiliares del paquete MATrstars" al final del libro.
# -----------------------------------------------------------------------------

## Descriptivos básicos

# Gráficos básicos

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
  ggtitle("Histograma")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Frecuencias")

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
  ggtitle("Gráfico de densidad vs curva normal")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Densidad")

g2

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

# Calcular estadísticos
estadisticos <- muestra_so %>% summarise( Media = mean(RENECO),
                                          DT = sd(RENECO),
                                          Mínimo = min(RENECO),
                                          Mediana = median(RENECO),
                                          Maximo = max(RENECO),
                                          Asimetria = skewness(RENECO),
                                          Curtosis = kurtosis(RENECO) - 3)

# Mostrar estadisticos
estadisticos %>%
  kable_rstars(caption   = "Principales Estadísticos de la Rentabilidad Económica",
               col.names = c("Media", "Mediana",
                             "Desviación Típica", "Valor mínimo",
                             "Valor Máximo", "C. Asimetría Fisher",
                             "C. Curtosis Fisher"),
               digits    = c(2, 2, 2, 2, 2, 2, 2))

## Normalidad

# Grafico QQ
g4 <-
ggplot(data = muestra_so, aes(sample = RENECO)) +
  stat_qq(colour = "red") + 
  stat_qq_line(colour = "dark blue") +
  ggtitle("QQ-Plot")

g4

# Prueba de Shapiro-Wilk

shapiro.test(x = muestra_so$RENECO)

## Resumen gráfico
## Resumen gráfico

resumen <- (g1 | g2)/(g3 | g4)
resumen <- resumen + 
  plot_annotation(
    title = "Rentabilidad Económica",
    subtitle = "Empresas TMI (sin outliers)",
    theme = theme(
      # TÍTULO de la composición
      plot.title = element_text(
        size = 16,          # tamaño
        face = "bold",      # negrita
      ),
      # SUBTÍTULO de la composición
      plot.subtitle = element_text(
        size = 12
      )))
resumen

## Trabajando con multiples variables.

# Copia de df original.
muestra2 <- interestelar_100

# Diagnóstico y filtrado de missing values con explora_na().
muestra2 <- explora_na(
  muestra2,
  variables = c(RENECO, ACTIVO, MARGEN, RES),
  accion    = "eliminar",
  titulo    = "Variables económico-financieras: valores ausentes",
  subtitulo = "Transporte de mercancías interestelar"
)

# Identificando y descartando outliers con distancia de Mahalanobis.
muestra2 <- muestra2 %>%
  mutate(
    MAHALANOBIS = {
      X <- pick(RENECO, ACTIVO, MARGEN, RES)
      mahalanobis(as.matrix(X),
                  center = colMeans(X),
                  cov = cov(X))
    }
  )

ggplot(data = muestra2, aes(y = MAHALANOBIS)) +
    geom_boxplot(fill = "orange") +
    ggtitle("DISTANCIA DE MAHALANOBIS",
            subtitle = "RENECO, ACTIVO, MARGEN, RES. Empresas TMI.") +
    ylab("MAHALANOBIS")
  
Q1M <- quantile (muestra2$MAHALANOBIS, c(0.25))
Q3M <- quantile (muestra2$MAHALANOBIS, c(0.75))

muestra2 %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
         MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS))%>%
  select(MAHALANOBIS, RENECO, ACTIVO, MARGEN, RES) 
  
muestra2_so <- muestra2 %>%
  filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
           MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS))  

# --- Nota: el mismo procedimiento con explora_outliers() del paquete {MATrstars}
# En el caso multivariante, explora_outliers() detecta automáticamente que hay
# varias variables y calcula internamente la distancia de Mahalanobis, sobre la
# que aplica la regla 1.5·IQR. Así, el bloque de arriba (mutate MAHALANOBIS +
# boxplot + calculo Q1M/Q3M + filter para listar + filter para eliminar) se
# reduce también a una única llamada. Como demostración, la aplicamos a una
# copia limpia del df original con solo las variables de interés:

muestra2_demo <- select(interestelar_100, RENECO, ACTIVO, MARGEN, RES)
muestra2_demo <- explora_na(muestra2_demo, accion = "eliminar")
muestra2_demo <- explora_outliers(
  muestra2_demo,
  variables = c(RENECO, ACTIVO, MARGEN, RES),
  accion    = "eliminar",
  titulo    = "DISTANCIA DE MAHALANOBIS",
  subtitulo = "RENECO, ACTIVO, MARGEN, RES. Empresas TMI."
)
rm(muestra2_demo)

# A partir de aquí, y en los capítulos siguientes, la detección y eliminación
# de outliers se realizará con explora_outliers(). La ficha completa de la
# función (argumentos, defaults, código fuente) se encuentra en el "Apéndice:
# Funciones auxiliares del paquete MATrstars" al final del libro.
# ------------------------------------------------------------------------------

## Correlaciones entre variables.

temporal <- muestra2_so %>% select(RENECO, ACTIVO, MARGEN, RES)
corr_plot_so <- ggpairs(temporal, 
                        lower = list(continuous = wrap("cor",
                                                       size = 4.5,
                                                       method = "pearson",
                                                       stars = TRUE)),
                        title = "Matriz de Correlación sin outliers")
rm(temporal)
corr_plot_so

# Fin de script :)

