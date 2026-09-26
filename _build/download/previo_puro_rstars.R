## =============================================================================
## Capítulo 5. Tratamiento y análisis previo de datos.
##
## VERSIÓN "PURA" (sin {MATrstars})
##
## Este script y su gemelo "previo_rstars.R" recorren exactamente la misma
## práctica y producen los mismos data frames, objetos, gráficos y resultados.
## La única diferencia está en el modo de escribir tres tareas recurrentes:
##
##   - en "previo_rstars.R" se resuelven con las funciones auxiliares del
##     paquete {MATrstars} (explora_na(), explora_outliers() y kable_rstars());
##   - aquí se resuelven "a mano", con código de {visdat}, {dplyr}, {ggplot2},
##     {knitr} y {kableExtra}. El paquete {MATrstars} NO se carga.
##
## Ambos scripts comparten los nombres de los objetos (muestra, muestra_so,
## muestra2, muestra2_so, k, estadisticos, g1...g4, resumen, corr_plot_so...).
## Esta versión crea, además, unos cuantos objetos auxiliares que allí quedan
## ocultos dentro de las funciones (los cuartiles Q1, Q3, Q1M y Q3M, los
## listados de casos y la distancia de Mahalanobis). Se eliminan al final,
## de modo que los dos scripts terminan con el mismo Global Environment.
## =============================================================================


## ---- p-prep-limpieza --------------------------------------------------------

# Limpiando el Global Environment
rm(list = ls())


## ---- p-prep-paquetes --------------------------------------------------------

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

# (Aquí NO se carga {MATrstars}: todo se hará con código "puro".)


## ---- p-datos-importacion ----------------------------------------------------

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

## ---- p-uni-copia ------------------------------------------------------------

## Análisis de una variable.

# Copia del df original, para preservar su integridad.
muestra <- interestelar_100


## ---- p-uni-na-grafico -------------------------------------------------------

# Localizando missing values.
muestra %>%
  select (RENECO) %>%
  vis_miss() +
    labs(title    = "Rentabilidad Económica: valores ausentes",
         subtitle = "Transporte de mercancías interestelar",
         y        = "Observación",
         fill     = NULL) +
    scale_fill_manual(
      values = c("TRUE" = "red", "FALSE" = "grey"),
      labels = c("TRUE" = "NA", "FALSE" = "Presente")) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      # Con pocas variables (hasta 4), los nombres de columna quedan por
      # defecto muy despegados del panel superior. Esto los recoloca.
      axis.text.x.top = element_text(angle  = 0,
                                     hjust  = 0.5,
                                     vjust  = 1,
                                     margin = margin(b = 2)))


## ---- p-uni-na-casos ---------------------------------------------------------

# Localizar los casos concretos con NA y mostrarlos en una tabla con estilo.
casos_na <- muestra %>% filter(is.na(RENECO)) %>% select(RENECO)



## ---- p-uni-na-tabla ---------------------------------------------------------

casos_na %>%
  kable(caption     = paste0("Casos con valores ausentes (", nrow(casos_na),
                             ") en: RENECO"),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width        = FALSE,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = TRUE,  align = "c") %>%
  row_spec(1:nrow(casos_na), bold = FALSE, align = "c")


## ---- p-uni-na-eliminar ------------------------------------------------------

# Eliminar los casos con NA
muestra <- muestra %>% filter(! is.na(RENECO))

# "muestra" conserva las 27 variables de "interestelar_100" y pierde el único
# caso sin dato en RENECO: 103 empresas.


## ---- p-uni-outliers-grafico -------------------------------------------------

# Localizando outliers
ggplot(data = muestra, aes(y = RENECO)) +
  geom_boxplot(fill = "orange", alpha = 0.9) +
  labs(title    = "Rentabilidad Económica",
       subtitle = "Empresas de transporte interestelar",
       y        = "RENECO",
       x        = NULL) +
  theme(plot.title   = element_text(face = "bold", size = 14),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())


## ---- p-uni-outliers-casos ---------------------------------------------------

# Localizar los casos atípicos por la regla de Tukey (1,5·IQR)
Q1 <- quantile (muestra$RENECO, c(0.25))
Q3 <- quantile (muestra$RENECO, c(0.75))

casos_out <- muestra %>%
  filter(RENECO > Q3 + 1.5*IQR(RENECO) |
         RENECO < Q1 - 1.5*IQR(RENECO)) %>%
  select(RENECO) %>%
  arrange(RENECO)



## ---- p-uni-outliers-tabla ---------------------------------------------------

casos_out %>%
  kable(caption     = paste0("Casos outliers en RENECO (", nrow(casos_out), ")"),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width        = FALSE,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = TRUE,  align = "c") %>%
  row_spec(1:nrow(casos_out), bold = FALSE, align = "c")


## ---- p-uni-outliers-eliminar ------------------------------------------------

# Eliminar outliers
muestra_so <- muestra %>%
  filter(RENECO <= Q3 + 1.5*IQR(RENECO) &
         RENECO >= Q1 - 1.5*IQR(RENECO))

# "muestra_so" tiene las mismas variables que "muestra" y dos casos menos: 101.


## ---- p-uni-tabla-frecuencias ------------------------------------------------

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

# Mostrar la tabla con {knitr} y {kableExtra}
conteo_intervalos_df %>%
  kable(caption     = "Dist. frecuencias agrupadas: Rentabilidad Económica",
        col.names   = c("Intervalo", "n(i)", "N(i)", "f(i)", "F(i)"),
        digits      = c(NA, 0, 0, 2, 2),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width        = FALSE,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = TRUE,  align = "c") %>%
  row_spec(1:nrow(conteo_intervalos_df), bold = FALSE, align = "c")


## ---- p-uni-g1-histograma ----------------------------------------------------

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


## ---- p-uni-g2-densidad ------------------------------------------------------

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


## ---- p-uni-g3-boxplot -------------------------------------------------------

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


## ---- p-uni-estadisticos -----------------------------------------------------

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
  kable(caption     = "Principales Estadísticos de la Rentabilidad Económica",
        col.names   = c("Media", "Desviación Típica",
                        "Valor mínimo", "Mediana",
                        "Valor Máximo", "C. Asimetría Fisher",
                        "C. Curtosis Fisher"),
        digits      = c(2, 2, 2, 2, 2, 2, 2),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width        = FALSE,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = TRUE,  align = "c") %>%
  row_spec(1:nrow(estadisticos), bold = FALSE, align = "c")


## ---- p-uni-g4-qq ------------------------------------------------------------

## Normalidad

# Gráfico QQ
g4 <-
ggplot(data = muestra_so, aes(sample = RENECO)) +
  stat_qq(colour = "red") +
  stat_qq_line(colour = "dark blue") +
  ggtitle("QQ-Plot")

g4


## ---- p-uni-shapiro ----------------------------------------------------------

# Prueba de Shapiro-Wilk
shapiro.test(x = muestra_so$RENECO)


## ---- p-uni-resumen ----------------------------------------------------------

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

## ---- p-multi-copia ----------------------------------------------------------

## Trabajando con múltiples variables.

# Copia del df original.
muestra2 <- interestelar_100


## ---- p-multi-na-grafico -----------------------------------------------------

# Localizando missing values en las cuatro variables del análisis.
muestra2 %>%
  select (RENECO, ACTIVO, MARGEN, RES) %>%
  vis_miss() +
    labs(title    = "Variables económico-financieras: valores ausentes",
         subtitle = "Transporte de mercancías interestelar",
         y        = "Observación",
         fill     = NULL) +
    scale_fill_manual(
      values = c("TRUE" = "red", "FALSE" = "grey"),
      labels = c("TRUE" = "NA", "FALSE" = "Presente")) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.x.top = element_text(angle  = 0,
                                     hjust  = 0.5,
                                     vjust  = 1,
                                     margin = margin(b = 2)))


## ---- p-multi-na-casos -------------------------------------------------------

# Localizar los casos con algún NA en las cuatro variables. Nótese el uso del
# operador "|" (o): basta con que falte uno cualquiera de los cuatro datos.
casos_na2 <- muestra2 %>%
  filter(is.na(RENECO) | is.na(ACTIVO) | is.na(MARGEN) | is.na(RES)) %>%
  select(RENECO, ACTIVO, MARGEN, RES)



## ---- p-multi-na-tabla -------------------------------------------------------

casos_na2 %>%
  kable(caption     = paste0("Casos con valores ausentes (", nrow(casos_na2),
                             ") en: RENECO, ACTIVO, MARGEN, RES"),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width        = FALSE,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = TRUE,  align = "c") %>%
  row_spec(1:nrow(casos_na2), bold = FALSE, align = "c")


## ---- p-multi-na-eliminar ----------------------------------------------------

# Eliminar los casos con algún NA. Como pedimos justo lo contrario que antes,
# hay que negar cada condición y cambiar el "|" por el "&".
muestra2 <- muestra2 %>%
  filter(!is.na(RENECO) & !is.na(ACTIVO) & !is.na(MARGEN) & !is.na(RES))

# "muestra2" pierde los 3 casos con algún valor ausente: 101 empresas.


## ---- p-multi-mahalanobis ----------------------------------------------------

# Identificando outliers con la distancia de Mahalanobis. La calculamos sobre
# una copia auxiliar, "muestra2_mah", porque es una medida de trabajo y no una
# variable más del análisis: no debe quedarse en el data frame definitivo.
muestra2_mah <- muestra2 %>%
  mutate(
    MAHALANOBIS = {
      X <- pick(RENECO, ACTIVO, MARGEN, RES)
      mahalanobis(as.matrix(X),
                  center = colMeans(X),
                  cov = cov(X))
    }
  )


## ---- p-multi-outliers-grafico -----------------------------------------------

# Box-plot de la distancia de Mahalanobis.
#
# Aquí los valores extremos son tan grandes que aplastarían la caja hasta
# hacerla ilegible: el rango intercuartílico apenas ocupa un 6% del rango
# total. Por eso transformamos el eje Y con una escala pseudologarítmica, que
# comprime los valores grandes sin alterar los pequeños y que, a diferencia
# del logaritmo puro, admite valores cero y negativos.
ggplot(data = muestra2_mah, aes(y = MAHALANOBIS)) +
  geom_boxplot(fill = "orange", alpha = 0.9) +
  labs(title    = "DISTANCIA DE MAHALANOBIS",
       subtitle = paste("RENECO, ACTIVO, MARGEN, RES. Empresas TMI.",
                        "(eje Y en escala pseudo-logaritmica por rango extremo)",
                        sep = "\n"),
       y        = "Distancia de Mahalanobis",
       x        = NULL) +
  scale_y_continuous(trans = "pseudo_log") +
  theme(plot.title   = element_text(face = "bold", size = 14),
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank())


## ---- p-multi-outliers-casos -------------------------------------------------

# Localizar los casos atípicos (regla de 1,5·IQR sobre MAHALANOBIS)
Q1M <- quantile (muestra2_mah$MAHALANOBIS, c(0.25))
Q3M <- quantile (muestra2_mah$MAHALANOBIS, c(0.75))

casos_out2 <- muestra2_mah %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
         MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS)) %>%
  select(MAHALANOBIS, RENECO, ACTIVO, MARGEN, RES) %>%
  arrange(desc(MAHALANOBIS))



## ---- p-multi-outliers-tabla -------------------------------------------------

casos_out2 %>%
  kable(caption     = paste0("Casos outliers según Mahalanobis (",
                             nrow(casos_out2),
                             ") en: RENECO, ACTIVO, MARGEN, RES"),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width        = FALSE,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = TRUE,  align = "c") %>%
  row_spec(1:nrow(casos_out2), bold = FALSE, align = "c")


## ---- p-multi-outliers-eliminar ----------------------------------------------

# Eliminar los outliers multivariantes y descartar la columna auxiliar, que ya
# ha cumplido su función.
muestra2_so <- muestra2_mah %>%
  filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
         MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS)) %>%
  select(-MAHALANOBIS)

rm(muestra2_mah)

# "muestra2_so" conserva 91 empresas y las mismas 27 variables que "muestra2".


## ---- p-multi-correlaciones --------------------------------------------------

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


## ---- p-multi-correlaciones-comparacion --------------------------------------

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


## ---- p-limpieza-final -------------------------------------------------------

# Esta versión ha necesitado objetos auxiliares que en "previo_rstars.R" quedan
# dentro de las funciones del paquete. Los eliminamos para que ambos scripts
# terminen con exactamente el mismo Global Environment.
rm(casos_na, casos_na2, casos_out, casos_out2, Q1, Q3, Q1M, Q3M)


# Fin de script :)
