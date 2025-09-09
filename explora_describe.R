# Descibiendo a las empresas eolicas (disculpad la falta de tildes)

rm(list = ls())

# DATOS

library(readxl)
eolica_100 <- read_excel("eolica_100_mv.xlsx", sheet = "Datos",
                         na = c("n.d.", "s.d."))
summary (eolica_100)
eolica_100 <- data.frame(eolica_100, row.names = 1)
summary (eolica_100)

# Analisis de una variable.

## Localizando missing values.

library(visdat)
vis_miss(eolica_100) 

library (dplyr)
muestra<- select(eolica_100, everything())
muestra %>% filter(is.na(RENECO)) %>% select(RENECO)
muestra <- muestra %>% filter(! is.na(RENECO))

## Localizando outliers.

library (ggplot2)
ggplot(data = muestra, map = (aes(y = RENECO))) +
    geom_boxplot(fill = "orange") +
    ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "Empresas eólicas") +
    ylab("Rentabilidad Económica (%)")

Q1 <- quantile (muestra$RENECO, c(0.25))
Q3 <- quantile (muestra$RENECO, c(0.75))
Q1; Q3

muestra %>% filter(RENECO > Q3 + 1.5*IQR(RENECO) | RENECO < Q1 - 1.5*IQR(RENECO)) %>%
            select(RENECO) 

muestra_so <- muestra %>%
              filter(RENECO <= Q3 + 1.5*IQR(RENECO) & RENECO >= Q1 - 1.5*IQR(RENECO))

## Tabla de datos (distribución de frecuencias agrupadas en intervalos)

library (knitr)
library (kableExtra)
muestra_so <- muestra_so %>% arrange(RENECO, row.names(muestra_so))

# Crear los intervalos
muestra_so$intervalos <- cut(muestra_so$RENECO, breaks = 5, include.lowest = TRUE)

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
  kable(caption = "Distribución de frecuencias agrupadas en intervalos de la Rentabilidad Económica",
        col.names = c("Intervalo rentabilidad", "Frecuencia absoluta n(i)",
                      "Frecuencia absoluta acum. N(i)", "Frecuencia relativa f(i)",
                      "Frecuencia relativa acum. F(i)"),
        format.args = list(decimal.mark = ".", digits = 2)) %>%
  kable_styling(full_width = F, bootstrap_options = "striped",
                "bordered", "condensed",
                position = "center", font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(conteo_intervalos_df)), bold= F, align = "c")

## Descriptivos básicos

# Gráficos básicos

g1 <-
ggplot(data = muestra_so, map = aes(x = RENECO)) +
  geom_histogram(bins = 40,
                 colour = "red",
                 fill = "orange",
                 alpha = 0.7) +
  geom_vline(xintercept = mean(muestra_so$RENECO),
             color = "dark blue",
             size = 1.2,
             alpha = 0.8) +
  ggtitle("Histograma")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Frecuencias")

g1

g2 <-
ggplot(data = muestra_so, map = aes(x = RENECO)) +
  geom_density(colour = "red",
               fill = "orange",
               alpha = 0.7) +
  geom_vline(xintercept = mean(muestra_so$RENECO),
             color = "dark blue",
             size = 0.8,
             alpha = 0.8) +
  stat_function(fun = dnorm, args = list(mean = mean(muestra_so$RENECO),
                                         sd = sd(muestra_so$RENECO)),
                                         geom = "area", color = "darkblue", 
                                         fill = "yellow", alpha = 0.2) +
  ggtitle("Gráfico de densidad vs curva normal")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Densidad")

g2

g3 <-
ggplot(data = muestra_so, map = (aes(x = "", y = RENECO))) +
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

library (moments) # paquete necesario para calcular la curtosis.

estadisticos <- muestra_so %>% summarise( Media = mean(RENECO),
                                          DT = sd(RENECO),
                                          Mínimo = min(RENECO),
                                          Mediana = median(RENECO),
                                          Maximo = max(RENECO),
                                          Asimetria = skewness(RENECO),
                                          Curtosis = kurtosis(RENECO) - 3)

# Mostrar estadisticos

estadisticos %>%
  kable(caption = "Principales Estadísticos de la Rentabilidad Económica",
        col.names = c("Media", "Mediana",
                      "Desviación Típica", "Valor mínimo",
                      "Valor Máximo", "C. Asimetría Fisher",
                      "C. Curtosis Fisher"),
        format.args = list(decimal.mark = ".", digits = 2)) %>%
  kable_styling(full_width = F, bootstrap_options = "striped",
                "bordered", "condensed",
                position = "center", font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(estadisticos)), bold= F, align = "c")

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

library (patchwork)
resumen <- (g1 | g2)/(g3 | g4)
resumen <- resumen + 
  plot_annotation(
    title = "Rentabilidad Económica",
    subtitle = "Empresas eólicas (sin outliers)")
resumen

## Trabajando con multiples variables.

# Localizando y descartando casos con missing values.

vis_miss(eolica_100)

muestra2<- select(eolica_100, everything())
muestra2 %>% filter(is.na(RENECO) | is.na(ACTIVO) | is.na(MARGEN) | is.na(RES))%>%
             select(RENECO, ACTIVO, MARGEN, RES)
muestra2 <- muestra2 %>%
            filter(! is.na(RENECO) & ! is.na(ACTIVO) & ! is.na(MARGEN) & ! is.na(RES))

# Identificando y descartando outliers con distancia de Mahalanobis.

muestra2 <- muestra2 %>%
            mutate (MAHALANOBIS = mahalanobis(cbind(RENECO, ACTIVO, MARGEN, RES),
                      center = colMeans(select(., RENECO, ACTIVO, MARGEN, RES)),
                      cov = cov(select(., RENECO, ACTIVO, MARGEN, RES))))

ggplot(data = muestra2, map = (aes(y = MAHALANOBIS))) +
    geom_boxplot(fill = "orange") +
    ggtitle("DISTANCIA DE MAHALANOBIS",
            subtitle = "RENECO, ACTIVO, MARGEN, RES. Empresas eólicas ") +
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

## Correlaciones entre variables.
library (GGally)

temporal <- muestra2_so %>% select(RENECO, ACTIVO, MARGEN, RES)
corr_plot_so <- ggpairs(temporal, 
                        lower = list(continuous = wrap("cor",
                                                       size = 4.5,
                                                       method = "pearson",
                                                       stars = TRUE)),
                        title = "Matriz de Correlación sin outliers")
rm(temporal)
corr_plot_so

temporal <- muestra2 %>% select(RENECO, ACTIVO, MARGEN, RES)
corr_plot_co <- ggpairs(temporal, 
                        lower = list(continuous = wrap("cor",
                                                       size = 4.5,
                                                       method = "pearson",
                                                       stars = TRUE)),
                        title = "Matriz de Correlación con outliers")
rm(temporal)
corr_plot_co

corr_plot_so_gg <- ggmatrix_gtable(corr_plot_so)
corr_plot_co_gg <- ggmatrix_gtable(corr_plot_co)

library (gridExtra)

grid.arrange(corr_plot_so_gg, corr_plot_co_gg, ncol = 2, top = "CORRELACIONES")

# Fin de script :)

