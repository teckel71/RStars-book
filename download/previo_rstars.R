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
muestra<- select(interestelar_100, everything())

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

## Localizando outliers
ggplot(data = muestra, map = (aes(y = RENECO))) +
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

## Descriptivos básicos

# Gráficos básicos

g1 <-
ggplot(data = muestra_so, map = aes(x = RENECO)) +
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
  ggplot(data = muestra_so, map = aes(x = RENECO)) +
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
        digits    = c(2, 2, 2, 2, 2, 2, 2),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
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
muestra2<- select(interestelar_100, everything())

# Localizando missing values.
muestra2 %>%
  select (RENECO, ACTIVO, MARGEN, RES) %>%
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

muestra2 %>% filter(is.na(RENECO) |
                      is.na(ACTIVO) |
                      is.na(MARGEN) |
                      is.na(RES))%>%
             select(RENECO, ACTIVO, MARGEN, RES)
muestra2 <- muestra2 %>%
            filter(! is.na(RENECO) &
                     ! is.na(ACTIVO) &
                     ! is.na(MARGEN) &
                     ! is.na(RES))

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

ggplot(data = muestra2, map = (aes(y = MAHALANOBIS))) +
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

