# COMPONENTES PRINCIPALES. Disculpad la falta de tildes!

rm(list = ls())

# DATOS

library(readxl)
datos <- read_excel("eolica_60.xlsx", sheet = "Datos", na = "n.d.")
datos <- data.frame(datos, row.names = 1)

# Seleccionando variables metricas para el analisis.

library(dplyr)
muestra <- datos %>% select(RES, FPIOS, MARGEN, SOLVENCIA)
summary (muestra)

# Identificando missing values.

library(visdat)
vis_miss(muestra)
muestra %>% filter(is.na(RES) | is.na(FPIOS) | is.na(MARGEN) | is.na(SOLVENCIA)) %>%
               select(RES, FPIOS, MARGEN, SOLVENCIA)  
muestra <- muestra %>%
  filter(! is.na(RES) & ! is.na(FPIOS) & ! is.na(MARGEN) & ! is.na(SOLVENCIA))  

# Identificando y eliminando outliers.

muestra <- muestra %>%
  mutate (MAHALANOBIS = mahalanobis(cbind(RES, FPIOS, MARGEN, SOLVENCIA),
            center = colMeans(select(., RES, FPIOS, MARGEN, SOLVENCIA)),
            cov = cov(select(., RES, FPIOS, MARGEN, SOLVENCIA))))

library (ggplot2)
ggplot(data = muestra, map = (aes(y = MAHALANOBIS))) +
  geom_boxplot(fill = "orange") +
  ggtitle("DISTANCIA DE MAHALANOBIS", subtitle = "Empresas eólicas") +
  ylab("MAHALANOBIS")

Q1M <- quantile (muestra$MAHALANOBIS, c(0.25))
Q3M <- quantile (muestra$MAHALANOBIS, c(0.75))
muestra %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
         MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS))%>%
         select(MAHALANOBIS)
muestra_so <- muestra %>%
  filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
         MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS))
muestra_so <- muestra_so %>% select(-MAHALANOBIS)

# Correlaciones.

library (GGally)

corr_plot_so <- ggpairs(muestra_so, 
                        lower = list(continuous = wrap("cor",
                                                       size = 4.5,
                                                       method = "pearson",
                                                       stars = TRUE)),
                        title = "Matriz de Correlación sin outliers")
corr_plot_so

# Obtencion de componentes.

componentes <- prcomp (muestra_so, scale=T)
temporal <- summary (componentes)
temporal

# Convertir el resumen en un data frame

summary_df <- as.data.frame(temporal$importance)
summary_df <- t(summary_df)  # Transponer para mejor visualización
rm (temporal)

# Crear la tabla con kable y personalizarla con kableExtra

library (knitr)
library (kableExtra)
knitr.table.format = "html"

summary_df %>%
kable(caption = "Resumen de Componentes",
      col.names = c("Desviación típica","Proporción de varianza (comunalidad)",
                    "Proporción de varianza (comunalidad) acumulada"),
      format.args = list(decimal.mark = ".", digits = 4)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "condensed"),
                full_width = F, 
                position = "center") %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(summary_df)), bold= F, align = "c") %>%
  column_spec(1, bold = TRUE, extra_css = "text-align: left;")

# Cargas de cada componente.

cargas <- componentes$rotation
cargas %>%
  kable(caption = "Cargas de las componentes obtenidas",
        format.args = list(decimal.mark = ".", digits = 4))  %>%
  kable_styling(full_width = F, bootstrap_options = "striped",
                "bordered", "condensed",
                position = "center") %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(cargas)), bold= F, align = "c") %>%
  column_spec(1, bold = TRUE, extra_css = "text-align: left;")

# Determinacion Componentes a retener.

# Criterio del Autovalor mayor que 1.

orden <- c(1:ncol(muestra_so))
autovalor <- componentes$sdev^2
autovalores <- data.frame(orden, autovalor)

autograph <- ggplot(data = autovalores, map = (aes(x = orden, y = autovalor))) +
             geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
             scale_x_continuous(breaks=c(1:nrow(autovalores)))+
             geom_hline(yintercept = 1, colour = "dark blue") +
             geom_text(aes(label = round(autovalor,2)), vjust = 1, colour = "dark blue", size = 3) +
             ggtitle("AUTOVALORES DE LAS COMPONENTES", subtitle = "Empresas eólicas") +
             xlab ("Número de componente") +
             ylab("Autovalor")

autograph

# Determinar si cada autovalor es mayor o igual a 1

autovalores <- autovalores %>%
  mutate(variacum = 100*(cumsum((autovalor/nrow(autovalores)))))
checkcp <- ifelse(autovalores$autovalor >= 1, "CP", "NCP")
checkcp
             
vacumgraph <- ggplot(data = autovalores, map = (aes(x = orden,
                                                    y = variacum,
                                                    fill = checkcp))) +
              geom_bar(stat = "identity", colour = "red", alpha = 0.7) +
              scale_x_continuous(breaks=c(1:nrow(autovalores)))+
              geom_text(aes(label = round(variacum,2)), vjust = 1,
                        colour = "dark blue", size = 3) +
              ggtitle("COMUNALIDAD ACUMULADA POR COMPONENTES",
                      subtitle = "Empresas eólicas") +
              xlab ("Número de componente") +
              ylab("Varianza acumulada")
vacumgraph

library (patchwork)
autograph / vacumgraph

# Scores.

scores <- componentes$x[,1] #tantas columnas como componentes retenidas
scores_df <- as.data.frame(scores)
scores_df <- cbind(scores_df,muestra_so)
scores_df %>%
  arrange(desc(scores)) %>%
  kable(caption = "Puntuaciones de las componentes obtenidas",
        col.names = c("Empresa", "Puntuación", "Resultado", "F. Propios",
                      "Margen", "Solvencia"),
        format.args = list(decimal.mark = ".", digits = 4))  %>%
  kable_styling(full_width = F, bootstrap_options = "striped",
                "bordered", "condensed",
                position = "center", font_size = 12) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(scores_df)), bold= F, align = "c") %>%
  column_spec(1, bold = TRUE, extra_css = "text-align: left;")

# Fin del script :)