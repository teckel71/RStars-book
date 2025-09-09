# COMPONENTES PRINCIPALES. Disculpad la falta de tildes!

rm(list = ls())

# DATOS

library(readxl)
datos <- read_excel("Datos_ejercicio_01_tema_04.xlsx", sheet = "HURTADO JAVIER", na = "n.d.")
datos <- data.frame(datos, row.names = 1)
summary (datos)

# Identificando y eliminando outliers.

library (dplyr)
datos <- datos %>%
  mutate (MAHALANOBIS = mahalanobis(.,
            center = colMeans(.),
            cov = cov(.)))

library (ggplot2)
ggplot(data = datos, map = (aes(y = MAHALANOBIS))) +
  geom_boxplot(fill = "orange") +
  ggtitle("DISTANCIA DE MAHALANOBIS", subtitle = "Empresas eólicas") +
  ylab("MAHALANOBIS")

Q1M <- quantile (datos$MAHALANOBIS, c(0.25))
Q3M <- quantile (datos$MAHALANOBIS, c(0.75))
datos %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
         MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS))%>%
         select(MAHALANOBIS)
datos <- datos %>% select(-MAHALANOBIS)

zdatos <- data.frame(scale(datos))
summary (zdatos)

d <- dist(zdatos)
library (factoextra)
fviz_dist(d, lab_size = 8)

# Método de Ward.

cluster_j<-hclust(d, method="ward.D2")
fviz_dend(cluster_j,
          cex = 0.6,
          rect = FALSE,
          labels_track_height = 5.5) +
  labs(title = "Casos Mini-Ejercicio",
       subtitle = "Método de Ward. Variables tipificadas.") +
  theme_grey()

fviz_dend(cluster_j,
          cex = 0.6,
          k = 4, # número de grupos o conglomerados que se ha decidido formar!
          k_colors = "black",
          labels_track_height = 5.5,
          rect = TRUE,
          rect_border = "npg",
          rect_fill = TRUE) +
  labs(title = "Empresas eólicas",
       subtitle = "Método de Ward. Variables originales tipificadas.") +
  theme_grey()

# CARACTERIZACIÓN Y COMPOSICIÓN DE GRUPOS.

datos$whatcluster_j <- as.factor(cutree(cluster_j, k=4))
levels(datos$whatcluster_j)

# Tabla con centroides de grupos.

tablamedias <- datos %>%
  group_by(whatcluster_j) %>% summarise(obs = length(whatcluster_j),
                                        MVariable_1 = round(mean(Variable_1),4),
                                        MVariable_2 = round(mean(Variable_2),4),
                                        MVariable_3 = round(mean(Variable_3),4),
                                        MVariable_4 = round(mean(Variable_2),4))

library (knitr)
library (kableExtra)
knitr.table.format = "html"

tablamedias %>%
  kable(format = knitr.table.format,
        caption = "Método de Ward. 4 grupos. Medias de variables",
        col.names = c("Clúster", "Observaciones", "Variable 1", "Variable 2",
                      "Variable 3", "Variable 4")) %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped", "bordered", "condensed",
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:nrow(tablamedias), bold= F, align = "c")
