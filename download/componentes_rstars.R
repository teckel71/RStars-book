### Análisis de Componentes Principales ###

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(gtExtras)
library (GGally)
library (knitr)
library (kableExtra)
library (patchwork)

## DATOS

# Importando datos desde Excel
interestelar_100 <- read_excel("interestelar_100.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_100 <- data.frame(interestelar_100, row.names = 1)

# Seleccionando variables metricas para el analisis.
seleccion <- interestelar_100 %>%
  select(IDIVERSE, IFIDE, IDIG)
seleccion_df_graph <- gt_plt_summary(seleccion)
seleccion_df_graph

# Localizando missing values.
seleccion %>%
  vis_miss() +
  labs(title = "Indicadores: Diversificación, Fidelidad, Digitalización",
       subtitle = "Transporte de mercancías interestelar",
       y = "Observación",
       fill = NULL) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "grey"),
    labels = c("TRUE" = "NA", "FALSE" = "Presente")) +
  theme(
    plot.title = element_text(face = "bold", size = 14))

seleccion %>% filter(is.na(IDIVERSE) |
                     is.na(IFIDE) |
                     is.na(IDIG)) %>%
              select(IDIVERSE, IFIDE, IDIG)
seleccion <- seleccion %>%
             filter(! is.na(IDIVERSE) &
                    ! is.na(IFIDE) &
                    ! is.na(IDIG))

# Identificando y descartando outliers con distancia de Mahalanobis.
seleccion <- seleccion %>%
  mutate(MAHALANOBIS = mahalanobis(as.matrix(.),
                                   center = colMeans(.),
                                   cov    = cov(.)))

ggplot(data = seleccion, map = (aes(y = MAHALANOBIS))) +
  geom_boxplot(fill = "orange") +
  ggtitle("DISTANCIA DE MAHALANOBIS",
          subtitle = "IDIVERSE, IFIDE, IDIG. Empresas TMI.") +
  ylab("MAHALANOBIS")

Q1M <- quantile (seleccion$MAHALANOBIS, c(0.25))
Q3M <- quantile (seleccion$MAHALANOBIS, c(0.75))

seleccion %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
           MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS))%>%
  select(MAHALANOBIS, IDIVERSE, IFIDE, IDIG) 

# Creando nuevo df sin outliers.
seleccion_so <- seleccion %>%
  filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
           MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS))  

# Eliminando variable MAHALANOBIS de los df
seleccion    <- seleccion    %>% select(-MAHALANOBIS)
seleccion_so <- seleccion_so %>% select(-MAHALANOBIS)

## COMPONENTES

# Correlaciones.
corr_plot_so <- ggpairs(seleccion_so, 
                        lower = list(continuous = wrap("cor",
                                                       size = 4.5,
                                                       method = "pearson",
                                                       stars = TRUE)),
                        title = "Matriz de Correlación sin outliers")
corr_plot_so

# Obtencion de componentes.
componentes <- prcomp (seleccion_so, scale=T)
temporal <- summary (componentes)
temporal

# Convertir el resumen en un data frame
summary_df <- as.data.frame(temporal$importance)
summary_df <- t(summary_df)  # Transponer para mejor visualización
rm (temporal)

# Crear la tabla con kable y personalizarla con kableExtra
summary_df %>%
  kable(caption = "Resumen de Componentes",
        col.names = c("Componente", 
                      "Desv. típica",
                      "Proporción de varianza (comunalidad)",
                      "Proporción de varianza (comunalidad) acumulada"),
        digits = c(2, 2, 2),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered", "condensed"),
                full_width = F, 
                position = "center") %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(summary_df)), bold= F, align = "c") %>%
  column_spec(1, bold = TRUE, extra_css = "text-align: center;")

# Cargas de cada componente.
cargas <- componentes$rotation
cargas %>%
  kable(caption = "Cargas de las componentes obtenidas",
        digits = c(3, 3, 3),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position = "center") %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(cargas)), bold= F, align = "c") %>%
  column_spec(1, bold = TRUE, extra_css = "text-align: left;")

# Determinacion Componentes a retener.
# Criterio del Autovalor mayor que 1.
orden <- c(1:ncol(seleccion_so))
autovalor <- componentes$sdev^2
autovalores <- data.frame(orden, autovalor)

autograph <- ggplot(data = autovalores, map = (aes(x = orden,
                                                   y = autovalor))) +
             geom_bar(stat = "identity",
                      colour = "red",
                      fill = "orange",
                      alpha = 0.7) +
             scale_x_continuous(breaks=c(1:nrow(autovalores)))+
             geom_hline(yintercept = 1,
                        colour = "dark blue") +
             geom_text(aes(label = round(autovalor,2)),
                       vjust = 1,
                       colour = "dark blue",
                       size = 3) +
             ggtitle("AUTOVALORES DE LAS COMPONENTES",
                     subtitle = "Empresas TMI") +
             xlab ("Número de componente") +
             ylab("Autovalor")

autograph

# Comunalidad acumulada.
autovalores <- autovalores %>%
  mutate(variacum = 100*(cumsum((autovalor/nrow(autovalores)))))
checkcp <- ifelse(autovalores$autovalor >= 1, "CP", "NCP")
checkcp
             
vacumgraph <- ggplot(data = autovalores, map = (aes(x = orden,
                                                    y = variacum))) +
              geom_bar(stat = "identity",
                       aes(fill = checkcp),
                       colour = "red",
                       alpha = 0.7) +
              scale_x_continuous(breaks=c(1:nrow(autovalores)))+
              geom_text(aes(label = round(variacum,2)),
                        vjust = 1,
                        colour = "dark blue",
                        size = 3) +
              ggtitle("COMUNALIDAD ACUMULADA POR COMPONENTES",
                      subtitle = "Empresas TMI") +
              xlab ("Número de componente") +
              ylab("Varianza acumulada")
vacumgraph

combinado <- autograph / vacumgraph
combinado <- combinado + 
  plot_annotation(
    title = "Retención de componentes (Autovalor >1)",
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
combinado

## Puntuaciones o Scores
scores <- componentes$x[,1]  #tantas columnas como componentes retenidas
scores_df <- as.data.frame(scores)
scores_df <- cbind(scores_df,seleccion_so)
scores_top10 <- scores_df %>%
  arrange(desc(scores)) %>%
  slice(1:10)

scores_top10 %>%
  kable(caption = "Puntuaciones emporesas TMI (Top-10, sin outliers)",
        col.names = c("Empresa",
                      "Puntuación",
                      "I. Diversif.",
                      "I. Fidelizac.",
                      "I. Digitalizac."),
        digits = c(3, 3, 3, 3),
        format.args = list(decimal.mark = ".",
                           scientific = FALSE)) %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped",
                "bordered",
                "condensed",
                position = "center",
                font_size = 12) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(scores_top10)),
           bold= F,
           align = "c") %>%
  column_spec(1, bold = TRUE,
              extra_css = "text-align: left;")

# Scores puntuando outliers
scores_all <- predict(componentes, newdata = seleccion)
scores_all <- scores_all[,1]
scores_all_df <- as.data.frame(scores_all)
scores_all_df <- cbind(scores_all_df,seleccion)
scores_all_top10 <- scores_all_df %>%
  arrange(desc(scores_all)) %>%
  slice(1:10)

scores_all_top10 %>%
  kable(caption = "Puntuaciones emporesas TMI (Top-10, con outliers)",
        col.names = c("Empresa",
                      "Puntuación",
                      "I. Diversif.",
                      "I. Fidelizac.",
                      "I. Digitalizac."),
        digits = c(3, 3, 3, 3),
        format.args = list(decimal.mark = ".",
                           scientific = FALSE)) %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped",
                "bordered",
                "condensed",
                position = "center",
                font_size = 12) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(scores_all_top10)),
           bold= F,
           align = "c") %>%
  column_spec(1, bold = TRUE,
              extra_css = "text-align: left;")

# Fin del script :)