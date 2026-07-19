## Generando gráficos con {ggplot2}

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(gtExtras)
library(ggExtra)
library(ggrepel)

# Importando datos desde Excel
interestelar_100 <- read_excel("interestelar_100.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_100 <- data.frame(interestelar_100, row.names = 1)

# visualizando el data frame de modo elegante con {gtExtras}
datos_df_graph <- gt_plt_summary(interestelar_100)
datos_df_graph

# Histograma
ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_histogram()

ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_histogram(bins = 20)

nbins <- nclass.Sturges(interestelar_100$LIQUIDEZ[is.finite(interestelar_100$LIQUIDEZ)])
ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins)

ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins, colour = "red", fill = "orange") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Frecuencias")

ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins, colour = "red", aes(fill = EFLO)) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Frecuencias")

# Gráfico de densidad
ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_density(colour = "red", fill = "orange") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Densidad")

ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_density(aes(fill = EFLO), colour = "red", alpha = 0.70) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Densidad")

# Gráfico de caja (box-plot)
ggplot(data = interestelar_100, aes(x = "", y = LIQUIDEZ)) +
  geom_boxplot(fill= "orange") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Ratio de liquidez")

ggplot(data = interestelar_100, aes(x = "", y = RES)) +
  geom_boxplot(fill= "orange") +
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

ggplot(data = interestelar_100, aes(x = "", y = RES)) +
  geom_boxplot(fill = "orange") +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

ggplot(data = interestelar_100, aes(x = EFLO, y = RES)) +
  geom_boxplot(aes(fill = EFLO)) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  stat_summary(fun = "mean",
               geom = "line",
               col = "darkblue",
               aes(group = TRUE)) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

ggplot(data = interestelar_100, aes(x = EFLO, y = RES)) +
  geom_boxplot(aes(fill = EFLO), outlier.shape = NA) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  stat_summary(fun = "mean",
               geom = "line",
               col = "darkblue",
               aes(group = TRUE)) +
  geom_jitter(width = 0.1,
              size = 1,
              col = "darkred",
              alpha = 0.40) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

# Gráfico de violín
ggplot(data = interestelar_100, aes(x = EFLO, y = LIQUIDEZ)) +
  geom_violin(aes(fill = EFLO), alpha = 0.7) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.8) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 2,
               col = "darkblue") +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RATIO DE LIQUIDEZ POR TIPO DE FLOTA",
          subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Ratio de liquidez")

# Gráfico de barras
ggplot(data = interestelar_100, aes(x = EFLO)) +
  geom_bar(aes(fill = EFLO)) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("NÚMERO DE EMPRESAS POR TIPO DE FLOTA",
          subtitle = "Transporte Interestelar") +
  xlab("Antigüedad de la flota") +
  ylab("Número de empresas")

res_por_galaxia <- interestelar_100 %>%
  group_by(GALAXIA) %>%
  summarise(RES_medio = mean(RES, na.rm = TRUE)) %>%
  arrange(desc(RES_medio))

ggplot(data = res_por_galaxia, aes(x = reorder(GALAXIA, RES_medio),
                                    y = RES_medio)) +
  geom_col(fill = "orange", colour = "red") +
  coord_flip() +
  ggtitle("RESULTADO MEDIO POR GALAXIA",
          subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado medio (miles de PAVOs)")

# Gráfico de dispersión o scatterplot
ggplot(data = interestelar_100, aes(x = LIQUIDEZ, y = RES)) +
  geom_point(color = "red", size = 2, alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggplot(data = interestelar_100, aes(x = LIQUIDEZ, y = RES)) +
  geom_point(aes(col = EFLO), size = 2, alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

scatter_plus <- ggplot(data = interestelar_100,
                       aes(x = LIQUIDEZ, y = RES)) +
  geom_point(aes(col = EFLO), size = 2, alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

scatter_plus <- ggplot(data = interestelar_100,
                       aes(x = LIQUIDEZ, y = RES)) +
  geom_point(aes(col = EFLO, size = SOLVENCIA), alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

scatter_plus <- ggplot(data = interestelar_100,
                       aes(x = LIQUIDEZ, y = RES)) +
  geom_point(aes(col = EFLO, size = SOLVENCIA), alpha = 0.7) +
  geom_label_repel(aes(label = row.names(interestelar_100)),
                   size = 2,
                   color = "black",
                   alpha = 0.5,
                   max.overlaps = 20,
                   box.padding = 0.1,
                   point.padding = 0.05,
                   force = 3,
                   max.time = 2,
                   seed = 123) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

# Paneles múltiples (faceting)
ggplot(data = interestelar_100, aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins, fill = "orange", colour = "red") +
  facet_wrap(~ EFLO) +
  ggtitle("RATIO DE LIQUIDEZ POR TIPO DE FLOTA",
          subtitle = "Transporte Interestelar") +
  xlab("Liquidez (ratio)") +
  ylab("Frecuencias")

ggplot(data = interestelar_100, aes(x = LIQUIDEZ, y = RES)) +
  geom_point(aes(col = EFLO), size = 2, alpha = 0.7) +
  facet_wrap(~ EFLO) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

# Fin del script :)
