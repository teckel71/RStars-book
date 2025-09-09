# Explorando a las empresas eolicas graficamente (disculpad la falta de tildes)

rm(list = ls())

# DATOS

library(readxl)
eolica_100 <- read_excel("eolica_100.xlsx", sheet = "Datos")
summary (eolica_100)
eolica_100 <- data.frame(eolica_100, row.names = 1)
summary (eolica_100)

# Cargando ggplot2

library (ggplot2)

# Histograma

ggplot(data = eolica_100, map = aes(x = RENECO)) +
  geom_histogram()

ggplot(data = eolica_100, map = aes(x = RENECO)) +
       geom_histogram(bins = 40)

ggplot(data = eolica_100, map = aes(x = RENECO)) +
  geom_histogram(bins = 40, colour = "red", fill = "orange") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Frecuencias")

ggplot(data = eolica_100, map = aes(x = RENECO, fill = DIMENSION)) +
  geom_histogram(bins = 60, colour = "red") +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Frecuencias")

# Grafico de densidad

ggplot(data = eolica_100, map = aes(x = RENECO)) +
  geom_density(colour = "red", fill = "orange") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Densidad")

ggplot(data = eolica_100, map = aes(x = RENECO, fill = DIMENSION)) +
  geom_density(colour = "red", alpha = 0.70, ) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas")+
  xlab("Rentabilidad Económica (%)") +
  ylab("Densidad")
       
# Gráfico de caja (box-plot)

ggplot(data = eolica_100, map = (aes(y = RENECO))) +
  geom_boxplot(fill= "orange") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas") +
  ylab("Rentabilidad Económica (%)")

ggplot(data = eolica_100, map = (aes(x = "", y = RENECO))) +
  geom_boxplot(fill = "orange") +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas") +
  ylab("Rentabilidad Económica (%)")

ggplot(data = eolica_100, map = (aes(x = DIMENSION, y = RENECO, fill = DIMENSION))) +
  geom_boxplot() +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  stat_summary(fun = "mean",
               geom = "line",
               col = "darkblue",
               map = (aes(group = TRUE))) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas") +
  ylab("Rentabilidad Económica (%)")

ggplot(data = eolica_100, map = (aes(x = DIMENSION, y = RENECO, fill = DIMENSION))) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  stat_summary(fun = "mean",
               geom = "line",
               col = "darkblue",
               map = (aes(group = TRUE))) +
  geom_jitter(width = 0.1,
              size = 1,
              col = "darkred",
              alpha = 0.40) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "100 empresas eólicas") +
  ylab("Rentabilidad Económica (%)")

# Gráfico de dispersión o scatterplot

ggplot(data = eolica_100, map = (aes(x = RENECO, y = RENFIN))) +
  geom_point(color = "red", size = 2, alpha = 0.7) +
  ggtitle("RENTABILIDAD ECONÓMICA vs RENTABILIDAD FINANCIERA", subtitle = "100 empresas eólicas") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Rentabilidad Financiera (%)")

ggplot(data = eolica_100, map = (aes(x = RENECO,
                                     y = RENFIN,
                                     col = DIMENSION))) +
  geom_point(size = 2, alpha = 0.7) +
  ggtitle("RENTABILIDAD ECONÓMICA vs FINANCIERA",
          subtitle = "100 empresas eólicas") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Rentabilidad Financiera (%)")

library ("ggExtra")
scatter_plus <- ggplot(data = eolica_100, map = (aes(x = RENECO,
                                                     y = RENFIN,
                                                     col = DIMENSION))) +
  geom_point(size = 2, alpha = 0.7) +
  ggtitle("RENTABILIDAD ECONÓMICA vs FINANCIERA",
          subtitle = "100 empresas eólicas") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Rentabilidad Financiera (%)")
ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

scatter_plus <- ggplot(data = eolica_100, map = (aes(x = RENECO,
                                                     y = RENFIN,
                                                     col = DIMENSION,
                                                     size = SOLVENCIA))) +
  geom_point(alpha = 0.7) +
  ggtitle("RENTABILIDAD ECONÓMICA vs FINANCIERA",
          subtitle = "100 empresas eólicas") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Rentabilidad Financiera (%)")
ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

scatter_plus <- ggplot(data = eolica_100, map = (aes(x = RENECO,
                                                     y = RENFIN,
                                                     col = DIMENSION,
                                                     size = SOLVENCIA))) +
  geom_point(alpha = 0.7) +
  geom_text(aes(label=row.names(eolica_100)), size=2, color="black", alpha = 0.7) +
  ggtitle("RENTABILIDAD ECONÓMICA vs FINANCIERA",
          subtitle = "100 empresas eólicas") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Rentabilidad Financiera (%)")
ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

library(ggrepel)
scatter_plus <- ggplot(data = eolica_100, map = (aes(x = RENECO,
                                                     y = RENFIN,
                                                     col = DIMENSION,
                                                     size = SOLVENCIA))) +
  geom_point(alpha = 0.7) +
  geom_label_repel(aes(label = row.names(eolica_100)),
                   size = 2,
                   color = "black",
                   alpha = 0.5) +
  ggtitle("RENTABILIDAD ECONÓMICA vs FINANCIERA",
          subtitle = "100 empresas eólicas") +
  xlab("Rentabilidad Económica (%)") +
  ylab("Rentabilidad Financiera (%)")
ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T,
           position = "identity", alpha = 0.5)

# Fin de script :)

