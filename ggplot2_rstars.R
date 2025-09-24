## Generando gráficos con {ggplot2}

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library (ggplot2)
library(gtExtras)
library (ggExtra)
library (ggrepel)

# Importando datos desde Excel
interestelar_100 <- read_excel("interestelar_100.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_100 <- data.frame(interestelar_100, row.names = 1)

# visualizando el data frame de modo elegante con {gtExtras}
datos_df_graph <- gt_plt_summary(interestelar_100)
datos_df_graph

# Histograma
ggplot(data = interestelar_100, map = aes(x = LIQUIDEZ)) +
  geom_histogram()

ggplot(data = interestelar_100) +     # Mismo gráfico, con aes en geom y sin map
  geom_histogram( aes(x = LIQUIDEZ))


ggplot(data = interestelar_100, map = aes(x = RENECO)) +
       geom_histogram(bins = 20)      # cambiando número de bins

nbins <- nclass.Sturges(interestelar_100$LIQUIDEZ[is.finite(interestelar_100$LIQUIDEZ)])
ggplot(data = interestelar_100, map = aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins)
ggplot(data = interestelar_100, map = aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins)        # número de bins de Sturges

ggplot(data = interestelar_100, map = aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins, colour = "red", fill = "orange") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Frecuencias")

ggplot(data = interestelar_100, map = aes(x = LIQUIDEZ)) +
  geom_histogram(bins = nbins, colour = "red", aes(fill = EFLO)) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Frecuencias")

# Gráfico de densidad
ggplot(data = interestelar_100, map = aes(x = LIQUIDEZ)) +
  geom_density(colour = "red", fill = "orange") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Densidad")

ggplot(data = interestelar_100, map = aes(x = LIQUIDEZ)) +
  geom_density(aes(fill = EFLO), colour = "red", alpha = 0.70, ) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar")+
  xlab("Liquidez (ratio)") +
  ylab("Densidad")
       
# Gráfico de caja (box-plot)
ggplot(data = interestelar_100, map = (aes(x = "", y = LIQUIDEZ))) +
  geom_boxplot(fill= "orange") +
  ggtitle("RATIO DE LIQUIDEZ", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Ratio de liquidez")

ggplot(data = interestelar_100, map = (aes(x = "", y = RES))) +
  geom_boxplot(fill= "orange") +
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

ggplot(data = interestelar_100, map = (aes(x = "", y = RES))) +
  geom_boxplot(fill = "orange") +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

ggplot(data = interestelar_100, map = (aes(x = EFLO, y = RES))) +
  geom_boxplot(aes(fill = EFLO)) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               col = "darkblue") +
  stat_summary(fun = "mean",
               geom = "line",
               col = "darkblue",
               map = (aes(group = TRUE))) +
  scale_fill_brewer(palette = "Oranges") +
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

ggplot(data = interestelar_100, map = (aes(x = EFLO, y = RES))) +
  geom_boxplot(aes(fill = EFLO), outlier.shape = NA) +
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
  ggtitle("RESULTADO DEL EJERCICIO", subtitle = "Transporte Interestelar") +
  xlab("") +
  ylab("Resultado (miles de PAVOs)")

# Gráfico de dispersión o scatterplot
ggplot(data = interestelar_100, map = (aes(x = LIQUIDEZ, y = RES))) +
  geom_point(color = "red", size = 2, alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggplot(data = interestelar_100, map = (aes(x = LIQUIDEZ, y = RES))) +
  geom_point(aes(col = EFLO), size = 2, alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOS)")


scatter_plus <- ggplot(data = interestelar_100,
                       map = (aes(x = LIQUIDEZ, y = RES))) +
  geom_point(aes(col = EFLO), size = 2, alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

scatter_plus <- ggplot(data = interestelar_100,
                       map = (aes(x = LIQUIDEZ, y = RES))) +
  geom_point(aes(col = EFLO, size = SOLVENCIA), alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

scatter_plus <- ggplot(data = interestelar_100,
                       map = (aes(x = LIQUIDEZ, y = RES))) +
  geom_point(aes(col = EFLO, size = SOLVENCIA), alpha = 0.7) +
  geom_text(aes(label=row.names(interestelar_100)),
            size=2, color="black", alpha = 0.7) +
  ggtitle("LIQUIDEZ vs RESULTADO DEL EJERCICIO",
          subtitle = "Transporte Interestelar") +
  xlab("Ratio de Liquidez") +
  ylab("Resultado del ejercicio (miles PAVOs)")

ggMarginal(scatter_plus, type = "histogram", groupColour = T,
           groupFill = T, position = "identity", alpha = 0.5)

scatter_plus <- ggplot(data = interestelar_100,
                       map = (aes(x = LIQUIDEZ, y = RES))) +
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

# Fin de script :)

