### CLUSTER jerárquico 25 empresas TMI.###

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(gtExtras)
library(visdat)
library (factoextra)
library (knitr)
library (kableExtra)
library (patchwork)

##### Función para crear composiciones de gráficos con patchwork ###############
create_patchwork <- function(plot_list) {
  n <- length(plot_list)
  if (n == 0) return(NULL)
  full_rows <- n %/% 4
  remaining <- n %% 4
  patchworks <- list()
  
  if (full_rows > 0) {
    for (i in seq(1, full_rows * 4, by = 4)) {
      patchworks <- c(patchworks, list((plot_list[[i]] + plot_list[[i+1]]) / 
                                         (plot_list[[i+2]] + plot_list[[i+3]])))
    }
  }
  
  if (remaining > 0) {
    last_plots <- plot_list[(full_rows * 4 + 1):n]
    empty_plots <- lapply(1:(4 - remaining), function(x) ggplot() + theme_void())
    last_patchwork <- do.call(patchwork::wrap_plots, c(last_plots, empty_plots))
    patchworks <- c(patchworks, list(last_patchwork))
  }
  return(patchworks)
}
################################################################################

## DATOS

# Importando datos desde Excel
interestelar_25 <- read_excel("interestelar_25.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_25 <- data.frame(interestelar_25, row.names = 1)

# Seleccionando variables metricas para el analisis.
seleccion <- interestelar_25 %>%
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

# Identificando outliers con distancia de Mahalanobis.
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

# Eliminando variable MAHALANOBIS del df
seleccion    <- seleccion    %>% select(-MAHALANOBIS)

## CLUSTER JERARQUICO CON VARIABLES ORIGINALES.

# Tipificando variables
zseleccion <- data.frame(scale(seleccion))
summary(zseleccion)

# Matriz de distancias
d <- dist(zseleccion)
fviz_dist(d, lab_size = 8)  # Del paquete {factoextra}

# Método de Ward.
cluster_j<-hclust(d, method="ward.D2")

fviz_dend(cluster_j,
          cex = 0.6,
          rect = FALSE,
          labels_track_height = 5.5) +
  labs(title = "Empresas TMI.",
       subtitle = "Método de Ward. Variables originales tipificadas.") +
  theme_grey()

# Método de obtención de número de grupos (k) del Ancho de Silueta.
p <- fviz_nbclust(
  x = zseleccion, 
  FUNcluster = hcut, 
  method = "silhouette",
  hc_method = "ward.D2",
  k.max = 15
)
p

ngrupos = 5   # números de conglomerados decidido! #############################
fviz_dend(cluster_j,
          cex = 0.6,
          k = ngrupos, # número de conglomerados que se ha decidido formar!
          k_colors = "black",
          labels_track_height = 5.5,
          rect = TRUE,
          rect_border = "npg",
          rect_fill = TRUE) +
  labs(title = "Empresas TMI.",
       subtitle = "Método de Ward. Variables originales tipificadas.") +
  theme_grey()

## CARACTERIZACIÓN Y COMPOSICIÓN DE GRUPOS.

# cortando árbol en grupos
cl <- cutree(cluster_j, k = ngrupos)

# etiquetas en el orden exacto del dendrograma (izq→der)
ord_labels <- cluster_j$labels[cluster_j$order]

# posición (1..n) de cada caso según ese orden
pos <- match(names(cl), ord_labels)

# mapa de id antiguo -> id nuevo (1..ngrupos) según el bloque más a la izquierda
mins       <- tapply(pos, cl, min)             # posición más a la izq por clúster
old_order  <- names(sort(mins))                # ids antiguos ordenados izq→der
map        <- setNames(seq_along(old_order), old_order)

# Formar etiqueta de grupos (factor "whatcluster")
whatcluster_j <- factor(map[as.character(cl)], levels = 1:ngrupos)
seleccion$whatcluster_j <- whatcluster_j

# Tabla con centroides de grupos.
tablamedias <- seleccion %>%
               group_by(whatcluster_j) %>%
               summarise(obs = length(whatcluster_j),
                                      Idiverse = mean(IDIVERSE),
                                      Ifide = mean(IFIDE),
                                      Idig = mean(IDIG))

tablamedias %>%
  kable(caption = "Método de Ward. 5 grupos. Medias de variables",
        col.names = c("Clúster",
                      "Observaciones",
                      "I. Diversif.",
                      "I. Fidelizac.",
                      "I. Digitalizac."),
        digits = c(0, 3, 3, 3),
        format.args = list(decimal.mark = ".",
                           scientific = FALSE)) %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped",
                                    "bordered",
                                    "condensed",
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T,
           align = "c") %>%
  row_spec(1:nrow(tablamedias),
           bold= F,
           align = "c")

# Gráficos de centroides

  # Vector de nombre de variables excluyendo la variable no deseada
    variables <- setdiff(names(tablamedias), c("whatcluster_j", "obs"))

  # Lista para almacenar los gráficos
    graficos.centroides <- list()

  # Bucle para crear y almacenar los gráficos
    for (i in seq_along(variables)) {
    var1 <- variables[[i]]
    grafico <- ggplot(data= tablamedias,
                      map = (aes_string(y = var1, x = "whatcluster_j"))) +
               geom_bar(stat = "identity",
                        colour = "red",
                        fill = "orange",
                        alpha = 0.7) +
               ggtitle(paste0(var1, ". Media por grupos."),
                       subtitle = "Empresas MIT.")+
               xlab ("Grupo") +
               ylab(var1)
    graficos.centroides[[paste0("grafico_", var1)]] <- grafico
}                   

# Aplicar función de composiciones a gráficos de centroides.
  grupos.graficos.centroides <- create_patchwork(graficos.centroides)
  
# Presentar las composiciones
  for (n in 1:length(grupos.graficos.centroides)){
       print(grupos.graficos.centroides[[n]])
  }

# Tablas con composiciones de grupos

  # Número de tablas y lista para guardarlas

  numclusters <- nlevels(seleccion$whatcluster_j)
  tablascompo <- list()

  # Bucle para generar las tablas

  for (n in 1:numclusters){
      tabla <- seleccion %>%
      filter(whatcluster_j == as.character(n)) %>%
      select(IDIVERSE, IFIDE, IDIG) %>%
      kable(caption = paste("Método de Ward. Grupo ", n, "."),
            col.names = c("I. Diversificación",
                          "I. Fidelización",
                          "I. Digitalización"),
            digits = c(0, 3, 3, 3),
            format.args = list(decimal.mark = ".",
                               scientific = FALSE)) %>%
      kable_styling(full_width = FALSE, 
                    bootstrap_options = c("striped",
                                          "bordered",
                                          "condensed"),
                    position = "center",
                    font_size = 12) %>%
      row_spec(0, bold = TRUE, align = "c")
      tablascompo[[n]] <- tabla
  }

  # Presentar las tablas
  for (n in 1:numclusters){
    print(tablascompo[[n]])
  }

# Gráficos Variable vs Variable

  # Lista de variables excluyendo la variable no deseada
    variables <- setdiff(names(seleccion), "whatcluster_j")

  # Lista para almacenar los gráficos
    graficos <- list()

  # Generar todas las combinaciones posibles de pares de variables
    combinaciones <- combn(variables, 2, simplify = FALSE)

  # Bucle para crear y almacenar los gráficos
    for (i in seq_along(combinaciones)) {
      var1 <- combinaciones[[i]][1]
      var2 <- combinaciones[[i]][2]
      grafico <- ggplot(seleccion,
                        map = aes_string(x = var1,
                                         y = var2,
                                         color = "whatcluster_j")) +
                 geom_point() +
                 labs(title = paste("GRÁFICO", var1, "-", var2),
                      subtitle = "Empresas TMI") +
                 xlab (var1) +
                 ylab (var2) +
                 scale_color_brewer(palette = "Set1") 
      graficos[[paste0("grafico_", var1, "_", var2)]] <- grafico
}

  # Hacer agrupaciones con la función de patchworks creada anteriormente
    gruposgraficos <- create_patchwork(graficos)

  # Presentar las composiciones
    for (n in 1:length(gruposgraficos)){
      print(gruposgraficos[[n]])
    }

# Fin del Script :)
