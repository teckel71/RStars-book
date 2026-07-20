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
library (patchwork)

# Paquete MATrstars: funciones auxiliares del libro R-Stars.
# Contiene, entre otras, las funciones create_patchwork() y kable_rstars(),
# utilizadas más adelante.
# Si el paquete no está instalado, se instala desde GitHub (una sola vez).
if (!requireNamespace("MATrstars", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("teckel71/MATrstars")
}
library(MATrstars)

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

# Diagnóstico y filtrado de missing values con explora_na().
seleccion <- explora_na(
  seleccion,
  accion    = "eliminar",
  titulo    = "Indicadores: Diversificación, Fidelidad, Digitalización",
  subtitulo = "Transporte de mercancías interestelar"
)

# Diagnóstico de outliers con explora_outliers().
# Al pasar un df con varias variables métricas, la función calcula
# internamente la distancia de Mahalanobis. Usamos accion = "documentar"
# porque en el análisis cluster jerárquico solo interesa identificar los
# outliers y verlos como puntos en el dendrograma, sin eliminarlos.
explora_outliers(
  seleccion,
  accion    = "documentar",
  titulo    = "DISTANCIA DE MAHALANOBIS",
  subtitulo = "IDIVERSE, IFIDE, IDIG. Empresas TMI."
)

## CLUSTER JERARQUICO CON VARIABLES ORIGINALES.

# Tipificando variables
zseleccion <- data.frame(scale(seleccion))
summary(zseleccion)

# Matriz de distancias
d <- dist(zseleccion)
fviz_dist(d, lab_size = 8)  # Del paquete {factoextra}

# Método de Ward.
cluster_j<-hclust(d, method="ward.D2")

d1<-fviz_dend(cluster_j,
          cex = 0.6,
          lwd = 0.2,
          rect = FALSE,
          labels_track_height = 5.5) +
  labs(title = "Empresas TMI.",
       subtitle = "Método de Ward. Variables originales tipificadas.") +
  theme_grey() +
  theme(legend.position = "none")
d1$layers[[1]]$aes_params$linewidth <- 0.2
d1

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
d2<-fviz_dend(cluster_j,
          cex = 0.6,
          lwd = 0.2,
          k = ngrupos, # número de conglomerados que se ha decidido formar!
          k_colors = "black",
          labels_track_height = 5.5,
          rect = TRUE,
          rect_border = "npg",
          rect_fill = TRUE) +
  labs(title = "Empresas TMI.",
       subtitle = "Método de Ward. Variables originales tipificadas.") +
  theme_grey() +
  theme(legend.position = "none")
d2$layers[[1]]$aes_params$linewidth <- 0.2
d2

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
  kable_rstars(caption   = "Método de Ward. 5 grupos. Medias de variables",
               col.names = c("Clúster",
                             "Observaciones",
                             "I. Diversif.",
                             "I. Fidelizac.",
                             "I. Digitalizac."),
               digits    = c(0, 3, 3, 3))

# Gráficos de centroides

  # Vector de nombre de variables excluyendo la variable no deseada
    variables <- setdiff(names(tablamedias), c("whatcluster_j", "obs"))

  # Lista para almacenar los gráficos
    graficos.centroides <- list()

  # Bucle para crear y almacenar los gráficos
    for (i in seq_along(variables)) {
    var1 <- variables[[i]]
    grafico <- ggplot(data= tablamedias,
                      aes_string(y = var1, x = "whatcluster_j")) +
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
      kable_rstars(caption   = paste("Método de Ward. Grupo ", n, "."),
                   col.names = c("I. Diversificación",
                                 "I. Fidelización",
                                 "I. Digitalización"),
                   digits    = c(0, 3, 3, 3))
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
                        aes_string(x = var1,
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
