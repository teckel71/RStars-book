# Análisis Clúster jerárquico (Ward)

# Limpiando el Global Environment

rm(list = ls())

# Cargando paquetes

library(MATdatatools)
library(MATmultivar)
library(dplyr)
library(gtExtras)

# importando datos

datos <- MATfexcel("interestelar_25.xlsx", "Datos",
                   na_values = c("n.d."))

# Seleccionando variables metricas para el analisis.

library(dplyr)
seleccion <- datos %>%
  select(IDIVERSE, IFIDE, IDIG)
seleccion_df_graph <- gt_plt_summary(seleccion)
seleccion_df_graph

# Identificando missing values.

MATmv(seleccion)
seleccion_sm_info$grafico_vis_miss
seleccion_sm_info$tabla_na

# Identificación de outliers.

MATout_Mahalanobis(seleccion_sm)
remove(seleccion_sm_so)
seleccion_sm_so_info$Boxplot
seleccion_sm_so_info$Outliers_Table

# Modelo tentativo para determinar k

cluster0 <- MATclus_Ward(seleccion_sm, IDIVERSE, IFIDE, IDIG,
                         k=0, silueta = T)

cluster0$hclust         # modelo guardado
cluster0$heatmap        # mapa de calor con distancias euclídeas
cluster0$dendrogram     # dendograma
cluster0$silhouette     # k aconsejado por método de la Silueta.

# Selección de k y resultados.

cluster1 <- MATclus_Ward(seleccion_sm, IDIVERSE, IFIDE, IDIG,
                         k=5, silueta = F)

cluster1$dendrogram          # dendograma
cluster1$group_summary       # tabla resumen de grupos
cluster1$bar_patchworks      # gráficos de barras por variable
cluster1$scatter_patchworks  # gráficos de dispersión variable x variable

cluster1$data_groups         # dataframe con grupo de pertenencia
seleccion_con_grupos <- cluster1$data_groups %>%
                       select(-NOMBRE)

