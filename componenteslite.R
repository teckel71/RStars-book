# Análisis de Componentes Principales

# Limpiando el Global Environment

rm(list = ls())

# Cargando paquetes

library(MATdatatools)
library(MATmultivar)
library(dplyr)
library(gtExtras)

# importando datos

datos <- MATfexcel("interestelar_100.xlsx", "Datos",
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

# Identificando y eliminando outliers.

MATout_Mahalanobis(seleccion_sm)
seleccion_sm_so_info$Boxplot
seleccion_sm_so_info$Outliers_Table

# Correlaciones.

MATcor(seleccion_sm_so)
seleccion_sm_so_correlaciones_info$correlaciones

# Obtencion de componentes.

componentes <- MATpca(seleccion_sm_so)

# varianza retenida por cada componente
componentes$info$resumen_componentes

# cargas
componentes$info$cargas_componentes

# gráfico para saber cuántas componentes retener
componentes$info$graficos

# Puntuaciones o scores.
MATpcascores(seleccion_sm_so, componentes$pca, Nscores =  1, Rank =  10)

# Ranking.
scores_seleccion_sm_so_componentes_info$resultados$PC1$tabla
scores_seleccion_sm_so_componentes_info$resultados$PC1$grafico

# Puntuaciones o scores (con outliers)
MATpcascores(seleccion_sm, componentes$pca, Nscores =  1, Rank =  10)

# Ranking.
scores_seleccion_sm_componentes_info$resultados$PC1$tabla
scores_seleccion_sm_componentes_info$resultados$PC1$grafico

# Fin del script :)
