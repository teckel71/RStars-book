### CLUSTER por algoritmo DBScan empresas TMI. ###

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(gtExtras)
library(visdat)
library(dbscan)
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
interestelar_300 <- read_excel("interestelar_300.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_300 <- data.frame(interestelar_300, row.names = 1)

# Seleccionando variables metricas para el analisis.
seleccion <- interestelar_300 %>%
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
# En el análisis DBSCAN se emplean todos los casos (los outliers no se
# eliminan del df), ya que el propio algoritmo los identificara como
# "ruido" (cluster 0). Aquí solo documentamos gráficamente qué casos
# se comportan como outliers estadísticos (Mahalanobis + regla 1.5·IQR)
# para poder contrastarlos más adelante con la clasificación de DBSCAN.
explora_outliers(
  seleccion,
  accion    = "documentar",
  titulo    = "DISTANCIA DE MAHALANOBIS",
  subtitulo = "IDIVERSE, IFIDE, IDIG. Empresas TMI."
)

## APLICACION DE DBSCAN

# 1) Tipificación (z-scores)
zseleccion <- scale(seleccion)
zseleccion <- as.matrix(zseleccion)  # dbscan espera matriz/numérico

# 2) Parámetro minPts (regla empírica: 2*p, con p = 3 variables)
minPts0 <- 6

# 3) Curva kNN para elegir eps por el método del codo
knn_d    <- kNNdist(zseleccion, k = minPts0 - 1)
d_sorted <- sort(as.numeric(knn_d))
df_knn   <- data.frame(punto = seq_along(d_sorted), distancia = d_sorted)

# Gráfico de la curva kNN
ggplot(df_knn, aes(x = punto, y = distancia)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Curva kNN para determinación de eps",
       subtitle = paste("k =", minPts0 - 1, "vecinos"),
       x = "Puntos ordenados por distancia",
       y = paste("Distancia al", minPts0 - 1, "vecino más cercano")) +
  theme_minimal(base_size = 13)

# 4) Valor de eps elegido en el codo de la curva kNN
eps_final <- 0.55

# 5) Modelo DBSCAN definitivo
modelo_db <- dbscan::dbscan(zseleccion, eps = eps_final, minPts = minPts0)
modelo_db

table(Cluster = modelo_db$cluster)

seleccion$whatcluster_dbs <- as.factor(modelo_db$cluster)

# CARACTERIZANDO GRUPOS FORMADOS

# Tabla con centroides de grupos.
tablamedias <- seleccion %>%
  group_by(whatcluster_dbs) %>%
  summarise(obs = length(whatcluster_dbs),
            Idiverse = mean(IDIVERSE),
            Ifide = mean(IFIDE),
            Idig = mean(IDIG))

tablamedias %>%
  kable_rstars(caption   = "Método DBSCAN. Medias de variables (Grupo 0 = Ruido)",
               col.names = c("Clúster",
                             "Observaciones",
                             "I. Diversif.",
                             "I. Fidelizac.",
                             "I. Digitalizac."),
               digits    = c(NA, 0, 3, 3, 3))

# Gráficos de centroides

# Vector de nombre de variables excluyendo la variable no deseada
variables <- setdiff(names(tablamedias), c("whatcluster_dbs", "obs"))

# Lista para almacenar los gráficos
graficos.centroides <- list()

# Bucle para crear y almacenar los gráficos
for (i in seq_along(variables)) {
  var1 <- variables[[i]]
  grafico <- ggplot(data= tablamedias,
                    aes_string(y = var1, x = "whatcluster_dbs")) +
    geom_bar(stat = "identity",
             colour = "red",
             fill = "orange",
             alpha = 0.7) +
    ggtitle(paste0(var1, ". Media por grupos."),
            subtitle = "Empresas TMI.")+
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

# GRÁFICOS Variable vs Variable

# Lista de variables excluyendo la variable no deseada
variables <- setdiff(names(seleccion), "whatcluster_dbs")

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
                                     color = "whatcluster_dbs")) +
    geom_point() +
    labs(title = paste("GRÁFICO", var1, "-", var2),
         subtitle = "Empresas TMI.") +
    xlab (var1) +
    ylab (var2) +
    scale_color_brewer(palette = "Set1") 
  graficos[[paste0("grafico_", var1, "_", var2)]] <- grafico
}

# Aplicar función de composiciones de patchwork

gruposgraficos <- create_patchwork(graficos)

# Presentar las composiciones
for (n in 1:length(gruposgraficos)){
  print(gruposgraficos[[n]])
}

# ¿Los outliers son ruido?

seleccion <- seleccion %>%
  mutate(MAHALANOBIS = mahalanobis(select(., IDIVERSE, IFIDE, IDIG),
                                   colMeans(select(., IDIVERSE, IFIDE, IDIG)),
                                   cov(select(., IDIVERSE, IFIDE, IDIG))))
Q1M <- quantile (seleccion$MAHALANOBIS, c(0.25))
Q3M <- quantile (seleccion$MAHALANOBIS, c(0.75))

seleccion_out <- seleccion %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
           MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS))%>%
  select(whatcluster_dbs, MAHALANOBIS, IDIVERSE, IFIDE, IDIG)

seleccion_out %>%
  kable_rstars(caption   = "¿Outliers son ruido? (Grupo 0 = Ruido)",
               col.names = c("Caso",
                             "Observaciones",
                             "Grupo (0=ruido)",
                             "D. Mahalanobis",
                             "I. Diversificación",
                             "I. Fidelizac.",
                             "I. Digitalizac."),
               digits    = c(NA, 0, 3, 3, 3))

# Fin del Script :)