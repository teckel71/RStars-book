### CLUSTER de k-medias empresas TMI. ###

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(gtExtras)
library(visdat)
library (cluster)
library (ClusterR)
library (patchwork)
library (pgirmess)

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

# Eliminando outliers.
seleccion_so <-seleccion %>%
  filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
         MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS))%>%
  select(IDIVERSE, IFIDE, IDIG)

# Eliminando variable MAHALANOBIS del df con outliers
seleccion <- seleccion %>% select(-MAHALANOBIS)


## CLUSTER K-MEDIAS CON VARIABLES ORIGINALES

# Tipificando variables
zseleccion_so <- data.frame(scale(seleccion_so))
summary(zseleccion_so)

d <- dist(zseleccion_so)

# --- Calcular el ancho medio de silueta para distintos k ---
set.seed(123)
res_sil <- sapply(2:10, 
                  function(k) {
                    km <- KMeans_rcpp(zseleccion_so,
                    clusters = k,
                    num_init = 10,
                    max_iters = 100,
                    initializer = "kmeans++")
  mean(silhouette(km$clusters, d)[, "sil_width"])
}
)

# --- Crear data frame para graficar ---
df_sil <- data.frame(
  k = 2:10,
  Silhouette = res_sil
)

# --- Gráfico con ggplot2 ---
ggplot(df_sil, aes(x = k, y = Silhouette)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "darkred") +
  labs(title = "Ancho medio de Silueta para distintos k (k-means++)",
       x = "Número de clústeres (k)",
       y = "Silueta media") +
  theme_minimal(base_size = 13)

# Aplicación definitiva de k-medias.
  k <- 7  # poner aquí número de grupos decidido!!!!

  # Aplicando k-means con inicializacion kmeans++

  cluster_k <-KMeans_rcpp (zseleccion_so,
                           clusters = k,
                           num_init = 10,
                           max_iters = 100,
                           initializer = "kmeans++")

  seleccion_so$whatcluster_k <- as.factor(cluster_k$clusters)

  # CARACTERIZANDO GRUPOS FORMADOS
  
  # Tabla con centroides de grupos.
  tablamedias <- seleccion_so %>%
    group_by(whatcluster_k) %>%
    summarise(obs = length(whatcluster_k),
              Idiverse = mean(IDIVERSE),
              Ifide = mean(IFIDE),
              Idig = mean(IDIG))
  
  tablamedias %>%
    kable_rstars(caption   = "Método de k-medias. 7 grupos. Medias de variables",
                 col.names = c("Clúster",
                               "Observaciones",
                               "I. Diversif.",
                               "I. Fidelizac.",
                               "I. Digitalizac."),
                 digits    = c(NA, 0, 3, 3, 3))

# Gráficos de centroides
  
  # Vector de nombre de variables excluyendo la variable no deseada
  variables <- setdiff(names(tablamedias), c("whatcluster_k", "obs"))
  
  # Lista para almacenar los gráficos
  graficos.centroides <- list()
  
  # Bucle para crear y almacenar los gráficos
  for (i in seq_along(variables)) {
    var1 <- variables[[i]]
    grafico <- ggplot(data= tablamedias,
                      map = (aes_string(y = var1, x = "whatcluster_k"))) +
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

  # ¿Diferencias entre centroides estadísticamente significativas?

    # Vector de nombre de variables excluyendo la variable no deseada
      variables <- setdiff(names(seleccion_so), "whatcluster_k")

    # Inicializar listas para almacenar gráficos y tablas
      graficos_kw <- list()
      tablas_kw <- list()

  # Bucle para generar gráficos
    for (i in seq_along(variables)) {
         variable <- variables[i]
  
    # Crear el gráfico
      p <- ggplot(data = seleccion_so,
                  aes_string(x = "whatcluster_k",
                             y = variable,
                             fill = "whatcluster_k")) +
           geom_boxplot(outlier.shape = NA) +
           stat_summary(fun = "mean",
                        geom = "point",
                        size = 3,
                        col = "red") +
           stat_summary(fun = "mean",
                        geom = "line",
                        col = "red",
                        aes(group = TRUE)) +   
           geom_jitter(width = 0.1,
                       size = 1,
                       col = "red",
                       alpha = 0.40) +
           ggtitle(paste(variable, ". Comparación de grupos."),
                   subtitle = "Empresas TMI.") +
           ylab("Valor")
  
    # Almacenar el gráfico en la lista
      graficos_kw[[i]] <- p
    }

    # Crear composiciones de gráficos.
            gruposgraficos_kw <- create_patchwork(graficos_kw)    

            for (i in seq_along(gruposgraficos_kw)) {
              print(gruposgraficos_kw[[i]])
            }   
      
    # Realizar el análisis de Kruskal-Wallis  y llevar resultados a tablas
            
      tablas_kw <- list()
      for (i in seq_along(variables)) {
         variable <- variables[i]
         datos_kmc <- kruskalmc(as.formula(paste(variable, "~ whatcluster_k")),
                                data = seleccion_so)
              
         tabla <- datos_kmc$dif.com %>%
         kable_rstars(caption   = paste("k-medias. Diferencias de Centroides", variable),
                      col.names = c("Diferencias centros",
                                    "Diferencias críticas",
                                    "Significación"),
                      digits    = c(3, 3, NA))
              
       # Almacenar la tabla en la lista
              
         tablas_kw[[i]] <- tabla
       }
            
       # Mostrar tablas almacenadas
            
       for (i in seq_along(tablas_kw)) {
            print(tablas_kw[[i]])
            }

# GRÁFICOS Variable vs Variable

  # Lista de variables excluyendo la variable no deseada
    variables <- setdiff(names(seleccion_so), "whatcluster_k")

  # Lista para almacenar los gráficos
    graficos <- list()

  # Generar todas las combinaciones posibles de pares de variables
    combinaciones <- combn(variables, 2, simplify = FALSE)

  # Bucle para crear y almacenar los gráficos
    for (i in seq_along(combinaciones)) {
         var1 <- combinaciones[[i]][1]
         var2 <- combinaciones[[i]][2]
         grafico <- ggplot(seleccion_so,
                           map = aes_string(x = var1,
                                            y = var2,
                                            color = "whatcluster_k")) +
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

# Fin del Script :)
