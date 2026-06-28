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
interestelar_300 <- read_excel("interestelar_300.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_300 <- data.frame(interestelar_300, row.names = 1)

# Seleccionando variables metricas para el analisis.
seleccion <- interestelar_300 %>%
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

# Eliminando outliers.
seleccion_so <-seleccion %>%
  filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
           MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS))%>%
  select(IDIVERSE, IFIDE, IDIG)

# Eliminando variable MAHALANOBIS del df con outliers
seleccion <- seleccion %>% select(-MAHALANOBIS)

## APLICACION DE DBSCAN

# 1) Tipificación (z-scores)
zseleccion <- scale(seleccion)
zseleccion <- as.matrix(zseleccion)  # dbscan espera matriz/numérico

# 2) Determinación de minPts y eps
minPts0 <- 6  # regla general: 2*p (p=3)
ruido_obj <- 0.20 # proporción objetivo de ruido (p. ej., 0.10, 0.20, 0.30)
tol_ruido <- 0.02 # tolerancia (±2 p.p.)
max_iter  <- 30   # tope de iteraciones

# 3) Rango inicial inteligente para eps usando kNN (k = minPts0-1)
knn_d <- kNNdist(zseleccion, k = minPts0 - 1)
d_sorted <- sort(as.numeric(knn_d))

eps_lo <- max(min(d_sorted) * 0.9, 1e-6)      # límite inferior
eps_hi <- max(d_sorted) * 1.5                  # límite superior amplio

# 4) Función auxiliar: calcula proporción de ruido para un eps
noise_rate <- function(eps) {
  fit <- dbscan::dbscan(zseleccion, eps = eps, minPts = minPts0)
  mean(fit$cluster == 0)
}

# 5) Búsqueda binaria para acercarnos a ruido_obj
tested <- data.frame(eps = numeric(0), ruido = numeric(0))

for (i in seq_len(max_iter)) {
  eps_mid <- (eps_lo + eps_hi) / 2
  r_mid   <- noise_rate(eps_mid)
  tested  <- rbind(tested, data.frame(eps = eps_mid, ruido = r_mid))
  
  if (abs(r_mid - ruido_obj) <= tol_ruido) break
  if (r_mid > ruido_obj) {
    # demasiado ruido -> aumentar eps para unir vecindarios y reducir ruido
    eps_lo <- eps_mid
  } else {
    # poco ruido -> disminuir eps para ser más estricto
    eps_hi <- eps_mid
  }
}

# 6) Elegimos el eps con ruido más cercano a la diana
ix_best   <- which.min(abs(tested$ruido - ruido_obj))
eps_final <- tested$eps[ix_best]
ruido_est <- tested$ruido[ix_best]

# 7) Modelo final
modelo_db <- dbscan::dbscan(zseleccion, eps = eps_final, minPts = minPts0)
modelo_db

# 8) Resumen rápido
cat("\n---\n",
    "eps_final =", round(eps_final, 3), 
    "| ruido_obj =", scales::percent(ruido_obj),
    "| ruido_logrado =", scales::percent(ruido_est), "\n")

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
  kable(caption = "Método DBSCAN. Medias de variables (Grupo 0 = Ruido)",
        col.names = c("Clúster",
                      "Observaciones",
                      "I. Diversif.",
                      "I. Fidelizac.",
                      "I. Digitalizac."),
        digits = c(NA, 0, 3, 3, 3),
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
variables <- setdiff(names(tablamedias), c("whatcluster_dbs", "obs"))

# Lista para almacenar los gráficos
graficos.centroides <- list()

# Bucle para crear y almacenar los gráficos
for (i in seq_along(variables)) {
  var1 <- variables[[i]]
  grafico <- ggplot(data= tablamedias,
                    map = (aes_string(y = var1, x = "whatcluster_dbs"))) +
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
                    map = aes_string(x = var1,
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
  kable(caption = "¿Outliers son ruido? (Grupo 0 = Ruido)",
        col.names = c("Caso",
                      "Observaciones",
                      "Grupo (0=ruido)",
                      "D. Mahalanobis",
                      "I. Diversificación",
                      "I. Fidelizac.",
                      "I. Digitalizac."),
        digits = c(NA, 0, 3, 3, 3),
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
  row_spec(1:nrow(seleccion_out),
           bold= F,
           align = "c")

# Fin del Script :)