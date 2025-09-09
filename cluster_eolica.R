# CLUSTER 25 empresas eolicas. Disculpad la falta de tildes!!!!

rm(list = ls())

# DATOS

# Importando

library(readxl)
eolicos <- read_excel("eolica_25.xlsx", sheet = "Datos",
                      na = c("n.d.", "s.d."))
eolicos <- data.frame(eolicos, row.names = 1)
summary (eolicos)

# Seleccionando variables clasificadoras para el analisis.

library(dplyr)
originales <- eolicos %>% select(RES, MARGEN, FPIOS, SOLVENCIA)
summary (originales)

# Identificando missing values.

library(visdat)
vis_miss(originales)
originales %>%
  filter(is.na(RES) | is.na(MARGEN) | is.na(FPIOS) | is.na(SOLVENCIA)) %>%
  select(RES, MARGEN, FPIOS, SOLVENCIA)  
originales <- originales %>%
  filter(! is.na(RES) & ! is.na(MARGEN) & ! is.na(FPIOS) & ! is.na(SOLVENCIA))  

# Identificando outliers.

originales <- originales %>% mutate(MAHALANOBIS = mahalanobis(.,
                                    center = colMeans(.),
                                    cov=cov(.)))

library (ggplot2)
ggplot(data = originales, map = (aes(y = MAHALANOBIS))) +
  geom_boxplot(fill = "orange") +
  ggtitle("DISTANCIA DE MAHALANOBIS", subtitle = "Empresas eólicas") +
  ylab("MAHALANOBIS")

Q1M <- quantile (originales$MAHALANOBIS, c(0.25))
Q3M <- quantile (originales$MAHALANOBIS, c(0.75))

originales %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
         MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS)) %>%
  select(MAHALANOBIS)

originales <- originales %>% select(-MAHALANOBIS)

# CLUSTER JERARQUICO CON VARIABLES ORIGINALES.

# Tipificando variables

zoriginales <- data.frame(scale(originales))
summary (zoriginales)

# Matriz de distancias

d <- dist(zoriginales)
library (factoextra)
fviz_dist(d, lab_size = 8)

# Método de Ward.

cluster_j<-hclust(d, method="ward.D2")
fviz_dend(cluster_j,
          cex = 0.6,
          rect = FALSE,
          labels_track_height = 5.5) +
  labs(title = "Empresas eólicas",
       subtitle = "Método de Ward. Variables originales tipificadas.") +
  theme_grey()

fviz_dend(cluster_j,
          cex = 0.6,
          k = 5, # número de grupos o conglomerados que se ha decidido formar!
          k_colors = "black",
          labels_track_height = 5.5,
          rect = TRUE,
          rect_border = "npg",
          rect_fill = TRUE) +
  labs(title = "Empresas eólicas",
       subtitle = "Método de Ward. Variables originales tipificadas.") +
  theme_grey()

# CARACTERIZACIÓN Y COMPOSICIÓN DE GRUPOS.

originales$whatcluster_j <- as.factor(cutree(cluster_j, k=5))
levels(originales$whatcluster_j)

# Tabla con centroides de grupos.

tablamedias <- originales %>%
  group_by(whatcluster_j) %>% summarise(obs = length(whatcluster_j),
                                        Resultado = round(mean(RES),0),
                                        Margen = round(mean(MARGEN),2),
                                        Fondos_Propios = round(mean(FPIOS),0),
                                        Solvencia = round(mean(SOLVENCIA),2))

library (knitr)
library (kableExtra)
knitr.table.format = "html"

tablamedias %>%
  kable(format = knitr.table.format,
        caption = "Método de Ward. 5 grupos. Medias de variables",
        col.names = c("Clúster", "Observaciones", "Resultado", "Margen",
                      "Fondos Propios", "Solvencia")) %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped", "bordered", "condensed",
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:nrow(tablamedias), bold= F, align = "c")

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
                       subtitle = "Empresas eólicas")+
               xlab ("Grupo") +
               ylab(var1)
    graficos.centroides[[paste0("grafico_", var1)]] <- grafico
}                   

# Función para crear agrupaciones de 2x2 con espacios en blanco si es necesario

library (patchwork)    
    
  create_patchwork <- function(plot_list) {
    n <- length(plot_list)
    full_rows <- n %/% 4
    remaining <- n %% 4
      
    patchworks <- list()
      
    # Crear agrupaciones completas de 2x2
      for (i in seq(1, full_rows * 4, by = 4)) {
        patchworks <- c(patchworks,
                        list((plot_list[[i]] + plot_list[[i+1]]) /
                             (plot_list[[i+2]] + plot_list[[i+3]])))
      }
      
    # Crear la última agrupación con espacios en blanco si es necesario
      if (remaining > 0) {
        last_plots <- plot_list[(full_rows * 4 + 1):n]
        empty_plots <- lapply(1:(4 - remaining),
                              function(x) ggplot() + theme_void())
        last_patchwork <- do.call(patchwork::wrap_plots,
                                  c(last_plots, empty_plots))
        patchworks <- c(patchworks, list(last_patchwork))
      }
      
      return(patchworks)
    }
    
  # Aplicar a gráficos de centroides.
  
    grupos.graficos.centroides <- create_patchwork(graficos.centroides)
  
  # Presentar las composiciones
    for (n in 1:length(grupos.graficos.centroides)){
      print(grupos.graficos.centroides[[n]])
    }

# Tablas con composiciones de grupos

  # Número de tablas y lista para guardarlas

  numclusters <- nlevels(originales$whatcluster_j)
  tablascompo <- list()

  # Bucle para generar las tablas

  for (n in 1:numclusters){
      tabla <- originales %>%
      filter(whatcluster_j == as.character(n)) %>%
      select(RES, MARGEN, FPIOS, SOLVENCIA) %>%
      kable(caption = paste("Método de Ward. Grupo ", n, "."),
            col.names = c("Resultado", "Margen",
                          "Fondos Propios", "Solvencia"),
            format.args = list(decimal.mark = ".", digits = 2)) %>%
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
    variables <- setdiff(names(originales), "whatcluster_j")

  # Lista para almacenar los gráficos
    graficos <- list()

  # Generar todas las combinaciones posibles de pares de variables
    combinaciones <- combn(variables, 2, simplify = FALSE)

  # Bucle para crear y almacenar los gráficos
    for (i in seq_along(combinaciones)) {
      var1 <- combinaciones[[i]][1]
      var2 <- combinaciones[[i]][2]
      grafico <- ggplot(originales,
                        map = aes_string(x = var1,
                                         y = var2,
                                         color = "whatcluster_j")) +
                 geom_point() +
                 labs(title = paste("GRÁFICO", var1, "-", var2),
                      subtitle = "Empresas eólicas") +
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
