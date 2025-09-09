# CLUSTER de k-medias productores eolicos. Disculpad la falta de tildes!!!!

rm(list = ls())

# DATOS

  # Importando

    library(readxl)
    eolicos <- read_excel("eolica_350.xlsx", sheet = "Datos",
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
      filter(! is.na(RES) & ! is.na(MARGEN) & ! is.na(FPIOS) &
             ! is.na(SOLVENCIA))  

  # Identificando outliers.

    originales <- originales %>% mutate(MAHALANOBIS = mahalanobis((.),
                                        center = colMeans(.),
                                        cov=cov(.)))

    library (ggplot2)
    
    ggplot(data = originales, map = (aes(y = log(MAHALANOBIS)))) +
    geom_boxplot(fill = "orange") +
    ggtitle("DISTANCIA DE MAHALANOBIS", subtitle = "Empresas eólicas") +
    ylab("ln(MAHALANOBIS)")

    Q1M <- quantile (originales$MAHALANOBIS, c(0.25))
    Q3M <- quantile (originales$MAHALANOBIS, c(0.75))

    originales %>%
      filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
             MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS)) %>%
      select(MAHALANOBIS)

    originales_so <- originales %>%
                      filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
                             MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS))

    originales_so <- originales_so %>% select(-MAHALANOBIS)

# CLUSTER K-MEDIAS CON VARIABLES ORIGINALES.

  # Tipificando variables

    zoriginales_so <- data.frame(scale(originales_so))
    summary (zoriginales_so)

  # Numero de grupos

    library(NbClust)

    result <- capture.output(NbClust(data = zoriginales_so,
                                     min.nc = 2,
                                     max.nc = 10,
                                     method = "kmeans"))

    start_line <- grep("* Among all indices:", result)
    end_line <- grep("* According to", result)
    print(result[start_line:end_line])

  k <- 3  # poner aquí número de grupos decidido!!!!

  # Aplicando k-means con inicializacion kmeans++

  library (ClusterR)

  set.seed(123)

  cluster_k <-KMeans_rcpp (zoriginales_so,
                           clusters = k,
                           num_init = 10,
                           max_iters = 100,
                           initializer = "kmeans++")

  originales_so$whatcluster_k <- as.factor(cluster_k$clusters)
  summary(originales_so)

# CARACTERIZANDO GRUPOS FORMADOS
  
  # Tabla con centroides de grupos.

    tablamedias <- originales_so %>%
     group_by(whatcluster_k) %>% summarise(obs = length(whatcluster_k),
                                        Resultado = round(mean(RES),0),
                                        Margen = round(mean(MARGEN),2),
                                        Fondos_Propios = round(mean(FPIOS),0),
                                        Solvencia = round(mean(SOLVENCIA),2))

    library (knitr)
    library (kableExtra)
    knitr.table.format = "html"

    tablamedias %>%
      kable(format = knitr.table.format,
            caption = "Método de k-medias. 3 grupos. Medias de variables",
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
                subtitle = "Empresas eólicas")+
        xlab ("Grupo") +
        ylab(var1)
        graficos.centroides[[paste0("grafico_", var1)]] <- grafico
      }                   

  # Función para crear agrupaciones de 2x2 con espacios en blanco si necesario

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

  # Aplicar función a gráficos de centroides.
    grupos.graficos.centroides <- create_patchwork(graficos.centroides)

  # Presentar las composiciones
    for (n in 1:length(grupos.graficos.centroides)){
         print(grupos.graficos.centroides[[n]])
    }

  # ¿Centroides estadísticamente significativos?

    # Vector de nombre de variables excluyendo la variable no deseada
      variables <- setdiff(names(originales_so), "whatcluster_k")

    # Inicializar listas para almacenar gráficos y tablas
      graficos_kw <- list()
      tablas_kw <- list()
      library(pgirmess)

  # Bucle para generar gráficos y análisis
    for (i in seq_along(variables)) {
         variable <- variables[i]
  
    # Crear el gráfico
      p <- ggplot(data = originales_so,
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
                   subtitle = "Empresas eólicas") +
           ylab("Valor")
  
    # Almacenar el gráfico en la lista
      graficos_kw[[i]] <- p
  
    # Realizar el análisis de Kruskal-Wallis
      datos_kmc <- kruskalmc(as.formula(paste(variable, "~ whatcluster_k")),
                             data = originales_so)
      tabla <- datos_kmc$dif.com %>%
               kable(format = knitr.table.format,
                     caption = paste("k-medias. Diferencias de Centroides",
                                     variable),
                     col.names = c("Diferencias centros",
                                   "Diferencias críticas",
                                   "Significación")) %>%
               kable_styling(full_width = F,
                             bootstrap_options = c("striped",
                                                   "bordered",
                                                   "condensed"),
                             position = "center",
                             font_size = 11) %>%
               row_spec(0, bold = T, align = "c") %>%
               row_spec(1:nrow(datos_kmc$dif.com), bold = F, align = "c")
  
    # Almacenar la tabla en la lista
      tablas_kw[[i]] <- tabla
    }
  
    # Crear composiciones de gráficos.

      gruposgraficos_kw <- create_patchwork(graficos_kw)

    # Mostrar los gráficos y tablas almacenados

      for (i in seq_along(gruposgraficos_kw)) {
           print(gruposgraficos_kw[[i]])
      }
      for (i in seq_along(tablas_kw)) {
           print(tablas_kw[[i]])
      }

# GRÁFICOS Variable vs Variable

  # Lista de variables excluyendo la variable no deseada
    variables <- setdiff(names(originales_so), "whatcluster_k")

  # Lista para almacenar los gráficos
    graficos <- list()

  # Generar todas las combinaciones posibles de pares de variables
    combinaciones <- combn(variables, 2, simplify = FALSE)

  # Bucle para crear y almacenar los gráficos
    for (i in seq_along(combinaciones)) {
         var1 <- combinaciones[[i]][1]
         var2 <- combinaciones[[i]][2]
         grafico <- ggplot(originales_so,
                           map = aes_string(x = var1,
                                            y = var2,
                                            color = "whatcluster_k")) +
         geom_point() +
         labs(title = paste("GRÁFICO", var1, "-", var2),
              subtitle = "Empresas eólicas") +
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
