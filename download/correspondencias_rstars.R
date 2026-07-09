#### Analisis de Correspondencias Simple. Universo R-Stars ####################
################################################################################

# Limpiando el Global Environment
  rm(list = ls())

# Cargando paquetes

  library (readxl)
  library (dplyr)
  library (ggplot2)
  library (gtExtras)
  library (visdat)
  library (knitr)
  library (kableExtra)
  library (patchwork)
  library (vcd)         # Visualización tablas de contingencia
  library (FactoMineR)  # Cálculo del análisis de correspondencias
  library (factoextra)  # Visualización de resultados del A. de Corresp.

################################################################################

# DATOS

  # Importando datos

    interestelar_300 <- read_excel("interestelar_300.xlsx", sheet = "Datos")
    interestelar_300 <- data.frame(interestelar_300, row.names = 1)

  # Seleccionando variables categoricas para el analisis

    seleccion <- interestelar_300 %>%
      dplyr::select(GALAXIA,
                    FJUR)
    seleccion_df_graph <- gt_plt_summary(seleccion)
    seleccion_df_graph

  # Localizando (posibles) missing values

    seleccion %>%
      vis_miss() +
      labs(title = "Análisis de correspondencias.",
           subtitle = "Transporte de mercancías interestelar",
           y = "Observación",
           fill = NULL) +
      scale_fill_manual(
        values = c("TRUE" = "red", "FALSE" = "grey"),
        labels = c("TRUE" = "NA", "FALSE" = "Presente")) +
      theme(
        plot.title = element_text(face = "bold", size = 14))

    seleccion %>% filter(is.na(GALAXIA) |
                         is.na(FJUR)) %>%
                  dplyr::select(GALAXIA,
                                FJUR)

  # Eliminando (si procede) filas con NA, y renombrando los niveles de GALAXIA

    seleccion <- seleccion %>%
      filter(!is.na(GALAXIA) &
             !is.na(FJUR)) %>%
      mutate(GALAXIA = case_match(GALAXIA,
        "Gran Nube de Magallanes"    ~ "GN Magallanes",
        "Pequeña Nube de Magallanes" ~ "PN Magallanes",
        "Galaxia de Andrómeda"       ~ "Andrómeda",
        "Galaxia del Triángulo"      ~ "Triángulo",
        "Vía Láctea"                 ~ "Vía Láctea",
        .default = GALAXIA))

################################################################################

# TABLA DE CONTINGENCIA (GALAXIA X FJUR)

  tab.GA.FJ <- with(seleccion, table(GALAXIA, FJUR))

  knitr.table.format = "html"

  addmargins(tab.GA.FJ) %>%
    kable(format = knitr.table.format,
          caption = "Empresas TMI: Galaxia y Forma Jurídica.") %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped", "bordered", "condensed",
                  position = "center",
                  font_size = 12) %>%
    add_header_above(c(GALAXIA = 1, FJUR = 3, " " = 1),
                     bold = T,
                     line = T) %>%
    row_spec(0, bold = T, align = "c") %>%
    column_spec(1, bold = T)

################################################################################

# ANALISIS DE CORRESPONDENCIAS SIMPLE

  # Formato de las tablas de resultados

    knitr.table.format <- "html"

  # Cálculo del análisis con la función CA() de {FactoMineR}

    acinterestelar <- CA(X = tab.GA.FJ, graph = F)

  # Estructura general de la solución

    SolucionCA <- list()

  # Tabla 1: Inercia principal por dimensión

    EigenCA <- as.data.frame(t(acinterestelar$eig))
    EigenCA$elemento <- c("Inercia Total",
                          "% Inercia Principal",
                          "Acumulada")
    EigenCA <- data.frame(EigenCA, row.names = ncol(EigenCA))

    TableEigenCA <- EigenCA %>%
      kable(format = knitr.table.format,
            caption = "Análisis de correspondencias: Galaxia vs Forma Jurídica.",
            digits = 3,
            align = rep("c", ncol(EigenCA))) %>%
      kable_styling(full_width = F,
                    bootstrap_options = "striped", "bordered", "condensed",
                    position = "center",
                    font_size = 12) %>%
      row_spec(0, bold = T, align = "c")

    SolucionCA[[1]] <- TableEigenCA

  # Tabla 2: Categorías de las filas (GALAXIA)

    rows_data  <- acinterestelar$row$coord
    rows_cos2  <- acinterestelar$row$cos2

    rows_df <- data.frame(
      Inercia = acinterestelar$row$inertia,
      Dim_1   = rows_data[, 1],
      cos2_1  = rows_cos2[, 1],
      Dim_2   = rows_data[, 2],
      cos2_2  = rows_cos2[, 2]
    )
    rownames(rows_df) <- rownames(rows_data)

    TableRows <- rows_df %>%
      kable(format = knitr.table.format,
            caption = paste0("Análisis de correspondencias: ",
                             names(dimnames(tab.GA.FJ))[1], "."),
            col.names = c("Inercia",
                          "Coordenadas Dim. 1", "Cos2 Dim. 1",
                          "Coordenadas Dim. 2", "Cos2 Dim. 2"),
            digits = 3,
            align = c("c", "c", "c", "c", "c")) %>%
      kable_styling(full_width = F,
                    bootstrap_options = "striped", "bordered", "condensed",
                    position = "center",
                    font_size = 12) %>%
      row_spec(0, bold = T, align = "c")

    SolucionCA[[2]] <- TableRows

  # Tabla 3: Categorías de las columnas (FJUR)

    columns_data <- acinterestelar$col$coord
    columns_cos2 <- acinterestelar$col$cos2

    columns_df <- data.frame(
      Inercia = acinterestelar$col$inertia,
      Dim_1   = columns_data[, 1],
      cos2_1  = columns_cos2[, 1],
      Dim_2   = columns_data[, 2],
      cos2_2  = columns_cos2[, 2]
    )
    rownames(columns_df) <- rownames(columns_data)

    TableCols <- columns_df %>%
      kable(format = knitr.table.format,
            caption = paste0("Análisis de correspondencias: ",
                             names(dimnames(tab.GA.FJ))[2], "."),
            col.names = c("Inercia",
                          "Coordenadas Dim. 1", "Cos2 Dim. 1",
                          "Coordenadas Dim. 2", "Cos2 Dim. 2"),
            digits = 3,
            align = c("c", "c", "c", "c", "c")) %>%
      kable_styling(full_width = F,
                    bootstrap_options = "striped", "bordered", "condensed",
                    position = "center",
                    font_size = 12) %>%
      row_spec(0, bold = T, align = "c")

    SolucionCA[[3]] <- TableCols

    SolucionCA[[1]]
    SolucionCA[[2]]
    SolucionCA[[3]]

################################################################################

# GRÁFICOS DEL ANÁLISIS DE CORRESPONDENCIAS

  # Gráfico de contribuciones de las dimensiones a la Inercia Total

    gcontrib <- fviz_screeplot(acinterestelar,
                               addlabels = TRUE,
                               barcolor = "darkblue",
                               barfill = "orange",
                               linecolor = "red") +
      labs(title = "Contribución de los ejes a la Inercia Total.",
           subtitle = "Galaxia y Forma Jurídica.") +
      ylab("Porcentaje de Inercia Total") +
      xlab("Eje") +
      theme(text = element_text(size = 12))

    gcontrib

  # Contribución de las categorías (filas y columnas) a las dimensiones

    grow_dim1 <- fviz_contrib(acinterestelar,
                              choice = "row",
                              axes = 1,
                              fill = "orange",
                              color = "darkblue") +
      theme(text = element_text(size = 9))

    grow_dim2 <- fviz_contrib(acinterestelar,
                              choice = "row",
                              axes = 2,
                              fill = "orange",
                              color = "darkblue") +
      theme(text = element_text(size = 9))

    gcol_dim1 <- fviz_contrib(acinterestelar,
                              choice = "col",
                              axes = 1,
                              fill = "orange",
                              color = "darkblue") +
      theme(text = element_text(size = 9))

    gcol_dim2 <- fviz_contrib(acinterestelar,
                              choice = "col",
                              axes = 2,
                              fill = "orange",
                              color = "darkblue") +
      theme(text = element_text(size = 9))

    (grow_dim1 + grow_dim2) / (gcol_dim1 + gcol_dim2)

  # Presentación compacta de las contribuciones con título general

    gcontribs <- (grow_dim1 + grow_dim2) / (gcol_dim1 + gcol_dim2)
    gcontribs <- gcontribs +
      plot_annotation(title = "Contribuciones de las categorías a las dimensiones.",
                      subtitle = "Empresas TMI: Galaxia y Forma Jurídica.",
                      theme = theme(plot.title = element_text(size = 14, face = "bold"),
                                    plot.subtitle = element_text(size = 12)))

    gcontribs

  # Gráfico bidimensional (biplot simétrico) de categorías

    gbiplot <- fviz_ca_biplot(acinterestelar,
                              axes = c(1, 2),
                              label = "all",
                              repel = TRUE,
                              col.col = "orange",
                              col.row = "darkblue",
                              map = "symmetric") +
      labs(title = "Gráfico de dispersión de categorías.",
           subtitle = "Empresas TMI: Galaxia y Forma Jurídica.") +
      theme(text = element_text(size = 12))

    gbiplot

  # Presentación compacta: contribuciones + biplot

    gcombinado <- gcontrib / gbiplot
    gcombinado <- gcombinado +
      plot_annotation(title = "GALAXIA vs FORMA JURÍDICA.",
                      subtitle = "Empresas TMI.",
                      caption = "Análisis de Correspondencias Simple.",
                      theme = theme(plot.title = element_text(size = 16, face = "bold"),
                                    plot.subtitle = element_text(size = 14),
                                    plot.caption = element_text(size = 12)))

    gcombinado

################################################################################
