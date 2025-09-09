# Analisis de correspondencias simple de eolicas
# Disculpen por la falta de tildes!

rm(list = ls())

# DATOS

  # Importando datos

    library (readxl)
    eolicas <- read_excel("eolica_contingencia.xlsx", sheet ="Datos")
    eolicas <- data.frame(eolicas, row.names = 1)
    summary (eolicas)

  # Seleccionando factores/atributos para el analisis

    library(dplyr)
    originales<-select(eolicas, DIMENSION, VALORACION)
    summary (originales)

  # Identificando missing values.

    library(visdat)
    vis_miss(originales)
    originales %>% filter(is.na(DIMENSION) | is.na(VALORACION)) %>%
      select(DIMENSION, VALORACION)  
    originales <- originales %>%
      filter(! is.na(DIMENSION) & ! is.na(VALORACION))  

# TABLA DE CONTINGENCIA

  tab.originales <- table(originales)

 library(knitr)
 library(kableExtra)
 knitr.table.format = "html"

 addmargins(tab.originales) %>%
  kable(format = knitr.table.format,
        caption="Empresas eólicas") %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped", "bordered", "condensed",
                position = "center",
                font_size = 12) %>%
  add_header_above(c(DIMENSION = 1, VALORACION = 3, " " = 1),
                   bold=T,
                   line=T) %>%
  row_spec(0, bold= T, align = "c") %>%
  column_spec(1, bold = T)

  # Representación gráfica de la tabla con mosaico
 
    library (vcd)
    mosaic(tab.originales,
            main = "Eólicas: Dimensión Matriz y Valoración Expertos.",
            shade = T,
            gp = shading_Marimekko(tab.originales),
            main_gp = gpar(fontsize = 14),
            sub_gp = gpar(fontsize = 12))

  # Representando frecuencias de categorias en factores

    library (ggplot2)
    library (patchwork)

    g1 <- ggplot(originales, mapping= aes(x= DIMENSION, fill = VALORACION)) +
          geom_bar() +
          ggtitle("Tamaño de la matriz.", subtitle = "Empresas eólicas") + 
          ylab("Frecuencias") +
          xlab("Dimensión") 

    g2 <- ggplot(originales, mapping= aes(x= VALORACION, fill = DIMENSION)) +
          geom_bar() +
          ggtitle("Valoración Expertos", subtitle = "Empresas eólicas") + 
          ylab("Frecuencias") +
          xlab("Valoración") 

    (g1 + g2) + plot_annotation(title = "Frecuencias Marginales.",
                  theme = theme(plot.title = element_text(size = 14)))


# INDEPENDENCIA / ASOCIACION
  
  # Test de asociación de Pearson (Ji-Cuadrado)
    
  Prueba_asoc_Pearson <- chisq.test(tab.originales)
  Prueba_asoc_Pearson
  Residuos_std <- Prueba_asoc_Pearson$stdres

  # Definir una función de sombreado personalizada
  custom_shading <- function(residuals, cutoff = 1.96) {
    # Crear una matriz de colores basada en los residuos estandarizados
    colors <- ifelse(abs(residuals) > cutoff, "orange", "lightgray")
    return(colors)
  }
  
  # Aplicar la función de sombreado en el gráfico mosaico
  
  assoc(tab.originales,
        main = "Asociación: Dimensión Matriz y Valoración Expertos.",
        sub = "Residuos de Pearson Tipificados. Naranja: significativos con sig. = 0,05",
        compress = FALSE,
        gp = gpar(fill = custom_shading(Residuos_std)),
        main_gp = gpar(fontsize = 14),
        sub_gp = gpar(fontsize = 12))
  
  # ANALISIS DE CORRESPONDENCIAS SIMPLE
  
  library (FactoMineR)
  aceolicas <- CA(X = tab.originales, graph = F)
  
  SolucionCA <- list() 
  EigenCA <- as.data.frame(t(aceolicas$eig))
  EigenCA$elemento <- c("Inercia Total", "% Inercia Principal", "Acumulada")
  EigenCA <- data.frame(EigenCA, row.names = 3)
  
  
  TableEigenCA <- EigenCA %>%
    kable(format = knitr.table.format,
          caption="Análisis de correspondencias: Dimensión Matrix vs Valoración.",
          col.names = c("Dimensión/Eje 1", "Dimensión/Eje 2"),
          digits = 3,
          align= c("c", "c")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped", "bordered", "condensed",
                  position = "center",
                  font_size = 12) %>%
    row_spec(0, bold= T, align = "c")
  
  SolucionCA[[1]] <-TableEigenCA
  
  # Extraer la información de las filas
  rows_data <- aceolicas$row$coord
  rows_cos2 <- aceolicas$row$cos2
  
  # Crear un DataFrame para las filas
  rows_df <- data.frame(
    Iner_1000 = aceolicas$row$inertia,
    Dim_1 = rows_data[, 1],
    cos2_1 = rows_cos2[, 1],
    Dim_2 = rows_data[, 2],
    cos2_2 = rows_cos2[, 2]
  )
  rownames(rows_df) <- rownames(rows_data)
  
  TableRows <- rows_df %>%
    kable(format = knitr.table.format,
          caption= (paste0("Análisis de correspondencias: ", colnames(originales)[1], ".")),
          col.names = c("Inercia", "Coordenadas Dim. 1", "Cos2 Dim. 1",
                        "Coordenadas Dim. 2", "Cos2 Dim. 2"),
          digits = 3,
          align= c("c", "c", "c", "c", "c")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped", "bordered", "condensed",
                  position = "center",
                  font_size = 12) %>%
    row_spec(0, bold= T, align = "c")
  
  SolucionCA[[2]] <-TableRows
  
  # Extraer la información de las columnas
  columns_data <- aceolicas$col$coord
  columns_cos2 <- aceolicas$col$cos2
  
  # Crear un DataFrame para las columnas
  columns_df <- data.frame(
    Inercia = aceolicas$col$inertia,
    Dim_1 = columns_data[, 1],
    cos2_1 = columns_cos2[, 1],
    Dim_2 = columns_data[, 2],
    cos2_2 = columns_cos2[, 2]
  )
  rownames(columns_df) <- rownames(columns_data)
  
  TableCols <- columns_df %>%
    kable(format = knitr.table.format,
          caption= (paste0("Análisis de correspondencias: ", colnames(originales)[2], ".")),
          col.names = c("Inercia", "Coordenadas Dim. 1", "Cos2 Dim. 1",
                        "Coordenadas Dim. 2", "Cos2 Dim. 2"),
          digits = 3,
          align= c("c", "c", "c", "c", "c")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped", "bordered", "condensed",
                  position = "center",
                  font_size = 12) %>%
    row_spec(0, bold= T, align = "c")
  
  SolucionCA[[3]] <-TableCols
  
  print(SolucionCA[[1]])
  print(SolucionCA[[2]])
  print(SolucionCA[[3]])

  # Gráfico de contribuciones de las dimensiones  a la Inercia Total

    library(factoextra)

    gcontrib <- fviz_screeplot(aceolicas,
                               addlabels= F,
                               barcolor= "darkblue",
                               barfill= "orange",
                               linecolor= "red") +
                labs(title= "Contribución de los ejes a la Inercia Total.",
                     subtitle = "Dimensión Matriz y Valoración Expertos.") +
                ylab("Porcentaje de Inercia Total") +
                xlab("Eje") +
                theme(text = element_text(size = 12))

    gcontrib

  # Gráfico bidimensional

    gbiplot <- fviz_ca_biplot (aceolicas,
                               axes= c(1,2),
                               label= "all",
                               repel = T,
                               col.col= "orange",
                               col.row= "darkblue",
                               map= "symmetric") +
    labs(title= "Gráfico de dispersión de categorías.",
         subtitle = "Eólicas: Matriz y Valoración Expertos.") +
    theme(text = element_text(size = 12))

    gbiplot

    gcombinado <- gcontrib / gbiplot
    gcombinado <- gcombinado +
      plot_annotation(title = "DIMENSIÓN MATRIZ vs VALORACIÓN EXPERTOS.",
      subtitle = "Empresas eólicas.",
      caption = "Análisis de Correspondencias Simple.",
      theme = theme(plot.title = element_text(size = 16, face = "bold"),
                    plot.subtitle = element_text(size = 14),
                    plot.caption = element_text(size = 12))
    )

    gcombinado
# Fin del script :)