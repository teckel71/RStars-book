# Analisis de Asociacion y modelos log-lineales de eolicas
# Disculpen por la falta de tildes!

rm(list = ls())

# DATOS

  # Importando datos

    library (readxl)
    eolicas <- read_excel("eolica_contingencia2.xlsx", sheet ="Datos")
    eolicas <- data.frame(eolicas, row.names = 1)
    summary (eolicas)

  # Seleccionando factores/atributos para el analisis

    library(dplyr)
    originales2 <- eolicas %>%
    select(DIMENSION, AUTOFINA, FJUR)
    summary (originales2)

  # Identificando missing values.

    library(visdat)
    vis_miss(originales2)
    originales2 %>% filter(is.na(DIMENSION) |
                           is.na(AUTOFINA)|
                           is.na(FJUR)) %>%
    select(DIMENSION, AUTOFINA, FJUR)  

    originales2 <- originales2 %>%
  filter(! is.na(DIMENSION) &
           ! is.na(AUTOFINA) &
           ! is.na(FJUR))  

# TABLA DE CONTINGENCIA

  tab.originales2 <- table(originales2)

  library(knitr)
  library(kableExtra)
  knitr.table.format = "html"

  tab.originales2 %>%
    kable(format = knitr.table.format,
         caption="Empresas eólicas") %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped", "bordered", "condensed",
                  position = "center",
                  font_size = 12) %>%
    row_spec(0, bold= T, align = "c")

  # Representación gráfica de la tabla con mosaico

    library (vcd)
   mosaic(tab.originales2,
          main = "Eólicas: Dimensión Matriz y Valoración Expertos.",
          shade = T,
          gp = shading_Marimekko(tab.originales2),
          main_gp = gpar(fontsize = 14),
          sub_gp = gpar(fontsize = 12),
          labeling_args = list(gp_labels = gpar(fontsize = 7)))

  # Representando frecuencias de categorias en factores

    library (ggplot2)
    library (patchwork)

    g1b <- ggplot(originales2, map= aes(x= DIMENSION,
                                        fill = DIMENSION)) +
    geom_bar() +
    ggtitle("Dimensión del grupo empresarial",
            subtitle = "Empresas eólicas") + 
    ylab("Frecuencias") +
    xlab("Dimensión") 

    g2b <- ggplot(originales2, map= aes(x= AUTOFINA,
                                        fill = AUTOFINA)) +
    geom_bar() +
    ggtitle("Valoración de expertos",
            subtitle = "Empresas eólicas") + 
    ylab("Frecuencias") +
    xlab("Valoración") 

    g3b <- ggplot(originales2, map= aes(x= FJUR,
                                        fill = FJUR)) +
    geom_bar() +
    ggtitle("Forma Jurídica",
            subtitle = "Empresas eólicas") + 
    ylab("Frecuencias") +
    xlab("Forma Jurídica") 

    (g1b / g2b / g3b) + plot_annotation(title = "Frecuencias Marginales.",
                      theme = theme(plot.title = element_text(size = 12)))

# MODELOS LOG-LINEALES

# Función para extraer coeficientes y sus nombres de un modelo ##########
extraer_coeficientes <- function(modelo) {
  # Extraer los parámetros del modelo
  parametros <- modelo$param
  
  # Crear listas para almacenar nombres y valores de coeficientes
  coef_names <- c()
  coef_values <- c()
  
  # Función para generar nombres de coeficientes
  generate_coef_name <- function(levels) {
    return(paste(levels, collapse = ":"))
  }
  
  # Recorrer los coeficientes y extraer los nombres y valores
  for (term in names(parametros)) {
    if (term == "(Intercept)") {
      coef_names <- c(coef_names, "T. Independiente")
      coef_values <- c(coef_values, parametros[[term]])
    } else if (is.matrix(parametros[[term]])) {
      # Si es una matriz, recorrer filas y columnas
      for (i in 1:nrow(parametros[[term]])) {
        for (j in 1:ncol(parametros[[term]])) {
          coef_names <- c(coef_names,
                          paste(rownames(parametros[[term]])[i],
                                colnames(parametros[[term]])[j],
                                sep = ":"))
          coef_values <- c(coef_values, parametros[[term]][i, j])
        }
      }
    } else if (is.array(parametros[[term]])) {
      # Si es un array de más de dos dimensiones
      dims <- dim(parametros[[term]])
      dimnames_list <- dimnames(parametros[[term]])
      for (i in seq_len(dims[1])) {
        for (j in seq_len(dims[2])) {
          for (k in seq_len(dims[3])) {
            coef_name <- paste(dimnames_list[[1]][i],
                               dimnames_list[[2]][j],
                               dimnames_list[[3]][k],
                               sep = ":")
            coef_names <- c(coef_names, coef_name)
            coef_values <- c(coef_values,
                             parametros[[term]][i, j, k])
          }
        }
      }
    } else {
      levels <- names(parametros[[term]])
      for (level in levels) {
        coef_names <- c(coef_names,
                        generate_coef_name(c(term, level)))
        coef_values <- c(coef_values,
                         parametros[[term]][[level]])
      }
    }
  }
  
  # Verificar la longitud de los vectores antes de crear el data frame
  if (length(coef_names) == length(coef_values)) {
    tabla_coeficientes <- data.frame(Coefficient = coef_names,
                                     Value = coef_values,
                                     stringsAsFactors = FALSE)
    return(tabla_coeficientes)
  } else {
    stop("Error: Las longitudes de coef_names y coef_values no coinciden.")
  }
}
############################################################################      
############################################################################
# Función generar tablas y gráfico a partir de modelo log-lineal

generar_solucion <- function(modelo) {
  # Extraer la información del modelo
  summary_modelo <- summary(modelo)
  
  # Crear una tabla con la información relevante de las pruebas de validez
  tabla_informacion <- data.frame(
    Statistic = c("Likelihood Ratio", "Pearson"),
    X2 = c(summary_modelo$tests[1, "X^2"], summary_modelo$tests[2, "X^2"]),
    df = c(summary_modelo$tests[1, "df"], summary_modelo$tests[2, "df"]),
    P_value = c(summary_modelo$tests[1, "P(> X^2)"], summary_modelo$tests[2, "P(> X^2)"]),
    stringsAsFactors = FALSE
  )
  
  # Formatear la tabla usando kable
  independencia_valida_tab <- tabla_informacion %>%
    kable(caption ="Validación del modelo",
          format = "html",
          col.names = c("Prueba", "Estadístico", "Grados Libertad", "P-valor")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = c("striped", "bordered", "condensed"),
                  position = "center",
                  font_size = 12) %>%
    row_spec(0, bold= T, align = "c")
  
  # Extraer los coeficientes del modelo
  independencia_df <- extraer_coeficientes(modelo)
  
  # Formatear la tabla de coeficientes usando kable
  independencia_coef_tab <- independencia_df %>%
    kable(format = "html",
          caption ="Coeficientes del modelo",
          digits = 3) %>%
    kable_styling(full_width = F,
                  bootstrap_options = c("striped", "bordered", "condensed"),
                  position = "center",
                  font_size = 12) %>%
    row_spec(0, bold= T, align = "c")
  
  # Crear el gráfico de mosaico con los residuos y mostrarlo en pantalla directamente
  plot(modelo, panel = mosaic,
       main="Residuos del modelo",
       residuals_type = c("deviance"),
       gp = shading_hcl,
       gp_args = list(interpolate = c(0, 1)),
       main_gp = gpar(fontsize = 14),
       sub_gp = gpar(fontsize = 9),
       labeling_args = list(gp_labels = gpar(fontsize = 7)))
  
  # Guardar las tablas en una lista
  solucion_nombre <- paste0("solucion_", deparse(substitute(modelo)))
  solucion_lista <- list(
    Informacion = independencia_valida_tab,
    Coeficientes = independencia_coef_tab
  )
  
  assign(solucion_nombre, solucion_lista, envir = .GlobalEnv)
}
##########################################################################

  # Modelo Independencia.

    modelo_indep <- MASS::loglm(~ DIMENSION + AUTOFINA + FJUR,
                                data= tab.originales2)

    generar_solucion(modelo_indep)

    solucion_modelo_indep$Informacion
    solucion_modelo_indep$Coeficientes

  # Modelo Saturado.

    modelo_sat <- MASS::loglm(~ DIMENSION * AUTOFINA * FJUR,
                              data= tab.originales2)

    generar_solucion(modelo_sat)
    solucion_modelo_sat$Informacion
    solucion_modelo_sat$Coeficientes

  # Elección del modelo final.

    modelo_def <- step(modelo_sat, scale = 0,
                       direction = c("backward"),
                       trace = 1, steps = 1000)

    generar_solucion(modelo_def)

    solucion_modelo_def$Informacion
    solucion_modelo_def$Coeficientes

# Fin del script :)