## Regresion multiple empresas eolicas. Disculpen la falta de tildes.

rm(list = ls())

# DATOS

  # Importando

    library(readxl)
    eolicos <- read_excel("eolica_50.xlsx", sheet = "Datos",
                          na = c("n.d.", "s.d."))
    eolicos <- data.frame(eolicos, row.names = 1)
    summary (eolicos)

  # Seleccionando variables clasificadoras para el analisis

    library(dplyr)
    originales<-select(eolicos,
                       RENECO,
                       RES,
                       ACTIVO,
                       ENDEUDA,
                       APALANCA,
                       DIMENSION)
    summary (originales)

  # Identificando missing values.

    library(visdat)
    vis_miss(originales)

    originales %>%
      filter(is.na(RENECO) | is.na(RES) | is.na(ACTIVO) |
             is.na(ENDEUDA) | is.na(APALANCA) | is.na(DIMENSION)) %>%
      select(RENECO, RES, ACTIVO, ENDEUDA, APALANCA, DIMENSION)  

    originales <- originales %>%
      filter(! is.na(RENECO) & ! is.na(RES) & ! is.na(ACTIVO) &
             ! is.na(ENDEUDA) & ! is.na(APALANCA) & ! is.na(DIMENSION))

  # Identificando outliers.

    originales <- originales %>%
      mutate(MAHALANOBIS = mahalanobis(select(.,
                                          RENECO,
                                          RES,
                                          ACTIVO,
                                          ENDEUDA,
                                          APALANCA),
                                        center = colMeans(select(.,
                                                            RENECO,
                                                            RES,
                                                            ACTIVO,
                                                            ENDEUDA,
                                                            APALANCA)),
                                        cov=cov(select(.,
                                                  RENECO,
                                                  RES,
                                                  ACTIVO,
                                                  ENDEUDA,
                                                  APALANCA))))

    library (ggplot2)
    ggplot(data = originales, map = (aes(y = MAHALANOBIS))) +
    geom_boxplot(fill = "orange") +
    ggtitle("DISTANCIA DE MAHALANOBIS", subtitle = "Empresas eólicas") +
    ylab("MAHALANOBIS")

    Q1M <- quantile (originales$MAHALANOBIS, c(0.25))
    Q3M <- quantile (originales$MAHALANOBIS, c(0.75))
    
    originales %>% filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
                      MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS)) %>%
                   select(MAHALANOBIS)
    originales_so <- originales %>%
                     filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
                            MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS)) 
    originales <- originales %>% select(-MAHALANOBIS)
    originales_so <- originales_so %>% select(-MAHALANOBIS)

  # Convertir variable DIMENSION en Factor.

    originales_so$DIMENSION <- as.factor(originales_so$DIMENSION)
    levels(originales_so$DIMENSION)

# ESPECIFICACION Y ESTIMACION

  # Cargar las librerías necesarias

    library (knitr)
    library (kableExtra)
    library (broom)
    library (car) # para obtener el vif

  # Especificar el modelo de regresión lineal
    ecua0 <- lm(data = originales_so,
                RENECO ~ RES + ACTIVO + ENDEUDA + APALANCA + DIMENSION)
    summary(ecua0)

  #diseña salida ordenador

    knitr.table.format = "html"

  # Definir la función de presentación de resultados:  presenta_modelo() #####

    presenta_modelo <- function(modelo) {

    # Lista de piezas
      modelo_piezas <-list()
  
    # Aplicar la función tidy() al modelo
      resultados <- tidy(modelo)
  
    # Seleccionar las columnas deseadas
      resultados <- resultados[, c("term",
                                   "estimate",
                                   "std.error","statistic",
                                   "p.value")]
    # Añadir la columna 'stars' según los valores de 'p.value'
      resultados$stars <- cut(resultados$p.value,
                            breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
                            labels = c("***", "**", "*", "·", " "),
                            right = FALSE)
    # formatear los valores de la columna "estimate" a 5 decimales
      resultados$estimate <- formatC(resultados$estimate,
                                     format = "f",
                                     digits = 5)
  
    # Crear la tabla con kable
      tabla1 <- resultados %>%
        kable(format = knitr.table.format,
          caption = "Modelo Lineal",
          col.names = c("Variable", "Coeficiente", "Desv. Típica",
                        "Estadístico t", "p-valor", "Sig."),
          digits = 3,
          align = c("l", "c", "c", "c", "c", "c")) %>%
        kable_styling(full_width = F,
                      bootstrap_options = "striped",
                                          "bordered",
                                          "condensed",
                      position = "center",
                      font_size = 11)
 
      modelo_piezas[[1]] <- tabla1
  
    # Aplicar la función glance
      estadisticos <- glance(modelo)
      estadisticos <- estadisticos[,c("r.squared",
                                      "adj.r.squared",
                                      "sigma",
                                      "statistic",
                                      "p.value",
                                      "AIC",
                                      "nobs")]
    # Crear la tabla con kable
      tabla2 <- estadisticos %>%
        kable(format = knitr.table.format,
          caption = "Estadísticos del modelo",
          col.names = c("R2", "R2 ajustado", "Sigma", "Estadístico F",
                        "p-valor", "AIC", "num. observaciones"),
          digits = 3,
          align = "c") %>%
        kable_styling(full_width = F,
                      bootstrap_options = "striped",
                                          "bordered",
                                          "condensed",
                      position = "center",
                      font_size = 11)
      
      modelo_piezas[[2]] <- tabla2  

    # Obtener VIF
      vif_df <- as.data.frame(vif(modelo))

    # Añadir nombres de filas
      library(tibble)
      vif_df <- vif_df %>%
      rownames_to_column(var = "Variable")
  
    # Crear tabla con kable
      tabla3 <- vif_df[,1:2] %>%
        kable(format = knitr.table.format,
              caption = "Factor de inflación de la varianza",
              col.names = c("Variable","Valor VIF"),
              digits = 3,
              align = "c") %>%
        kable_styling(full_width = F,
                      bootstrap_options = "striped",
                                          "bordered",
                                          "condensed",
                      position = "center",
                      font_size = 11)
      
      modelo_piezas[[3]] <- tabla3
  
    return(modelo_piezas)
  }
############################################################################

  modelo_0 <- presenta_modelo(ecua0)

  modelo_0[[1]]
  modelo_0[[2]]
  modelo_0[[3]]

  ## Prestar especial atencion a VIF para descartar variables que contribuyen
  ## a multicolinealidad.
  ## Si hay alguna variable con vif muy elevado, suprimir y reestimar.
  ## Cuando no existan variables con vif muy alto,
  ## pasar a obtener ecuacion con mayor AIC con el bloque siguiente:

  ecuaDEF <- step(ecua0, scale = 0,
                  direction = c("backward"),
                  trace = 1, steps = 1000, k = 2)
  summary (ecuaDEF)
  modelo_DEF <- presenta_modelo(ecuaDEF)

  modelo_DEF[[1]]
  modelo_DEF[[2]]
  modelo_DEF[[3]]

# MODELO FINAL: CONTRASTACIÓN.

  series_ecuaDEF <- augment(ecuaDEF)
  series_ecuaDEF <- series_ecuaDEF %>%
  rename(RENECO.est = .fitted,
         residuos = .resid,
         cooksd = .cooksd)
  series_ecuaDEF$ORDEN = c(1:nrow(series_ecuaDEF))
  summary (series_ecuaDEF)

  # Gráficos.
  
    g_real_pred <- ggplot(data = series_ecuaDEF) +
      geom_point(aes(x = ORDEN, y = RENECO.est),
                 size= 2,
                 alpha= 0.6,
                 color = "blue") +    
      geom_point(aes(x = ORDEN, y = RENECO),
                 size= 2,
                 alpha= 0.6,
                 color = "red") +
      geom_line(aes(x = ORDEN, y = RENECO.est),
                color = "blue",
                linetype = "dashed",
                size= 1) +
      geom_line(aes(x = ORDEN, y = RENECO),
                color = "red",
                linetype = "dashed",
                size= 1) +
      geom_segment(aes(x = ORDEN, xend = ORDEN, y = RENECO.est, yend = RENECO),
                   color = "orange") +
      ggtitle("RENTABILIDAD ECONÓMICA.",
              subtitle= "VALORES REALES (rojo) vs PREDICCIONES (azul).") +
      xlab("Casos") + 
      ylab("Rentabilidad Económica: Real y Predicción")

  g_resid <- ggplot(data = series_ecuaDEF, aes(x = ORDEN, y = residuos)) +
    geom_point(size=2, alpha= 0.6, color = "blue") +
    geom_smooth(color = "firebrick", span = 0.5) +
    geom_hline(yintercept = 0, color = "red")+
    ggtitle("RENTABILIDAD ECONÓMICA.", subtitle= "Residuos.")+
    xlab("Casos") + 
    ylab("Residuos")

  g_hresid <- ggplot(data = series_ecuaDEF, map = aes(x = residuos)) +
    geom_density(colour = "red", fill = "orange", alpha = 0.6) +
    ggtitle("RENTABILIDAD ECONÓMICA", subtitle = "Densidad Residuos")+
    xlab("Rentabilidad Económica") +
    ylab("Densidad")

  g_cook <- ggplot(data = series_ecuaDEF, aes(x = ORDEN, y = cooksd)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 4/nrow(series_ecuaDEF),
               linetype = "dashed",
               color = "red") +
    ggtitle("RENTABILIDAD ECONÓMICA.", subtitle= "Distancia de Cook.")+
    xlab("Casos") + 
    ylab("Distancias")

  library (patchwork)

  (g_real_pred | g_hresid) / (g_resid | g_cook)

  tablaCook <- series_ecuaDEF %>%
    filter ( cooksd > 4/nrow(series_ecuaDEF)) %>%
    select (.rownames, cooksd) %>%
    kable(format = knitr.table.format,
          caption = "Casos destacados distancia de Cook",
          col.names = c("Caso","Distancia de Cook"),
          digits = 3,
          align = c("l","c")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped",
                                      "bordered",
                                      "condensed",
                  position = "center",
                  font_size = 11)

    tablaCook

  # Hipótesis básicas MBR.

    library (lmtest)
    reset_test <- resettest(ecuaDEF, data= originales_so) # F. Funcional
    shapiro_test <- shapiro.test(series_ecuaDEF$residuos) # Normalidad
    bp_test <- bptest(ecuaDEF) # Homoscedasticidad

  # Tabla resultados

    # Crear un data frame con los resultados

      check_hipotesis <- data.frame(
        "Tipo_de_prueba" = c("Forma Funcional",
                             "Normalidad de perturbación aleatoria",
                             "Homoscedasticidad de perturbación aleatoria"),
        "Prueba" = c("Ramsey-Reset",
                     "Shapiro-Wilk",
                     "Breusch-Pagan"),
        "Estadistico" = c(reset_test$statistic,
                          shapiro_test$statistic,
                          bp_test$statistic),
        "P_valor" = c(reset_test$p.value,
                      shapiro_test$p.value,
                      bp_test$p.value),
        "Conclusion" = c(ifelse(reset_test$p.value >= 0.05,
                                 "F. funcional correcta",
                                 "F. funcional incorrecta"),
                         ifelse(shapiro_test$p.value >= 0.05,
                                "Normalidad",
                                "No-Normalidad"),
                         ifelse(bp_test$p.value >= 0.05,
                                "Homoscedasticidad",
                                "Heteroscedasticidad")))

    row.names(check_hipotesis) <- NULL

  # Crear la tabla con kable
    
    tabla_check <- check_hipotesis %>%
      kable(format = knitr.table.format,
            caption = "Contrastes de hipótesis del MBR",
            col.names = c("Tipo de prueba",
                          "Prueba",
                          "Estadístico",
                          "P-valor",
                          "Conclusión"),
            digits = 3,
            align = c("l", "c", "c", "c", "c")) %>%
      kable_styling(full_width = F,
                    bootstrap_options = "striped",
                                        "bordered",
                                        "condensed",
                    position = "center",
                    font_size = 11)

    tabla_check

# SIMULACIÓN

  # Cargar escenario de Excel

    escenario <- read_excel("eolica_escenarios.xlsx", sheet = "Simula")
    escenario <- data.frame(escenario, row.names = 1)
    escenario

  # Simulación con el modelo

    estimacion <-predict (object= ecuaDEF,
                          newdata = escenario,
                          interval="prediction",
                          level=0.95)
    estimacion
    simulacion <- cbind(escenario, estimacion)

  # Formatear las columnas con el número mínimo de decimales deseado

    simulacion$ENDEUDA <- format(simulacion$ENDEUDA, nsmall = 3)
    simulacion$APALANCA <- format(simulacion$APALANCA, nsmall = 3)
    simulacion$fit <- format(simulacion$fit, nsmall = 3)
    simulacion$lwr <- format(simulacion$lwr, nsmall = 3)
    simulacion$upr <- format(simulacion$upr, nsmall = 3)

  # Tabla

    tablasimula <- simulacion %>%
      kable(format = knitr.table.format,
            caption = "Simulación Modelo Rentabilidad Económica",
            col.names = c("Escenario",
                          "Resultado",
                          "Activo",
                          "Endeuda",
                          "Apalancamiento",
                          "Dimensión",
                          "Previsión",
                          "Inferior 95%",
                          "Superior 95%"),
        digits = 3) %>%
      kable_styling(full_width = F,
                    bootstrap_options = "striped",
                    "bordered",
                    "condensed",
                    position = "center",
                    font_size = 11) %>%
      row_spec(0, bold= T, align = "c") %>%
      row_spec(1:(nrow(simulacion)), bold= F, align = "c")

  tablasimula

# Fin del script :)  