### REGRESIÓN MÚLTIPLE. Empresas TMI. ###

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
library (broom)
library (car) # para obtener el vif
library (lmtest)

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

##### Definir la función de presentación de resultados:  presenta_modelo() #####

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
#### Predicción por escenarios (valores o fórmulas con df$var)  ############
#### =========================================================  ############
#### Requisitos: readxl, dplyr, stringr, purrr                  ############
#### (kableExtra es opcional, solo se quiere tabla formateada)  ############
############################################################################

predict_from_excel_scenarios <- function(model,
                                         train_df,
                                         excel_path,
                                         sheet = "Escenarios",
                                         id_col = "ESCENARIO",
                                         eval_dfs = list(train = NULL),
                                         interval = c("prediction","confidence"),
                                         level = 0.95,
                                         transforma_log = FALSE,
                                         return_kable = TRUE,
                                         kable_caption = "Predicciones por escenario",
                                         kable_digits = 3) {
  
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Falta 'readxl'")
  if (!requireNamespace("dplyr",  quietly = TRUE)) stop("Falta 'dplyr'")
  if (return_kable && !requireNamespace("kableExtra", quietly = TRUE)) stop("Falta 'kableExtra'")
  
  interval <- match.arg(interval)
  
  form   <- stats::formula(model)
  y_name <- all.vars(form)[1]
  vars_x <- setdiff(all.vars(form), y_name)
  
  escenarios_raw <- readxl::read_excel(excel_path, sheet = sheet)
  escenarios_raw <- as.data.frame(escenarios_raw, check.names = FALSE)
  if (!(id_col %in% names(escenarios_raw))) {
    escenarios_raw[[id_col]] <- seq_len(nrow(escenarios_raw))
  }
  
  parece_num <- function(x_chr) grepl("^\\s*-?\\d+(?:[\\.,]\\d+)?\\s*$", x_chr)
  
  evalua_celda <- function(x, env_eval) {
    if (is.numeric(x)) return(x)
    if (is.na(x)) return(NA_real_)
    xc <- as.character(x)
    
    if (parece_num(xc)) return(as.numeric(gsub(",", ".", xc)))
    
    es_formula <- grepl("[\\(\\)\\+\\-\\*/\\^:\\$]", xc)
    if (!es_formula) {
      xc <- gsub("^\\s*['\"]|['\"]\\s*$", "", xc)
      return(xc)
    }
    
    expr_txt <- gsub("(\\d),(\\d)", "\\1.\\2", xc) # 2,5 -> 2.5
    val <- try(eval(parse(text = expr_txt), envir = env_eval), silent = TRUE)
    if (inherits(val, "try-error")) {
      stop(paste0("No se pudo evaluar la fórmula: ", xc,
                  "\nTransformada como: ", expr_txt,
                  "\nError: ", as.character(val)))
    }
    if (length(val) > 1) val <- mean(val, na.rm = TRUE)
    as.numeric(val)
  }
  
  # Entorno de evaluación para las fórmulas de Excel
  env_eval <- new.env(parent = .GlobalEnv)
  
  if (length(eval_dfs)) {
    nms <- names(eval_dfs)
    for (nm in nms) {
      if (!is.null(eval_dfs[[nm]])) assign(nm, eval_dfs[[nm]], envir = env_eval)
    }
  }
  if (is.null(eval_dfs$train)) assign("train", train_df, envir = env_eval)
  
  for (vn in names(train_df)) assign(vn, train_df[[vn]], envir = env_eval)
  
  factor_levels <- lapply(intersect(vars_x, names(train_df)), function(vn) {
    if (is.factor(train_df[[vn]])) levels(train_df[[vn]]) else NULL
  })
  names(factor_levels) <- intersect(vars_x, names(train_df))
  
  split_list <- split(escenarios_raw, escenarios_raw[[id_col]], drop = TRUE)
  
  newdatas_list <- lapply(names(split_list), function(id) {
    fila <- split_list[[id]][1, , drop = FALSE]
    nd_vals <- vector("list", length(vars_x)); names(nd_vals) <- vars_x
    for (vn in vars_x) {
      if (!(vn %in% names(fila))) {
        stop(paste0("Falta la columna '", vn, "' en la hoja '", sheet, "'."))
      }
      celda <- fila[[vn]][[1]]
      
      if (vn %in% names(train_df) && is.factor(train_df[[vn]])) {
        v <- evalua_celda(celda, env_eval)
        nd_vals[[vn]] <- factor(v, levels = factor_levels[[vn]])
      } else {
        nd_vals[[vn]] <- evalua_celda(celda, env_eval)
      }
    }
    nd <- as.data.frame(nd_vals, check.names = FALSE)
    nd[[id_col]] <- id
    nd
  })
  
  newdatas <- do.call(rbind, newdatas_list)
  rownames(newdatas) <- NULL
  
  preds <- stats::predict(model,
                          newdata = newdatas[, vars_x, drop = FALSE],
                          interval = interval, level = level, se.fit = TRUE)
  
  fit_mat <- as.data.frame(preds$fit); names(fit_mat) <- c("fit","lwr","upr")
  se_vec  <- as.numeric(preds$se.fit)
  
  # --- Transformación antilog si corresponde ---
  if (transforma_log) {
    fitted_vals <- exp(fit_mat$fit)
    lwr_vals    <- exp(fit_mat$lwr)
    upr_vals    <- exp(fit_mat$upr)
    # Método delta aproximado para la desviación típica en el nivel original
    se_vals     <- exp(fit_mat$fit) * se_vec
  } else {
    fitted_vals <- as.numeric(fit_mat$fit)
    lwr_vals    <- as.numeric(fit_mat$lwr)
    upr_vals    <- as.numeric(fit_mat$upr)
    se_vals     <- se_vec
  }
  
  out_df <- cbind(
    newdatas[, c(id_col, vars_x), drop = FALSE],
    .fitted   = fitted_vals,
    .lwr      = lwr_vals,
    .upr      = upr_vals,
    .se_fit   = se_vals,
    .interval = interval,
    .level    = level,
    .log_transform = transforma_log
  )
  
  if (return_kable) {
    knitr.table.format <- "html"
    tab <- kableExtra::kbl(
      out_df,
      format = knitr.table.format,
      caption = paste0(
        kable_caption,
        " — Variable dependiente: ", y_name,
        " (", interval, " ", level*100, "%)",
        if (transforma_log) " — valores antilog."
      ),
      digits = kable_digits,
      align = "c"
    ) |>
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped","bordered","condensed"),
        position = "center",
        font_size = 11
      )
  } else {
    tab <- NULL
  }
  
  list(data = out_df, table = tab)
}

###############################################################################

## DATOS

# Importando datos desde Excel
interestelar_300 <- read_excel("interestelar_300.xlsx",
                               sheet = "Datos",
                               na = c("n.d."))
interestelar_300 <- data.frame(interestelar_300, row.names = 1)

# Seleccionando variables para el analisis.
seleccion <- interestelar_300 %>%
  select(BMAL,
         DIST,
         IDIVERSE,
         ACTIVO,
         LIQUIDEZ,
         EFLO)
seleccion_df_graph <- gt_plt_summary(seleccion)
seleccion_df_graph

# Localizando missing values.
seleccion %>%
  vis_miss() +
  labs(title = "Modelo de regresión: Beneficio por mil años luz recorridos (BMAL).",
       subtitle = "Transporte de mercancías interestelar",
       y = "Observación",
       fill = NULL) +
  scale_fill_manual(
    values = c("TRUE" = "red", "FALSE" = "grey"),
    labels = c("TRUE" = "NA", "FALSE" = "Presente")) +
  theme(
    plot.title = element_text(face = "bold", size = 14))

seleccion %>% filter(is.na(BMAL) |
                     is.na(DIST) |
                     is.na(IDIVERSE) |
                     is.na(ACTIVO) |
                     is.na(LIQUIDEZ) |
                     is.na(EFLO))%>%
               select(BMAL,
                      DIST,
                      IDIVERSE,
                      ACTIVO,
                      LIQUIDEZ,
                      EFLO)
seleccion <- seleccion %>%
  filter(! is.na(BMAL) &
         ! is.na(DIST) &
         ! is.na(IDIVERSE) &
         ! is.na(ACTIVO) &
         ! is.na(LIQUIDEZ) &
         ! is.na(EFLO)) 

# Identificando outliers con distancia de Mahalanobis.
seleccion <- seleccion %>%
  mutate(
    MAHALANOBIS = mahalanobis(
      x      = as.matrix(pick(where(is.numeric))),
      center = colMeans(pick(where(is.numeric))),
      cov    = cov(pick(where(is.numeric)))
    )
  )

ggplot(data = seleccion, map = (aes(y = MAHALANOBIS))) +
  geom_boxplot(fill = "orange") +
  ggtitle("DISTANCIA DE MAHALANOBIS",
          subtitle = "BMAL, DIST, IDIVERSE, ACTIVO, LIQUIDEZ. Empresas TMI.") +
  ylab("MAHALANOBIS")

Q1M <- quantile (seleccion$MAHALANOBIS, c(0.25))                #!
Q3M <- quantile (seleccion$MAHALANOBIS, c(0.75))                #!

seleccion %>%
  filter(MAHALANOBIS > Q3M + 1.5*IQR(MAHALANOBIS) |
           MAHALANOBIS < Q1M - 1.5*IQR(MAHALANOBIS))%>%
  select(MAHALANOBIS, BMAL, DIST, IDIVERSE, ACTIVO, LIQUIDEZ)

# Eliminando outliers.
seleccion_so <-seleccion %>%
  filter(MAHALANOBIS <= Q3M + 1.5*IQR(MAHALANOBIS) &
           MAHALANOBIS >= Q1M - 1.5*IQR(MAHALANOBIS))

# Eliminando variable MAHALANOBIS de los df
seleccion <- seleccion %>% select(-MAHALANOBIS)
seleccion_so <- seleccion_so %>% select(-MAHALANOBIS)

# Convertir variable EFLO en Factor.

seleccion_so$EFLO <- as.factor(seleccion_so$EFLO)
  levels(seleccion_so$EFLO)

## ESPECIFICACION Y ESTIMACION

# Especificar el modelo de regresión lineal
    ecua0 <- lm(data = seleccion_so,
                log(BMAL) ~ 
                  log(DIST) +
                  log(IDIVERSE) +
                  log(ACTIVO) +
                  log(LIQUIDEZ) +
                  EFLO)
    summary(ecua0)

# Diseña salida ordenador y presentar
    
  knitr.table.format = "html"

  modelo_0 <- presenta_modelo(ecua0)

  modelo_0[[1]]
  modelo_0[[2]]
  modelo_0[[3]]

  ## Prestar especial atencion a VIF para descartar variables que contribuyen
  ## a multicolinealidad.
  ## Si hay alguna variable con vif muy elevado, suprimir y reestimar.
  
  ecua1 <- lm(data = seleccion_so,
              log(BMAL) ~ 
                log(DIST) +
                log(IDIVERSE) +
# ACTIVO elim.  log(ACTIVO) + 
                log(LIQUIDEZ) +
                EFLO)
  summary(ecua1)
  
  modelo_1 <- presenta_modelo(ecua1)
  
  modelo_1[[1]]
  modelo_1[[2]]
  modelo_1[[3]]
  
  ## Cuando no existan variables con vif muy alto,
  ## pasar a obtener ecuación con mayor AIC con el bloque siguiente:

  ecuaDEF <- step(ecua1, scale = 0,
                  direction = c("backward"),
                  trace = 1, steps = 1000, k = 2)
  summary (ecuaDEF)
  modelo_DEF <- presenta_modelo(ecuaDEF)

  modelo_DEF[[1]]
  modelo_DEF[[2]]
  modelo_DEF[[3]]

# MODELO FINAL: CONTRASTACIÓN.

  # Generar series útiles.
  
  series_ecuaDEF <- augment(ecuaDEF, seleccion_so)
  series_ecuaDEF <- series_ecuaDEF %>%
    rename(logBMAL.est = .fitted,
           residuos = .resid,
           cooksd = .cooksd)
  series_ecuaDEF$ORDEN = c(1:nrow(series_ecuaDEF))
  summary (series_ecuaDEF)

  # Gráficos.
  
  g_real_pred <- ggplot(series_ecuaDEF) +
    geom_point(aes(x = reorder(ORDEN, abs(residuos)), y = logBMAL.est),
               size = 2, alpha = 0.6, color = "blue") +
    geom_point(aes(x = reorder(ORDEN, abs(residuos)), y = log(BMAL)),
               size = 2, alpha = 0.6, color = "red") +
    geom_segment(aes(
      x = reorder(ORDEN, abs(residuos)),
      xend = reorder(ORDEN, abs(residuos)),
      y = logBMAL.est, yend = log(BMAL)),
      color = "navy"
    ) +
    ggtitle("BMAL empresas TMI.",
            subtitle = "VALORES REALES (rojo) vs PREDICCIONES (azul).") +
    xlab("Casos (ordenados por |residuo|)") +
    ylab("Resultado: Real y Predicción")

  g_resid <- ggplot(
    data = series_ecuaDEF,
    aes(x = reorder(ORDEN, abs(residuos)), y = residuos)
  ) +
    geom_point(size = 2, alpha = 0.6, color = "blue") +
    geom_hline(yintercept = 0, color = "red") +
    ggtitle("BMAL empresas TMI.", subtitle = "Residuos (ordenados por |residuo|).") +
    xlab("Casos (ordenados por |residuo|)") +
    ylab("Residuos")
  
  g_hresid <- ggplot(data = series_ecuaDEF, map = aes(x = residuos)) +
    geom_density(colour = "red", fill = "orange", alpha = 0.6) +
    ggtitle("BMAL empresas TMI.", subtitle = "Densidad Residuos")+
    xlab("Residuos") +
    ylab("Densidad")

  g_cook <- ggplot(data = series_ecuaDEF, aes(x = ORDEN, y = cooksd)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 4/nrow(series_ecuaDEF),
               linetype = "dashed",
               color = "red") +
    ggtitle("BMAL empresas TMI.", subtitle= "Distancia de Cook.")+
    xlab("Casos") + 
    ylab("Distancias")

  (g_real_pred | g_resid) / (g_hresid | g_cook)

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

    reset_test <- resettest(ecuaDEF, data= seleccion_so) # F. Funcional
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
    
    res_Simula1 <- predict_from_excel_scenarios(
      model        = ecuaDEF,
      train_df     = seleccion_so,
      excel_path   = "escenarios_rstars.xlsx",
      sheet        = "Simula1",
      id_col       = "ESCENARIO",
      eval_dfs     = list(
        train = seleccion_so,
        df1   = seleccion        # para usar df1$variable en Excel
      ),
      interval       = "prediction",
      level          = 0.95,
      transforma_log = TRUE,
      return_kable   = TRUE
    )

    res_Simula1[["table"]]
    
    res_Simula2 <- predict_from_excel_scenarios(
      model        = ecuaDEF,
      train_df     = seleccion_so,
      excel_path   = "escenarios_rstars.xlsx",
      sheet        = "Simula2",
      id_col       = "ESCENARIO",
      eval_dfs     = list(
        train = seleccion_so,
        df1   = seleccion        # para usar df1$variable en Excel
      ),
      interval       = "prediction",
      level          = 0.95,
      transforma_log = TRUE,
      return_kable   = TRUE
    )

    res_Simula2[["table"]]
        
# Fin del script :)  
    