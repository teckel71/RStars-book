#!/usr/bin/env Rscript
# =====================================================================
# numerar_tablas_figuras.R  (v2)
# ---------------------------------------------------------------------
# Postprocesador de ficheros .Rmd para numerar tablas y figuras
# manualmente y de forma consistente.
#
# Reglas resumidas:
#   - Las TABLAS se numeran con "**Tabla X.Y.** <descripción opcional>"
#     centrado debajo, donde X = nº de capítulo, Y = orden en el cap.
#   - Las FIGURAS con "**Figura X.Y.** <descripción opcional>" igual.
#   - Soporta:
#       * Llamadas estáticas a kable / kbl / kable_rstars / gt /
#         gt_plt_summary / flextable / datatable (con o sin condicional
#         html/docx/pdf).
#       * Imágenes markdown ![alt](ruta) (excluyendo iconos .hicon).
#       * Llamadas a ggplot / vis_miss / ggpairs / grid.arrange /
#         patchwork::wrap_plots / fviz_* / plot / barplot / hist.
#       * Tablas markdown manuales (líneas con | + separador |---|).
#       * Chunks mixtos: si tienen p.ej. una tabla y dos figuras, emite
#         los tres bloques de numeración en orden tras el chunk.
#   - NO numera (deja que bookdown haga la numeración automática):
#       * Chunks que contienen un bucle (for/while/lapply/sapply/map*).
#         Además, intenta quitar los `caption = ...` de esos bucles
#         para que bookdown tampoco genere doble numeración rara.
#   - NO toca:
#       * Iconos .hicon en titulares.
#       * Chunks con eval = FALSE (código pedagógico).
#       * Definiciones de función dentro de chunks.
#
# Uso:
#   Rscript numerar_tablas_figuras.R entrada.Rmd [salida.Rmd] [num_cap]
#   procesar_rmd("foo.Rmd")                                       # desde R
# =====================================================================

procesar_rmd <- function(ruta_entrada,
                        ruta_salida = NULL,
                        num_capitulo = NULL,
                        incluir_caption_tabla = TRUE,
                        incluir_alt_figura    = TRUE,
                        envolver_alt_simple   = TRUE,
                        verbose = TRUE) {

  # ===================================================================
  # 1. Configuración y lectura
  # ===================================================================
  if (is.null(num_capitulo)) {
    nombre <- basename(ruta_entrada)
    m <- regmatches(nombre, regexpr("^[0-9]+", nombre))
    if (length(m) == 0) {
      stop("No se pudo inferir el número de capítulo del nombre del ",
           "fichero. Pase num_capitulo explícitamente.")
    }
    num_capitulo <- as.integer(m)
  }
  if (is.null(ruta_salida)) {
    base <- tools::file_path_sans_ext(ruta_entrada)
    ext  <- tools::file_ext(ruta_entrada)
    ruta_salida <- paste0(base, "_numerado.", ext)
  }

  raw <- readChar(ruta_entrada, file.info(ruta_entrada)$size, useBytes = TRUE)
  eol_original <- if (grepl("\r\n", raw, fixed = TRUE)) "\r\n" else "\n"
  lineas <- readLines(ruta_entrada, warn = FALSE, encoding = "UTF-8")
  lineas <- gsub("\r$", "", lineas)

  # Pre-procesado: unir imágenes markdown que vienen partidas en varias
  # líneas. Patrón: línea que contiene `![[...` o `![...` sin cerrar el
  # paréntesis final. Las juntamos con las siguientes hasta encontrar `)`.
  unir_imagenes_multilinea <- function(lns) {
    if (length(lns) == 0L) return(lns)
    resultado <- character(0)
    i <- 1L
    while (i <= length(lns)) {
      linea <- lns[i]
      tiene_inicio <- grepl("!\\[", linea, perl = TRUE)
      if (tiene_inicio) {
        # ¿Tiene `)` después del `![`? Si sí, está completa.
        pos_corchete <- regexpr("!\\[", linea, perl = TRUE)
        substr_tras <- substr(linea, as.integer(pos_corchete),
                              nchar(linea))
        if (!grepl("\\)", substr_tras, perl = TRUE) && i < length(lns)) {
          # Continúa: ir uniendo líneas hasta encontrar `)`
          unida <- linea
          j <- i + 1L
          while (j <= length(lns) && !grepl("\\)", unida, perl = TRUE)) {
            # Une con espacio (no \n) para que las regex funcionen
            unida <- paste(unida, lns[j])
            j <- j + 1L
          }
          # Solo unir si REALMENTE encontramos el `)`
          if (grepl("\\)", unida, perl = TRUE)) {
            resultado <- c(resultado, unida)
            i <- j
            next
          }
        }
      }
      resultado <- c(resultado, linea)
      i <- i + 1L
    }
    resultado
  }
  lineas <- unir_imagenes_multilinea(lineas)

  # ===================================================================
  # 2. Patrones (constantes)
  # ===================================================================
  re_func_tabla <- paste(
    "\\bkable\\s*\\(",
    "\\bkbl\\s*\\(",
    "\\bkable_rstars\\s*\\(",
    "\\bgt_plt_summary\\s*\\(",
    "\\bflextable\\s*\\(",
    "\\bdatatable\\s*\\(",
    "\\bgt\\s*\\(",
    "\\bpander\\s*\\(",
    "\\bpander_return\\s*\\(",
    "\\bpandoc\\.table\\s*\\(",
    sep = "|")
  re_func_figura <- paste(
    "\\bggplot\\s*\\(",
    "\\bvis_miss\\s*\\(",
    "\\bggpairs\\s*\\(",
    "\\bggarrange\\s*\\(",
    "\\bggMarginal\\s*\\(",
    "\\bgrid\\.arrange\\s*\\(",
    "\\bgrid\\.draw\\s*\\(",
    "\\bplot_annotation\\s*\\(",
    "\\bwrap_plots\\s*\\(",
    "\\bcreate_patchwork\\s*\\(",   # función auxiliar del usuario
    "\\bfviz_[a-z_]+\\s*\\(",
    "\\bautoplot\\s*\\(",
    "(?<![._a-zA-Z0-9])plot\\s*\\(",
    "\\bbarplot\\s*\\(",
    "\\bboxplot\\s*\\(",
    "\\bhist\\s*\\(",
    "\\bpairs\\s*\\(",
    "\\bmosaicplot\\s*\\(",
    "\\bmosaic\\s*\\(",
    "\\bcorrplot\\s*\\(",
    "\\bassoc\\s*\\(",
    sep = "|")
  re_func_bucle <- paste(
    "\\bfor\\s*\\(",
    "\\bwhile\\s*\\(",
    "\\blapply\\s*\\(",
    "\\bsapply\\s*\\(",
    "\\bvapply\\s*\\(",
    "\\bmapply\\s*\\(",
    "\\bmap[a-z_]*\\s*\\(",
    "\\bwalk[a-z_]*\\s*\\(",
    sep = "|")

  re_chunk_inicio <- "^```\\s*\\{r[^}]*\\}\\s*$"
  re_chunk_fin    <- "^```\\s*$"
  re_heading      <- "^#"

  re_cap_lit      <- 'caption\\s*=\\s*"([^"]*)"\\s*,?\\s*'

  re_tabla_md_sep <- "^\\|[[:space:]\\-:|]+\\|[[:space:]]*$"
  re_tabla_md_row <- "^\\|.*\\|[[:space:]]*$"

  # ===================================================================
  # 2b. Conocimiento hardcoded sobre funciones del paquete MATrstars
  # ===================================================================
  # Estas funciones producen internamente tablas con caption fijo cuando
  # se llaman con sus argumentos por defecto. El preprocesador se ejecuta
  # antes del knit y no puede "ver" el runtime del paquete, así que para
  # preservar el texto descriptivo bajo las etiquetas manuales
  # ("**Tabla X.Y. <descripción>**") es necesario tener aquí las captions
  # que las funciones emiten por defecto.
  #
  # Si el usuario invoca alguna de estas funciones sobrescribiendo el
  # argumento `captions` (o su equivalente), estas etiquetas seguirán
  # apareciendo con los textos por defecto — mejor eso que dejarlas
  # vacías. Para captions dinámicas (predict_from_excel_scenarios), se
  # usa el texto base sin la decoración runtime.
  FUNCIONES_PAQUETE_MATRSTARS <- list(
    generar_solucion = list(
      figures = 1L,
      captions_by_field = c(
        Informacion  = "Validaci\u00f3n del modelo",
        Coeficientes = "Coeficientes del modelo"
      ),
      captions_by_index = c(
        "Validaci\u00f3n del modelo",
        "Coeficientes del modelo"
      )
    ),
    presenta_modelo = list(
      figures = 0L,
      captions_by_field = c(
        coef  = "Modelo Lineal",
        stats = "Estad\u00edsticos del modelo",
        vif   = "Factor de inflaci\u00f3n de la varianza"
      ),
      captions_by_index = c(
        "Modelo Lineal",
        "Estad\u00edsticos del modelo",
        "Factor de inflaci\u00f3n de la varianza"
      )
    ),
    predict_from_excel_scenarios = list(
      figures = 0L,
      captions_by_field = c(
        table = "Predicciones por escenario"
      ),
      captions_by_index = c(
        NA_character_,
        "Predicciones por escenario"
      )
    ),
    presenta_logit = list(
      figures = 0L,
      captions_by_field = c(
        coef = "Modelo Logit Binomial",
        gof  = "Bondad del ajuste",
        or   = "Odd ratios",
        conf = "Matriz de confusi\u00f3n"
      ),
      captions_by_index = c(
        "Modelo Logit Binomial",
        "Bondad del ajuste",
        "Odd ratios",
        "Matriz de confusi\u00f3n"
      )
    )
  )

  # -------------------------------------------------------------------
  # 2c. Funciones "side-effect" del paquete MATrstars
  # -------------------------------------------------------------------
  # Estas funciones, al ejecutarse en un chunk, emiten internamente una
  # secuencia FIJA de figuras y tablas via print()/cat(). El
  # preprocesador tiene que garantizar el patron:
  #
  #     [figura]
  #     **Figura X.Y**
  #     [tabla]
  #     **Tabla X.Z**
  #
  # que es el patron estandar del libro. Como el gráfico y la tabla se
  # emiten desde dentro de la funcion (en la misma linea de codigo), no
  # es posible dividir el chunk para intercalar las etiquetas. La
  # solucion es que la propia funcion emita las etiquetas justo debajo
  # de cada elemento, leyendo el numero X.Y que le corresponde de una
  # opcion de sesion (`matrstars.<tipo>_num`) que este preprocesador
  # inyecta al principio del chunk.
  #
  # Cada entrada declara la secuencia de tipos ("figura", "tabla") que
  # produce la funcion en el orden en que los emite. Los tipos "figura"
  # incrementan el contador de figuras del capitulo, y los tipos
  # "tabla" el de tablas; los numeros correspondientes se inyectan como
  # opciones. Los chunks con estas funciones NO reciben etiquetas al
  # final: las emite la propia funcion en su lugar exacto.
  FUNCIONES_PAQUETE_MATRSTARS_SIDEEFFECT <- list(
    explora_na       = c("figura", "tabla"),
    explora_outliers = c("figura", "tabla")
  )
  re_sideeffect <- paste0(
    "\\b(?:",
    paste(names(FUNCIONES_PAQUETE_MATRSTARS_SIDEEFFECT), collapse = "|"),
    ")\\s*\\("
  )

  # ===================================================================
  # 3. Helpers
  # ===================================================================
  bloque_numeracion <- function(tipo, n_cap, n, descripcion = NULL) {
    etiqueta <- if (tipo == "tabla") "Tabla" else "Figura"
    cabecera <- paste0("**", etiqueta, " ", n_cap, ".", n, "**")
    if (!is.null(descripcion) && nzchar(trimws(descripcion))) {
      cabecera <- paste0("**", etiqueta, " ", n_cap, ".", n, ".** ",
                         trimws(descripcion))
    }
    c("",
      "::: {style=\"text-align: center;\"}",
      cabecera,
      ":::",
      "")
  }

  es_chunk_inicio <- function(linea) grepl(re_chunk_inicio, linea, perl = TRUE)
  es_chunk_fin    <- function(linea) grepl(re_chunk_fin,    linea, perl = TRUE)
  es_eval_false   <- function(h) grepl("eval\\s*=\\s*F(ALSE)?\\b", h, perl = TRUE)
  es_include_false <- function(h) grepl("include\\s*=\\s*F(ALSE)?\\b", h, perl = TRUE)
  es_titular      <- function(l) grepl(re_heading, l, perl = TRUE)

  # -------------------------------------------------------------------
  # 3.1. Analizar contenido de un chunk
  # -------------------------------------------------------------------
  # Devuelve list(secuencia, captions, posiciones, tiene_bucle,
  #               contenedores, diferidos_tentativos).
  # Recibe contenedores y diferidos tentativos iniciales (estado global
  # heredado de chunks previos en el mismo documento). Esto permite
  # capturar patrones como `var <- f(...)` en un chunk y `var[[N]]` en
  # otro posterior.
  analizar_chunk <- function(buffer,
                             contenedores_iniciales = list(),
                             diferidos_tentativos_iniciales = list(),
                             capciones_iniciales = list()) {
    en_def         <- FALSE
    cuerpo_abierto <- FALSE
    nivel_llaves   <- 0L
    tiene_bucle    <- FALSE

    seq_tipo      <- character(0)
    seq_cap       <- character(0)
    seq_posicion  <- integer(0)

    # Secuencia de tipos ("figura"/"tabla") producidos por llamadas a
    # funciones "side-effect" del paquete MATrstars detectadas en el
    # chunk. Se acumula APARTE de `seq_tipo` porque estos elementos NO
    # se etiquetan al final del chunk; sus etiquetas las emite la
    # propia funcion en la posicion correcta, usando los numeros que
    # este preprocesador le inyecta como opciones.
    sideeffect_seq <- character(0)

    diferidos    <- list()        # tipo conocido (kable, ggplot, etc.)
    diferidos_tentativos <- diferidos_tentativos_iniciales
    contenedores <- contenedores_iniciales
    # Paralelo a `contenedores`: para variables cuyo contenido conocemos
    # (porque provienen de una función del paquete MATrstars que
    # figura en `FUNCIONES_PAQUETE_MATRSTARS`), almacenamos aquí las
    # captions esperables por campo/índice.
    capciones_conocidas <- capciones_iniciales

    en_kable <- FALSE

    # Asignación multi-línea: si vemos `var <- expr_que_continua_en_lineas
    # siguientes`, marcamos `var` como receptora y asociamos las
    # subsiguientes llamadas a kable/ggplot a `var` (en lugar de
    # contarlas como elementos directos del chunk).
    asig_multi_var   <- NA_character_
    asig_multi_paren <- 0L
    asig_multi_primera_func <- NA_character_  # primera función llamada
                                              # en la asignación multi

    # Funciones que NO marcan tentativo (asignación trivial de
    # estructura, no resultado de "presentación" de modelo o similar).
    lista_negra_funcs <- c(
      "read_excel", "read.csv", "read_csv", "read_tsv", "read.delim",
      "read.table", "readRDS", "read_rds", "load",
      "data.frame", "tibble", "as.data.frame", "as_tibble",
      "list", "c", "vector", "numeric", "character", "logical",
      "factor", "as.factor", "as.numeric", "as.character",
      "as.integer", "as.logical", "as.Date",
      "paste", "paste0", "sprintf", "format", "formatC",
      "seq", "seq_len", "seq_along", "rep",
      "length", "nrow", "ncol", "dim",
      "matrix", "array",
      "max", "min", "sum", "mean", "median", "sd", "var",
      "round", "floor", "ceiling",
      "names", "colnames", "rownames",
      "subset", "filter", "select", "mutate", "summarize", "summarise",
      "group_by", "arrange",
      "attr", "attributes",
      "xtabs", "table"
    )
    es_funcion_lista_negra <- function(fname) {
      if (is.na(fname) || !nzchar(fname)) return(FALSE)
      fname %in% lista_negra_funcs
    }  # nivel de paréntesis sin cerrar

    re_terminal      <- "\\b(?:create_patchwork|wrap_plots)\\s*\\("
    re_asig_inicio   <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*(<-|=)\\s*"
    re_asig_lista    <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\[\\[[^\\]]+\\]\\]\\s*(<-|=)\\s*"
    re_asig_lista_rhs <- paste0(
      "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\[\\[[^\\]]+\\]\\]\\s*",
      "(?:<-|=)\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*$")
    re_solo_nombre   <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*$"
    re_solo_lista    <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\[\\[[^\\]]+\\]\\]\\s*$"
    re_solo_dollar   <- "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\$\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*$"
    re_print_var     <- "\\bprint\\s*\\(\\s*([A-Za-z_.][A-Za-z_.0-9]*)"
    # Patchwork inline: línea con SOLO nombres unidos por +, /, |
    # (sin paréntesis con argumentos, sin números, sin strings).
    # Acepta paréntesis de agrupación: (g1 | g2) / (g3 | g4)
    re_patchwork_inline <- "^[\\s()A-Za-z_.0-9+/|]+$"
    # Una línea "continúa" si termina con %>%, +, |, ,, o paréntesis abierto
    re_continua      <- "(?:%>%|\\+|\\||,|<-|=)\\s*$"

    # Helper: cuenta caracteres específicos en una cadena
    contar_ch <- function(txt, ch) {
      sum(strsplit(txt, "", fixed = TRUE)[[1]] == ch)
    }

    for (i in seq_along(buffer)) {
      linea <- buffer[i]

      # ¿Abre una nueva definición de función?
      if (grepl("<-\\s*function\\s*\\(", linea, perl = TRUE)) {
        en_def         <- TRUE
        cuerpo_abierto <- FALSE
        nivel_llaves   <- 0L
        en_kable       <- FALSE
        asig_multi_var   <- NA_character_
        asig_multi_paren <- 0L
      }

      if (en_def) {
        n_abre   <- contar_ch(linea, "{")
        n_cierra <- contar_ch(linea, "}")
        if (!cuerpo_abierto) {
          if (n_abre > 0L) {
            cuerpo_abierto <- TRUE
            nivel_llaves   <- n_abre - n_cierra
            if (nivel_llaves <= 0L) {
              en_def         <- FALSE
              cuerpo_abierto <- FALSE
              nivel_llaves   <- 0L
            }
          }
        } else {
          nivel_llaves <- nivel_llaves + n_abre - n_cierra
          if (nivel_llaves <= 0L) {
            en_def         <- FALSE
            cuerpo_abierto <- FALSE
            nivel_llaves   <- 0L
          }
        }
        next
      }

      if (grepl(re_func_bucle, linea, perl = TRUE)) tiene_bucle <- TRUE

      tiene_terminal <- grepl(re_terminal, linea, perl = TRUE)

      # --- Deteccion de funciones "side-effect" del paquete MATrstars ---
      # Estas funciones emiten internamente una secuencia fija de
      # figuras/tablas. Se acumulan en `sideeffect_seq` (aparte de
      # `seq_tipo`) para que el bucle principal las trate de forma
      # especial: inyectar opciones al inicio del chunk y NO emitir
      # etiquetas al final.
      if (grepl(re_sideeffect, linea, perl = TRUE)) {
        m_se <- regmatches(linea,
                           regexpr(re_sideeffect, linea, perl = TRUE))
        fname_se <- sub("\\s*\\(\\s*$", "", m_se)
        elementos <- FUNCIONES_PAQUETE_MATRSTARS_SIDEEFFECT[[fname_se]]
        if (!is.null(elementos)) {
          sideeffect_seq <- c(sideeffect_seq, elementos)
          # Si esta llamada es RHS de una asignacion multi-linea,
          # marcar la variable como "ya procesada" para que al cerrar
          # los parentesis no se anada como diferido tentativo.
          m_asig_se <- regexec(re_asig_inicio, linea, perl = TRUE)
          r_asig_se <- regmatches(linea, m_asig_se)[[1]]
          if (length(r_asig_se) >= 2) {
            n_p_abre  <- contar_ch(linea, "(")
            n_p_cierr <- contar_ch(linea, ")")
            if (grepl(re_continua, linea, perl = TRUE) ||
                n_p_abre > n_p_cierr) {
              asig_multi_var          <- r_asig_se[2]
              asig_multi_paren        <- n_p_abre - n_p_cierr
              asig_multi_primera_func <- "__SIDEEFFECT_CONSUMED__"
            }
          }
          next
        }
      }

      # --- Verificar si seguimos en asignación multi-línea ---
      if (!is.na(asig_multi_var)) {
        # Caso especial: la asignacion fue iniciada por una funcion
        # side-effect ya consumida. Solo actualizamos el balance de
        # parentesis y esperamos el cierre, sin anadir nada mas.
        if (identical(asig_multi_primera_func,
                      "__SIDEEFFECT_CONSUMED__")) {
          asig_multi_paren <- asig_multi_paren +
                              contar_ch(linea, "(") - contar_ch(linea, ")")
          if (asig_multi_paren <= 0L &&
              !grepl(re_continua, linea, perl = TRUE)) {
            asig_multi_var          <- NA_character_
            asig_multi_paren        <- 0L
            asig_multi_primera_func <- NA_character_
          }
          next
        }
        # Asociar kable/ggplot/terminal en esta línea a asig_multi_var
        if (grepl(re_func_tabla, linea, perl = TRUE)) {
          diferidos[[asig_multi_var]] <- "tabla"
          en_kable <- TRUE
        }
        if (grepl(re_func_figura, linea, perl = TRUE) ||
            grepl(re_terminal, linea, perl = TRUE)) {
          diferidos[[asig_multi_var]] <- "figura"
        }
        # Actualizar nivel de paréntesis
        asig_multi_paren <- asig_multi_paren +
                            contar_ch(linea, "(") - contar_ch(linea, ")")
        # ¿Termina la asignación?
        if (asig_multi_paren <= 0L &&
            !grepl(re_continua, linea, perl = TRUE)) {
          # Si no se detectó ningún tipo conocido en la cadena
          # multi-línea Y la primera función llamada no está en la
          # lista negra de funciones triviales, marcamos como
          # tentativo (la RHS era una llamada a función desconocida).
          if (is.null(diferidos[[asig_multi_var]]) &&
              !es_funcion_lista_negra(asig_multi_primera_func)) {
            diferidos_tentativos[[asig_multi_var]] <- "tabla"
            # Si la función RHS es una función conocida del paquete
            # MATrstars, anotamos también las captions esperables por
            # campo/índice, para que al consumir después
            # `LHS$campo` o `LHS[[n]]` la etiqueta manual incluya la
            # descripción original de la tabla.
            info_pkg <- FUNCIONES_PAQUETE_MATRSTARS[[
              asig_multi_primera_func]]
            if (!is.null(info_pkg)) {
              capciones_conocidas[[asig_multi_var]] <- info_pkg
            }
          }
          asig_multi_var   <- NA_character_
          asig_multi_paren <- 0L
          asig_multi_primera_func <- NA_character_
        }
        next
      }

      # 1) Línea con SOLO un nombre de variable (impresión de objeto)
      m_solo <- regexec(re_solo_nombre, linea, perl = TRUE)
      r_solo <- regmatches(linea, m_solo)[[1]]
      if (length(r_solo) >= 2 && !tiene_terminal) {
        var_name <- r_solo[2]
        if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          diferidos[[var_name]] <- NULL
        }
        next
      }

      # 2) Línea con SOLO var[[expr]] (impresión de elemento de lista)
      m_solo_l <- regexec(re_solo_lista, linea, perl = TRUE)
      r_solo_l <- regmatches(linea, m_solo_l)[[1]]
      if (length(r_solo_l) >= 2 && !tiene_terminal) {
        var_name <- r_solo_l[2]
        # Extraer índice numérico si es literal (para lookup de captions)
        m_idx <- regexec(
          "^\\s*[A-Za-z_.][A-Za-z_.0-9]*\\s*\\[\\[\\s*([0-9]+)\\s*\\]\\]\\s*$",
          linea, perl = TRUE)
        r_idx <- regmatches(linea, m_idx)[[1]]
        idx_num <- if (length(r_idx) >= 2) as.integer(r_idx[2]) else NA_integer_
        # Función auxiliar: obtener caption conocida por índice
        cap_por_idx <- function(var_name, idx_num) {
          if (is.na(idx_num)) return(NA_character_)
          km <- capciones_conocidas[[var_name]]
          if (is.null(km) || is.null(km$captions_by_index)) return(NA_character_)
          if (idx_num < 1L || idx_num > length(km$captions_by_index)) {
            return(NA_character_)
          }
          val <- km$captions_by_index[idx_num]
          if (is.na(val)) NA_character_ else val
        }
        if (!is.null(contenedores[[var_name]])) {
          seq_tipo     <- c(seq_tipo, contenedores[[var_name]])
          seq_cap      <- c(seq_cap,  cap_por_idx(var_name, idx_num))
          seq_posicion <- c(seq_posicion, i)
          next
        } else if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  cap_por_idx(var_name, idx_num))
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos[[var_name]]
          next
        } else if (!is.null(diferidos_tentativos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos_tentativos[[var_name]])
          seq_cap      <- c(seq_cap,  cap_por_idx(var_name, idx_num))
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos_tentativos[[var_name]]
          next
        }
        next
      }

      # 2b) Línea con SOLO var$nombre (impresión de elemento de lista)
      m_solo_d <- regexec(re_solo_dollar, linea, perl = TRUE)
      r_solo_d <- regmatches(linea, m_solo_d)[[1]]
      if (length(r_solo_d) >= 3 && !tiene_terminal) {
        var_name   <- r_solo_d[2]
        field_name <- r_solo_d[3]
        # Función auxiliar: obtener caption conocida por campo
        cap_por_field <- function(var_name, field_name) {
          km <- capciones_conocidas[[var_name]]
          if (is.null(km) || is.null(km$captions_by_field)) return(NA_character_)
          if (!(field_name %in% names(km$captions_by_field))) {
            return(NA_character_)
          }
          val <- km$captions_by_field[[field_name]]
          if (is.na(val)) NA_character_ else val
        }
        if (!is.null(contenedores[[var_name]])) {
          seq_tipo     <- c(seq_tipo, contenedores[[var_name]])
          seq_cap      <- c(seq_cap,  cap_por_field(var_name, field_name))
          seq_posicion <- c(seq_posicion, i)
        } else if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  cap_por_field(var_name, field_name))
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos[[var_name]]
        } else if (!is.null(diferidos_tentativos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos_tentativos[[var_name]])
          seq_cap      <- c(seq_cap,  cap_por_field(var_name, field_name))
          seq_posicion <- c(seq_posicion, i)
          contenedores[[var_name]] <- diferidos_tentativos[[var_name]]
        }
        next
      }

      # 2c) Patrón especial (compatibilidad histórica): `generar_solucion(ARG)`
      #     solita, tal como se llamaba antes de la migración de la función al
      #     paquete MATrstars, cuando la lista de resultado se asignaba
      #     directamente al Global Environment como `solucion_<ARG>` mediante
      #     `assign(..., envir = .GlobalEnv)`. Produce 1 figura como side
      #     effect (un mosaico) y deja un contenedor `solucion_<ARG>` para
      #     los usos posteriores de `solucion_<ARG>$X`.
      re_generar_sol <- paste0(
        "^\\s*generar_solucion\\s*\\(\\s*",
        "([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\)\\s*$")
      m_gs <- regexec(re_generar_sol, linea, perl = TRUE)
      r_gs <- regmatches(linea, m_gs)[[1]]
      if (length(r_gs) >= 2) {
        arg_name <- r_gs[2]
        seq_tipo     <- c(seq_tipo, "figura")
        seq_cap      <- c(seq_cap,  NA_character_)
        seq_posicion <- c(seq_posicion, i)
        contenedor_nm <- paste0("solucion_", arg_name)
        contenedores[[contenedor_nm]] <- "tabla"
        info_pkg <- FUNCIONES_PAQUETE_MATRSTARS[["generar_solucion"]]
        if (!is.null(info_pkg)) {
          capciones_conocidas[[contenedor_nm]] <- info_pkg
        }
        next
      }

      # 2c') Patrón especial (post-migración a MATrstars):
      #      `LHS <- FUNC(ARG)` para funciones de MATrstars listadas en
      #      FUNCIONES_PAQUETE_MATRSTARS. Registra las figuras que la
      #      función produce como side effect, crea un contenedor LHS
      #      con tipo "tabla" y anota las captions esperables por
      #      campo/índice, para que las etiquetas manuales incluyan la
      #      descripción original.
      re_asig_func_paquete <- paste0(
        "^\\s*([A-Za-z_.][A-Za-z_.0-9]*)\\s*(?:<-|=)\\s*",
        "([A-Za-z_.][A-Za-z_.0-9]*)\\s*\\(")
      m_afp <- regexec(re_asig_func_paquete, linea, perl = TRUE)
      r_afp <- regmatches(linea, m_afp)[[1]]
      if (length(r_afp) >= 3) {
        var_lhs  <- r_afp[2]
        func_rhs <- r_afp[3]
        info_pkg <- FUNCIONES_PAQUETE_MATRSTARS[[func_rhs]]
        # Solo procedemos si la línea entera es
        # `LHS <- FUNC(ARG)` en una sola línea (para no interferir con
        # llamadas multilínea que se manejan por otro camino).
        re_asig_func_1arg <- paste0(
          "^\\s*[A-Za-z_.][A-Za-z_.0-9]*\\s*(?:<-|=)\\s*",
          "[A-Za-z_.][A-Za-z_.0-9]*\\s*\\(\\s*",
          "[A-Za-z_.][A-Za-z_.0-9]*\\s*\\)\\s*$")
        if (!is.null(info_pkg) &&
            grepl(re_asig_func_1arg, linea, perl = TRUE)) {
          if (info_pkg$figures > 0L) {
            for (k in seq_len(info_pkg$figures)) {
              seq_tipo     <- c(seq_tipo, "figura")
              seq_cap      <- c(seq_cap,  NA_character_)
              seq_posicion <- c(seq_posicion, i)
            }
          }
          contenedores[[var_lhs]] <- "tabla"
          capciones_conocidas[[var_lhs]] <- info_pkg
          next
        }
      }

      # 2d) Línea con patchwork inline tipo `g1 / g2`, `g1 + g2 + g3`
      #     o `(g1 | g2) / (g3 | g4)`. Solo se detecta si hay al menos
      #     un operador +,/,| y al menos un nombre que sea figura
      #     diferida.
      if (grepl(re_patchwork_inline, linea, perl = TRUE) &&
          grepl("[+/|]", linea, perl = TRUE) &&
          !grepl("\\bfunction\\b", linea, perl = TRUE)) {
        # Extraer los nombres de las variables
        nombres <- regmatches(linea,
                              gregexpr("[A-Za-z_.][A-Za-z_.0-9]*",
                                       linea, perl = TRUE))[[1]]
        # ¿Alguno es figura diferida?
        es_patchwork <- any(sapply(nombres, function(n) {
          !is.null(diferidos[[n]]) && diferidos[[n]] == "figura"
        }))
        if (es_patchwork) {
          seq_tipo     <- c(seq_tipo, "figura")
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          # Consumir las figuras diferidas usadas
          for (n in nombres) {
            if (!is.null(diferidos[[n]]) && diferidos[[n]] == "figura") {
              diferidos[[n]] <- NULL
            }
          }
          next
        }
      }

      # 3) print(var[[...]]) o print(var) que consume diferido/contenedor
      m_pr <- regexec(re_print_var, linea, perl = TRUE)
      r_pr <- regmatches(linea, m_pr)[[1]]
      if (length(r_pr) >= 2) {
        var_name <- r_pr[2]
        consumido <- FALSE
        if (!is.null(contenedores[[var_name]])) {
          seq_tipo     <- c(seq_tipo, contenedores[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          consumido <- TRUE
        } else if (!is.null(diferidos[[var_name]])) {
          seq_tipo     <- c(seq_tipo, diferidos[[var_name]])
          seq_cap      <- c(seq_cap,  NA_character_)
          seq_posicion <- c(seq_posicion, i)
          diferidos[[var_name]] <- NULL
          consumido <- TRUE
        }
        if (consumido) next
      }

      # 4a) Asignación a elemento de lista con RHS = otro nombre simple:
      #     `lista[[idx]] <- otra_var`. Si otra_var es diferida, el
      #     contenedor hereda su tipo.
      m_arl <- regexec(re_asig_lista_rhs, linea, perl = TRUE)
      r_arl <- regmatches(linea, m_arl)[[1]]
      if (length(r_arl) >= 3) {
        lhs <- r_arl[2]; rhs <- r_arl[3]
        if (!is.null(diferidos[[rhs]])) {
          contenedores[[lhs]] <- diferidos[[rhs]]
        } else if (!is.null(contenedores[[rhs]])) {
          contenedores[[lhs]] <- contenedores[[rhs]]
        }
        next
      }

      # 4b) Asignación a elemento de lista con expresión inline:
      m_asig_l <- regexec(re_asig_lista, linea, perl = TRUE)
      r_asig_l <- regmatches(linea, m_asig_l)[[1]]
      if (length(r_asig_l) >= 2) {
        var_name <- r_asig_l[2]
        if (grepl(re_func_tabla, linea, perl = TRUE)) {
          contenedores[[var_name]] <- "tabla"
          en_kable <- TRUE
        } else if (grepl(re_func_figura, linea, perl = TRUE) ||
                   grepl(re_terminal, linea, perl = TRUE)) {
          contenedores[[var_name]] <- "figura"
        } else if (grepl(re_continua, linea, perl = TRUE) ||
                   (contar_ch(linea, "(") - contar_ch(linea, ")")) > 0L) {
          # asignación a lista multi-línea (el kable vendrá más abajo)
          asig_multi_var <- var_name
          asig_multi_paren <- contar_ch(linea, "(") - contar_ch(linea, ")")
          # marcar como contenedor (tipo se decidirá al detectar kable/ggplot)
        }
        next
      }

      # 5) Asignación normal: var <- algo
      m_asig <- regexec(re_asig_inicio, linea, perl = TRUE)
      r_asig <- regmatches(linea, m_asig)[[1]]
      es_asignacion <- length(r_asig) >= 2
      var_name <- if (es_asignacion) r_asig[2] else NA_character_

      hay_tabla  <- grepl(re_func_tabla,  linea, perl = TRUE)
      hay_figura <- grepl(re_func_figura, linea, perl = TRUE)

      if (tiene_terminal && es_asignacion) {
        diferidos[[var_name]] <- "figura"
      } else if (tiene_terminal) {
        seq_tipo     <- c(seq_tipo, "figura")
        seq_cap      <- c(seq_cap,  NA_character_)
        seq_posicion <- c(seq_posicion, i)
      } else if (es_asignacion) {
        if (hay_tabla) {
          diferidos[[var_name]] <- "tabla"
          en_kable <- TRUE
        }
        if (hay_figura) {
          diferidos[[var_name]] <- "figura"
        }
        # ¿La asignación continúa en líneas siguientes?
        if (!hay_tabla && !hay_figura) {
          n_p_abre  <- contar_ch(linea, "(")
          n_p_cierr <- contar_ch(linea, ")")
          # Extraer la primera función llamada en el RHS
          primera_func <- NA_character_
          m_pf <- regexec(
            "(?:<-|=)\\s*([A-Za-z_.][A-Za-z_.0-9]*(?:::[A-Za-z_.][A-Za-z_.0-9]*)?)\\s*\\(",
            linea, perl = TRUE)
          r_pf <- regmatches(linea, m_pf)[[1]]
          if (length(r_pf) >= 2) {
            primera_func <- sub("^.*::", "", r_pf[2])  # quitar namespace
          }
          if (grepl(re_continua, linea, perl = TRUE) ||
              n_p_abre > n_p_cierr) {
            asig_multi_var   <- var_name
            asig_multi_paren <- n_p_abre - n_p_cierr
            asig_multi_primera_func <- primera_func
          } else if (n_p_abre > 0L &&
                     !es_funcion_lista_negra(primera_func)) {
            # asignación a llamada de función desconocida (sin tabla
            # ni figura conocidas, ni en lista negra). Marcamos como
            # tentativo: solo contará si después vemos var[[expr]] o
            # var$nombre.
            diferidos_tentativos[[var_name]] <- "tabla"
            # Si la función es una conocida del paquete MATrstars,
            # anotar las captions esperables por campo/índice.
            info_pkg <- FUNCIONES_PAQUETE_MATRSTARS[[primera_func]]
            if (!is.null(info_pkg)) {
              capciones_conocidas[[var_name]] <- info_pkg
            }
          }
        }
      } else {
        # Filtrar pander("texto literal") — no es tabla, es solo título.
        es_pander_texto <- grepl(
          "\\bpander(?:_return)?\\s*\\(\\s*\"",
          linea, perl = TRUE)
        m_tab <- gregexpr(re_func_tabla, linea, perl = TRUE)[[1]]
        if (m_tab[1] != -1 && !es_pander_texto) {
          for (k in seq_along(m_tab)) {
            seq_tipo     <- c(seq_tipo, "tabla")
            seq_cap      <- c(seq_cap,  NA_character_)
            seq_posicion <- c(seq_posicion, i)
          }
          en_kable <- TRUE
        }
        m_fig <- gregexpr(re_func_figura, linea, perl = TRUE)[[1]]
        if (m_fig[1] != -1) {
          for (k in seq_along(m_fig)) {
            seq_tipo     <- c(seq_tipo, "figura")
            seq_cap      <- c(seq_cap,  NA_character_)
            seq_posicion <- c(seq_posicion, i)
          }
        }
      }

      if (en_kable && grepl(re_cap_lit, linea, perl = TRUE)) {
        m_c <- regmatches(linea, regexec(re_cap_lit, linea, perl = TRUE))[[1]]
        if (length(m_c) >= 2) {
          idx <- tail(which(seq_tipo == "tabla" & is.na(seq_cap)), 1L)
          if (length(idx) > 0L) seq_cap[idx] <- m_c[2]
        }
      }
    }

    # Si hay terminal (patchwork) en el chunk, los bucles dejan de
    # descalificar el chunk.
    if (length(grep(re_terminal, paste(buffer, collapse = "\n"),
                    perl = TRUE)) > 0) {
      tiene_bucle <- FALSE
    }
    # Si hubo detección de elementos usando contenedores, el `for` solo
    # iteraba sobre la lista para imprimirla.
    if (length(seq_tipo) > 0L && length(contenedores) > 0L) {
      tiene_bucle <- FALSE
    }

    # Colapsar duplicados consecutivos del mismo tipo con MISMO caption
    if (length(seq_tipo) >= 2L) {
      mantener <- rep(TRUE, length(seq_tipo))
      for (i in 2:length(seq_tipo)) {
        if (seq_tipo[i] == seq_tipo[i - 1L]) {
          ci   <- seq_cap[i]
          cim1 <- seq_cap[i - 1L]
          if (!is.na(ci) && !is.na(cim1) && identical(ci, cim1)) {
            mantener[i] <- FALSE
          }
        }
      }
      seq_tipo     <- seq_tipo[mantener]
      seq_cap      <- seq_cap[mantener]
      seq_posicion <- seq_posicion[mantener]
    }

    list(
      secuencia            = seq_tipo,
      captions             = seq_cap,
      posiciones           = seq_posicion,
      tiene_bucle          = tiene_bucle,
      contenedores         = contenedores,
      diferidos_tentativos = diferidos_tentativos,
      capciones_conocidas  = capciones_conocidas,
      sideeffect_seq       = sideeffect_seq
    )
  }

  # -------------------------------------------------------------------
  # 3.2. Quitar TODOS los caption (literales y paste) de las llamadas
  #      kable/kbl del chunk para evitar la doble autonumeración bookdown
  # -------------------------------------------------------------------
  quitar_todos_captions <- function(buffer) {
    re_kable_open <- "\\b(?:kable|kbl|kable_rstars)\\s*\\("

    # Ya no saltamos definiciones de función: queremos limpiar también
    # los captions de los kables que viven dentro del cuerpo de funciones
    # custom (presenta_modelo, generar_solucion, predict_from_excel_*).
    # Sin esa limpieza, bookdown autonumeraría las tablas cuando se llame
    # a la función, produciendo "Table X.Y:" antes de nuestra etiqueta.
    dentro_kable <- FALSE
    nivel_paren  <- 0L

    nuevo <- buffer
    for (i in seq_along(nuevo)) {
      linea <- nuevo[i]
      if (is.na(linea)) next

      if (!dentro_kable && grepl(re_kable_open, linea, perl = TRUE)) {
        dentro_kable <- TRUE
        nivel_paren  <- 0L
      }

      if (dentro_kable) {
        # 1) caption literal
        if (grepl(re_cap_lit, linea, perl = TRUE)) {
          linea <- sub(re_cap_lit, "", linea, perl = TRUE)
        }
        # 2) caption = paste(...) o paste0(...) con balanceo
        m_paste <- regexpr("caption\\s*=\\s*paste0?\\s*\\(",
                          linea, perl = TRUE)
        if (m_paste > 0) {
          ini  <- as.integer(m_paste)
          pos_paren <- regexpr("\\(", substr(linea, ini, nchar(linea)),
                              perl = TRUE)
          if (pos_paren > 0) {
            par_ini_abs <- ini + as.integer(pos_paren) - 1L
            nivel <- 1L
            j <- par_ini_abs + 1L
            ll <- linea
            line_idx <- i
            cierre_col <- NA_integer_
            while (TRUE) {
              if (j > nchar(ll)) {
                line_idx <- line_idx + 1L
                if (line_idx > length(nuevo)) break
                ll <- nuevo[line_idx]
                if (is.na(ll)) ll <- ""
                j <- 1L
                if (nchar(ll) == 0L) next
              }
              ch <- substr(ll, j, j)
              if (ch == "(") nivel <- nivel + 1L
              else if (ch == ")") {
                nivel <- nivel - 1L
                if (nivel == 0L) {
                  cierre_col <- j
                  break
                }
              }
              j <- j + 1L
            }
            if (!is.na(cierre_col)) {
              if (line_idx == i) {
                resto <- substr(linea, cierre_col + 1L, nchar(linea))
                tm <- regexpr("^\\s*,\\s*", resto, perl = TRUE)
                if (tm > 0) {
                  cierre_col <- cierre_col + attr(tm, "match.length")
                }
                linea <- paste0(substr(linea, 1L, ini - 1L),
                                substr(linea, cierre_col + 1L, nchar(linea)))
              } else {
                nuevo[i] <- substr(linea, 1L, ini - 1L)
                if (line_idx > i + 1L) {
                  nuevo[(i + 1L):(line_idx - 1L)] <- NA_character_
                }
                ult <- nuevo[line_idx]
                if (is.na(ult)) ult <- ""
                resto <- substr(ult, cierre_col + 1L, nchar(ult))
                tm <- regexpr("^\\s*,\\s*", resto, perl = TRUE)
                if (tm > 0) {
                  cierre_col <- cierre_col + attr(tm, "match.length")
                }
                nuevo[line_idx] <- substr(ult, cierre_col + 1L, nchar(ult))
                linea <- nuevo[i]
              }
            }
          }
        }

        abiertos <- nchar(gsub("[^(]", "", linea))
        cerrados <- nchar(gsub("[^)]", "", linea))
        nivel_paren <- nivel_paren + abiertos - cerrados
        nuevo[i] <- linea
        if (nivel_paren <= 0L) {
          dentro_kable <- FALSE
          nivel_paren  <- 0L
        }
      }
    }

    nuevo <- nuevo[!is.na(nuevo)]
    nuevo
  }

  # -------------------------------------------------------------------
  # 3.3. Imágenes markdown
  # -------------------------------------------------------------------
  procesar_linea_imagen <- function(linea) {
    if (es_titular(linea)) return(list(linea = linea, contado = FALSE))
    if (grepl("\\.hicon", linea, perl = TRUE)) {
      return(list(linea = linea, contado = FALSE))
    }
    re_pandoc <- "!\\[\\[([^\\]]*)\\]\\{([^}]*)\\}\\]\\(([^)]+)\\)"
    if (grepl(re_pandoc, linea, perl = TRUE)) {
      m <- regmatches(linea, regexec(re_pandoc, linea, perl = TRUE))[[1]]
      alt <- if (length(m) >= 3) paste0("[", m[2], "]{", m[3], "}") else ""
      linea_modif <- sub(re_pandoc, "![](\\3)", linea, perl = TRUE)
      return(list(linea = linea_modif, contado = TRUE, alt = alt))
    }
    re_simple <- "!\\[([^\\]]*)\\]\\(([^)]+)\\)"
    if (grepl(re_simple, linea, perl = TRUE)) {
      m <- regmatches(linea, regexec(re_simple, linea, perl = TRUE))[[1]]
      alt <- if (length(m) >= 2) m[2] else ""
      if (envolver_alt_simple && nzchar(alt) &&
          !grepl("\\]\\{", alt, perl = TRUE)) {
        alt <- paste0("[", alt, "]{.smallcaps}")
      }
      linea_modif <- sub(re_simple, "![](\\2)", linea, perl = TRUE)
      return(list(linea = linea_modif, contado = TRUE, alt = alt))
    }
    list(linea = linea, contado = FALSE)
  }

  # -------------------------------------------------------------------
  # 3.4. Detectar tabla markdown manual
  # -------------------------------------------------------------------
  detectar_tabla_md <- function(lineas, i) {
    if (i > length(lineas)) return(list(es_tabla = FALSE))
    if (!grepl(re_tabla_md_row, lineas[i], perl = TRUE)) {
      return(list(es_tabla = FALSE))
    }
    j <- i
    tiene_sep <- FALSE
    while (j <= length(lineas) &&
           grepl(re_tabla_md_row, lineas[j], perl = TRUE)) {
      if (grepl(re_tabla_md_sep, lineas[j], perl = TRUE)) tiene_sep <- TRUE
      j <- j + 1L
    }
    fin <- j - 1L
    if (!tiene_sep) return(list(es_tabla = FALSE))

    caption <- NA_character_
    k <- fin + 1L
    while (k <= length(lineas) && grepl("^\\s*$", lineas[k])) k <- k + 1L
    if (k <= length(lineas)) {
      m_post <- regexec("^\\s*:\\s*(.+?)\\.?\\s*$", lineas[k], perl = TRUE)
      r <- regmatches(lineas[k], m_post)[[1]]
      if (length(r) >= 2) {
        caption <- r[2]
        fin <- k
      } else {
        m_post2 <- regexec("^\\s*Table:\\s*(.+?)\\s*$",
                          lineas[k], perl = TRUE)
        r2 <- regmatches(lineas[k], m_post2)[[1]]
        if (length(r2) >= 2) {
          caption <- r2[2]
          fin <- k
        }
      }
    }
    list(es_tabla = TRUE, fin = fin, caption = caption)
  }

  # -------------------------------------------------------------------
  # 3.5. Dividir un chunk mixto en sub-chunks
  # -------------------------------------------------------------------
  # Para chunks con varios elementos (p.ej. una tabla + una figura),
  # buscamos un punto de corte (línea en blanco) inmediatamente después
  # de cada elemento. Si lo encontramos, dividimos el chunk en
  # sub-chunks de forma que cada elemento quede en su propio sub-chunk
  # y su etiqueta pueda aparecer pegada justo debajo.
  #
  # Devuelve NULL si no se puede dividir (no hay puntos de corte
  # suficientes); en ese caso el bucle principal cae al modo "todas las
  # etiquetas al final".
  #
  # Si devuelve una lista, es una secuencia de bloques de tipo:
  #   list(tipo = "sub_chunk_elem", buffer = ..., elem_tipo, elem_cap)
  #   list(tipo = "sub_chunk_resto", buffer = ...)   # sin etiqueta
  dividir_chunk_mixto <- function(buffer, posiciones, secuencia, captions) {
    n_elem <- length(posiciones)
    if (n_elem == 0L) return(NULL)

    # Para cada elemento k determinamos:
    #   fines[k]   = última línea del sub-chunk k
    #   inicios[k] = primera línea del sub-chunk k
    # Heurísticas para el corte:
    #   - Si hay línea en blanco entre pos_k y pos_{k+1}: corte ahí
    #     y se salta la línea blanca.
    #   - Si pos_{k+1} == pos_k + 1 (elementos en líneas adyacentes):
    #     corte forzado, cada sub-chunk = una línea con su elemento.
    #   - Si hay código intermedio sin línea en blanco: NO se puede
    #     dividir limpiamente (NULL).
    fines    <- integer(n_elem)
    inicios  <- integer(n_elem)
    inicios[1L] <- 1L
    resto_inicio <- length(buffer) + 1L  # por defecto, sin resto

    for (k in seq_len(n_elem)) {
      pos_k <- posiciones[k]

      if (k < n_elem) {
        siguiente <- posiciones[k + 1L]
        corte_blanco <- 0L
        if ((pos_k + 1L) <= (siguiente - 1L)) {
          for (j in (pos_k + 1L):(siguiente - 1L)) {
            if (grepl("^\\s*$", buffer[j])) {
              corte_blanco <- j
              break
            }
          }
        }
        if (corte_blanco > 0L) {
          fines[k]            <- corte_blanco - 1L
          inicios[k + 1L]     <- corte_blanco + 1L
        } else if (siguiente == pos_k + 1L) {
          # Corte forzado entre líneas adyacentes
          fines[k]        <- pos_k
          inicios[k + 1L] <- siguiente
        } else {
          # Código entre elementos sin línea en blanco: no dividir
          return(NULL)
        }
      } else {
        # Último elemento: buscar línea en blanco después
        corte_blanco <- 0L
        if ((pos_k + 1L) <= length(buffer)) {
          for (j in (pos_k + 1L):length(buffer)) {
            if (grepl("^\\s*$", buffer[j])) {
              corte_blanco <- j
              break
            }
          }
        }
        if (corte_blanco > 0L) {
          fines[k]     <- corte_blanco - 1L
          resto_inicio <- corte_blanco + 1L
        } else {
          fines[k]     <- length(buffer)
          resto_inicio <- length(buffer) + 1L
        }
      }
    }

    # Si solo hay un elemento y el resto no existe (etiqueta al final
    # equivaldría al comportamiento sin dividir), no merece la pena.
    if (n_elem == 1L && resto_inicio > length(buffer)) {
      return(NULL)
    }

    # Construir bloques
    bloques <- list()
    for (k in seq_len(n_elem)) {
      sub_b <- if (fines[k] >= inicios[k])
                 buffer[inicios[k]:fines[k]] else character(0)
      bloques[[length(bloques) + 1L]] <- list(
        tipo         = "sub_chunk_elem",
        buffer       = sub_b,
        elem_tipo    = secuencia[k],
        elem_caption = captions[k]
      )
    }
    if (resto_inicio <= length(buffer)) {
      resto <- buffer[resto_inicio:length(buffer)]
      if (any(!grepl("^\\s*$", resto))) {
        bloques[[length(bloques) + 1L]] <- list(
          tipo   = "sub_chunk_resto",
          buffer = resto
        )
      }
    }

    # Verificación de balance de delimitadores
    contar_char <- function(txt, ch) {
      sum(strsplit(txt, "", fixed = TRUE)[[1]] == ch)
    }
    for (b in bloques) {
      txt <- paste(b$buffer, collapse = "\n")
      if (contar_char(txt, "{") != contar_char(txt, "}") ||
          contar_char(txt, "(") != contar_char(txt, ")") ||
          contar_char(txt, "[") != contar_char(txt, "]")) {
        return(NULL)
      }
    }

    bloques
  }

  # ===================================================================
  # 4. Recorrido principal
  # ===================================================================
  cont_tabla  <- 0L
  cont_figura <- 0L
  estado <- "texto"
  chunk_header <- ""
  chunk_buffer <- character(0)
  salida <- character(0)
  registro <- character(0)
  # Estado global entre chunks: contenedores y diferidos tentativos
  # creados en chunks previos pueden ser consumidos en chunks
  # posteriores (p.ej. `var <- f(...)` en chunk N y `var[[expr]]` en
  # chunk N+1).
  contenedores_globales <- list()
  tentativos_globales   <- list()
  capciones_globales    <- list()

  push <- function(...) salida <<- c(salida, ...)

  # ------------------------------------------------------------------
  # Inyección de chunk de setup: activa la opción global que hace que
  # las llamadas internas a kable_rstars() desde funciones del paquete
  # MATrstars (presenta_modelo, generar_solucion,
  # predict_from_excel_scenarios, ...) no emitan el argumento `caption`
  # al kable subyacente. Sin esta opción, bookdown auto-numeraría dos
  # veces cada tabla generada dentro del paquete, produciendo prefijos
  # dobles del tipo "Table 10.1: Table 10.2: <caption>". Con la opción
  # activa, las tablas quedan sin auto-caption y la numeración manual
  # de este script (bloque "**Tabla X.Y**") funciona como para las
  # tablas normales del .Rmd.
  # Requiere MATrstars >= 0.0.0.9006 (donde kable_rstars honra
  # `getOption('matrstars.suppress_caption', FALSE)`).
  # NOTA: el label del chunk incluye el número de capítulo para evitar
  # colisiones al fusionar los .Rmd de todos los capítulos en un único
  # RStarS.Rmd durante el render final (bookdown detecta labels
  # duplicados y aborta).
  push(sprintf("```{r matrstars-numerado-setup-cap%d, include=FALSE}",
               num_capitulo))
  push("options(matrstars.suppress_caption = TRUE)")
  push("```")
  push("")

  i <- 1L
  n_lineas <- length(lineas)
  while (i <= n_lineas) {
    linea <- lineas[i]

    if (estado == "texto") {
      if (es_chunk_inicio(linea)) {
        estado <- "chunk"
        chunk_header <- linea
        chunk_buffer <- character(0)
        i <- i + 1L
        next
      }
      td <- detectar_tabla_md(lineas, i)
      if (isTRUE(td$es_tabla)) {
        cont_tabla <- cont_tabla + 1L
        for (k in i:td$fin) push(lineas[k])
        desc <- if (incluir_caption_tabla && !is.na(td$caption)) td$caption else NULL
        push(bloque_numeracion("tabla", num_capitulo, cont_tabla, desc))
        registro <- c(registro, sprintf(
          "  Tabla  %d.%d  (markdown manual, linea %d)%s",
          num_capitulo, cont_tabla, i,
          if (!is.na(td$caption))
            sprintf(" - caption: \"%s\"", td$caption) else ""))
        i <- td$fin + 1L
        next
      }
      if (grepl("!\\[", linea, perl = TRUE)) {
        res <- procesar_linea_imagen(linea)
        if (isTRUE(res$contado)) {
          cont_figura <- cont_figura + 1L
          push(res$linea)
          desc <- if (incluir_alt_figura) res$alt else NULL
          push(bloque_numeracion("figura", num_capitulo, cont_figura, desc))
          registro <- c(registro, sprintf(
            "  Figura %d.%d  (imagen markdown, linea %d)",
            num_capitulo, cont_figura, i))
          i <- i + 1L
          next
        }
      }
      push(linea)
      i <- i + 1L
    } else {
      if (es_chunk_fin(linea)) {
        skip <- es_eval_false(chunk_header)
        info <- analizar_chunk(chunk_buffer,
                              contenedores_iniciales = contenedores_globales,
                              diferidos_tentativos_iniciales = tentativos_globales,
                              capciones_iniciales = capciones_globales)
        # Fusionar estado global con lo devuelto (no sobrescribir).
        # Así los tentativos/contenedores creados en chunks previos
        # persisten para los chunks siguientes.
        if (!skip && !is.null(info$contenedores)) {
          contenedores_globales <- modifyList(contenedores_globales,
                                              info$contenedores)
        }
        if (!skip && !is.null(info$diferidos_tentativos)) {
          tentativos_globales <- modifyList(tentativos_globales,
                                            info$diferidos_tentativos)
        }
        if (!skip && !is.null(info$capciones_conocidas)) {
          capciones_globales <- modifyList(capciones_globales,
                                           info$capciones_conocidas)
        }
        # Si un tentativo ha sido promovido a contenedor en este chunk,
        # ya no necesita estar en tentativos.
        if (length(contenedores_globales) > 0 &&
            length(tentativos_globales) > 0) {
          for (k in names(contenedores_globales)) {
            tentativos_globales[[k]] <- NULL
          }
        }

        # ¿El chunk tiene llamadas side-effect (explora_na, etc.)?
        # Si es asi, se procesa distinto: se inyecta al inicio del
        # chunk una linea `options(matrstars.<tipo>_num = "X.Y", ...)`
        # con los numeros que le corresponden a cada elemento, y NO se
        # emiten etiquetas al final del chunk (la propia funcion las
        # emitira en su lugar exacto).
        #
        # Excepciones (NO reservar side-effect):
        #  - Chunks con `eval=FALSE` (skip): no se ejecutan.
        #  - Chunks con `include=FALSE`: se ejecutan pero nada de su
        #    output aparece en el HTML. Util cuando el autor sabe de
        #    antemano que la funcion no va a emitir grafico ni tabla
        #    (por ejemplo, cuando ya sabe que el df de entrada no tiene
        #    NAs). Asi la numeracion del capitulo no se desplaza.
        include_false <- es_include_false(chunk_header)
        tiene_sideeffect <- !skip && !include_false &&
                            length(info$sideeffect_seq) > 0L

        if (tiene_sideeffect) {
          # Calcular los numeros X.Y por tipo, en el orden en que
          # aparecen en sideeffect_seq. Cada "figura" incrementa el
          # contador de figuras del capitulo; cada "tabla", el de
          # tablas.
          opciones_pairs <- character(0)
          for (elem in info$sideeffect_seq) {
            if (elem == "figura") {
              cont_figura <- cont_figura + 1L
              opciones_pairs <- c(
                opciones_pairs,
                sprintf('matrstars.figura_num = "%d.%d"',
                        num_capitulo, cont_figura)
              )
              registro <- c(registro, sprintf(
                "  Figura %d.%d  (side-effect, chunk linea %d)",
                num_capitulo, cont_figura,
                i - length(chunk_buffer) - 1L))
            } else if (elem == "tabla") {
              cont_tabla <- cont_tabla + 1L
              opciones_pairs <- c(
                opciones_pairs,
                sprintf('matrstars.tabla_num = "%d.%d"',
                        num_capitulo, cont_tabla)
              )
              registro <- c(registro, sprintf(
                "  Tabla  %d.%d  (side-effect, chunk linea %d)",
                num_capitulo, cont_tabla,
                i - length(chunk_buffer) - 1L))
            }
          }
          opciones_linea <- paste0(
            "options(",
            paste(opciones_pairs, collapse = ", "),
            ")"
          )
          chunk_limpio <- quitar_todos_captions(chunk_buffer)
          push(chunk_header)
          push(opciones_linea)
          push(chunk_limpio)
          push(linea)  # cierre del chunk

          estado <- "texto"
          chunk_header <- ""
          chunk_buffer <- character(0)
          i <- i + 1L
          next
        }

        if (skip) {
          # Chunk eval=FALSE: dejar tal cual, no se renderiza.
          push(chunk_header); push(chunk_buffer); push(linea)
        } else if (length(info$secuencia) == 0L) {
          # Chunk eval=TRUE pero sin tablas/figuras directas detectadas.
          # PUEDE contener definiciones de funciones cuyos kables se
          # ejecutarán al llamar la función — limpiamos sus captions
          # para evitar autonumeración bookdown.
          chunk_limpio <- quitar_todos_captions(chunk_buffer)
          push(chunk_header); push(chunk_limpio); push(linea)
        } else if (info$tiene_bucle) {
          chunk_limpio <- quitar_todos_captions(chunk_buffer)
          push(chunk_header); push(chunk_limpio); push(linea)
          n_tab <- sum(info$secuencia == "tabla")
          n_fig <- sum(info$secuencia == "figura")
          registro <- c(registro, sprintf(
            "  Bucle  (chunk linea %d) - sin numerar (tab=%d fig=%d)",
            i - length(chunk_buffer) - 1L, n_tab, n_fig))
        } else {
          chunk_limpio <- quitar_todos_captions(chunk_buffer)
          # Las posiciones e info del análisis ORIGINAL siguen siendo
          # válidas para el limpio (las líneas se vacían pero no se
          # eliminan, los números de línea no cambian).

          if (length(info$secuencia) >= 1L) {
            bloques <- dividir_chunk_mixto(chunk_limpio,
                                          info$posiciones,
                                          info$secuencia,
                                          info$captions)
          } else {
            bloques <- NULL
          }

          if (!is.null(bloques)) {
            # Emitir sub-chunks intercalados con etiquetas
            for (b in bloques) {
              push(chunk_header)
              push(b$buffer)
              push("```")
              if (b$tipo == "sub_chunk_elem") {
                if (b$elem_tipo == "tabla") {
                  cont_tabla <- cont_tabla + 1L
                  desc <- if (incluir_caption_tabla && !is.na(b$elem_caption))
                            b$elem_caption else NULL
                  push(bloque_numeracion("tabla", num_capitulo,
                                        cont_tabla, desc))
                  registro <- c(registro, sprintf(
                    "  Tabla  %d.%d  (sub-chunk de mixto)%s",
                    num_capitulo, cont_tabla,
                    if (!is.na(b$elem_caption))
                      sprintf(" - caption: \"%s\"", b$elem_caption) else ""))
                } else {
                  cont_figura <- cont_figura + 1L
                  push(bloque_numeracion("figura", num_capitulo, cont_figura))
                  registro <- c(registro, sprintf(
                    "  Figura %d.%d  (sub-chunk de mixto)",
                    num_capitulo, cont_figura))
                }
              }
            }
          } else {
            # No se puede dividir: comportamiento clásico (etiquetas al
            # final del chunk completo)
            push(chunk_header); push(chunk_limpio); push(linea)
            for (k in seq_along(info$secuencia)) {
              tipo <- info$secuencia[k]
              cap  <- info$captions[k]
              if (tipo == "tabla") {
                cont_tabla <- cont_tabla + 1L
                desc <- if (incluir_caption_tabla && !is.na(cap)) cap else NULL
                push(bloque_numeracion("tabla", num_capitulo, cont_tabla, desc))
                registro <- c(registro, sprintf(
                  "  Tabla  %d.%d  (chunk linea %d)%s",
                  num_capitulo, cont_tabla,
                  i - length(chunk_buffer) - 1L,
                  if (!is.na(cap)) sprintf(" - caption: \"%s\"", cap) else ""))
              } else {
                cont_figura <- cont_figura + 1L
                push(bloque_numeracion("figura", num_capitulo, cont_figura))
                registro <- c(registro, sprintf(
                  "  Figura %d.%d  (chunk linea %d)",
                  num_capitulo, cont_figura,
                  i - length(chunk_buffer) - 1L))
              }
            }
          }
        }

        estado <- "texto"
        chunk_header <- ""
        chunk_buffer <- character(0)
      } else {
        chunk_buffer <- c(chunk_buffer, linea)
      }
      i <- i + 1L
    }
  }

  # ===================================================================
  # 5. Volcado
  # ===================================================================
  con <- file(ruta_salida, "wb")
  writeLines(salida, con, sep = eol_original, useBytes = TRUE)
  close(con)

  if (verbose) {
    cat(sprintf("[OK] %s -> %s\n",
                basename(ruta_entrada), basename(ruta_salida)))
    cat(sprintf("     Capítulo %d. Tablas: %d. Figuras: %d.\n",
                num_capitulo, cont_tabla, cont_figura))
    if (length(registro) > 0) {
      cat("     Detalle:\n")
      cat(paste0("    ", registro, "\n"), sep = "")
    }
  }

  invisible(list(
    entrada   = ruta_entrada,
    salida    = ruta_salida,
    capitulo  = num_capitulo,
    n_tablas  = cont_tabla,
    n_figuras = cont_figura,
    registro  = registro
  ))
}

if (sys.nframe() == 0 &&
    length(commandArgs(trailingOnly = TRUE)) >= 1) {
  args <- commandArgs(trailingOnly = TRUE)
  entrada <- args[1]
  salida  <- if (length(args) >= 2 && nzchar(args[2])) args[2] else NULL
  cap     <- if (length(args) >= 3) as.integer(args[3]) else NULL
  procesar_rmd(entrada, salida, cap)
}
