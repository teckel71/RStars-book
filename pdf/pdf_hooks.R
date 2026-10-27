# =====================================================================
# pdf_hooks.R - Ajustes de R que solo se aplican al compilar el PDF.
# render.R lo inyecta al final de index.Rmd (copia en _build/) cuando el
# formato es pdf_book. La web no lo usa.
#
# gt_plt_summary() ({gtExtras}) dibuja miniaturas SVG que solo existen en
# HTML. En el PDF se imprime en su lugar una tabla con los mismos datos
# (tipo, ausentes, media, mediana, desviación típica), sin miniaturas.
# Se hace registrando un método de impresión para los objetos gt, así que
# no hay que tocar ningún capítulo.
# =====================================================================

local({
  if (!requireNamespace("gt", quietly = TRUE)) return(invisible())
  loadNamespace("gt")   # antes de registrar: si gt se cargara después,
                        # su propio método sustituiría al nuestro
  imprimir_gt_original <- getS3method("knit_print", "gt_tbl", optional = TRUE)

  formatear <- function(z) {
    z <- suppressWarnings(as.numeric(z))
    ifelse(is.na(z), "", formatC(z, format = "f", digits = 2))
  }

  knit_print_gt_pdf <- function(x, ...) {
    d <- x[["_data"]]
    columnas <- c("type", "name", "n_missing", "Mean", "Median", "SD")
    if (!all(columnas %in% names(d))) {
      if (is.null(imprimir_gt_original)) return(knitr::normal_print(x))
      return(imprimir_gt_original(x, ...))
    }

    categorica <- grepl("factor|character|ordered", d$type)
    tipo <- ifelse(categorica, "Categórica",
              ifelse(grepl("numeric|double|integer", d$type), "Métrica", d$type))
    niveles <- vapply(d$value, function(v) {
      if (is.factor(v) || is.character(v))
        as.character(length(unique(stats::na.omit(v)))) else ""
    }, character(1))

    tabla_df <- data.frame(
      Variable       = d$name,
      Tipo           = tipo,
      `Categorías`   = niveles,
      `Ausentes (%)` = sprintf("%.1f", 100 * as.numeric(d$n_missing)),
      Media          = formatear(d$Mean),
      Mediana        = formatear(d$Median),
      `Desv. típica` = formatear(d$SD),
      check.names = FALSE
    )

    # Encabezado: "interestelar_100: 104 casos × 28 variables"
    cab  <- x[["_heading"]]
    dims <- as.integer(regmatches(cab$subtitle %||% "",
                                  gregexpr("[0-9]+", cab$subtitle %||% ""))[[1]])
    texto_cab <- if (length(dims) == 2) {
      sprintf("\\noindent{\\small\\sffamily\\textbf{%s}: %d casos $\\times$ %d variables}\n\n",
              gsub("_", "\\\\_", as.character(cab$title)), dims[1], dims[2])
    } else ""

    # Tabla normal (no longtable) para que pdf/filtro.lua pueda mantenerla
    # en la misma página que su rótulo; el estilo (filas alternas, etc.) lo
    # pone el filtro, igual que en el resto de tablas.
    tabla <- knitr::kable(tabla_df, format = "latex", booktabs = TRUE,
                          linesep = "",
                          align = c("l", "l", "c", "r", "r", "r", "r")) |>
      kableExtra::kable_styling(font_size = 9, position = "center")

    impreso <- knitr::knit_print(tabla)
    knitr::asis_output(paste0(texto_cab, impreso),
                       meta = attr(impreso, "knit_meta"))
  }

  `%||%` <- function(a, b) if (is.null(a)) b else a
  registerS3method("knit_print", "gt_tbl", knit_print_gt_pdf,
                   envir = asNamespace("knitr"))
})
