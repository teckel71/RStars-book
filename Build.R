# build.R  (guárdalo en la carpeta del proyecto)
# =====================================================================
# Compila el libro y lo deja listo para publicar en GitHub Pages.
#
# IMPORTANTE: este script ya NO llama directamente a bookdown. Delega en
# render.R, que es el que:
#   1. Pasa cada capítulo por numerar_tablas_figuras.R (numeración
#      manual de tablas y figuras).
#   2. Prepara _build/ con la config y los assets (incluida la carpeta
#      assets/, donde vive styles.css).
#   3. Renderiza a docs/.
#   4. Depura los autoenlaces de downlit que apuntan a sitios vetados.
#
# Antes, build.R llamaba a `bookdown::render_book()` por su cuenta desde
# la raíz del proyecto. Eso producía un libro DISTINTO: sin numeración
# manual de tablas y figuras, y sin depuración de autoenlaces. Tener dos
# rutas de compilación que dan resultados diferentes es una fuente segura
# de confusión, así que ahora hay una sola.
# =====================================================================

build_book <- function(formato = NULL) {

  # --------------------------------------------------------------------
  # 1. Compilación completa a través de render.R
  # --------------------------------------------------------------------
  if (!file.exists("render.R")) {
    stop("No se encuentra render.R en la carpeta del proyecto. ",
         "build_book() depende de él.")
  }

  # render.R toma el formato del primer argumento de la línea de
  # comandos. Al hacer source() no hay argumentos, así que usará el
  # primer formato de _output.yml (bs4_book). Si se pasa `formato`, lo
  # forzamos enmascarando commandArgs() mientras dure la compilación.
  if (!is.null(formato)) {
    assign("commandArgs",
           function(trailingOnly = FALSE) formato,
           envir = globalenv())
    on.exit(suppressWarnings(rm("commandArgs", envir = globalenv())),
            add = TRUE)
  }

  source("render.R", local = FALSE, encoding = "UTF-8")

  # --------------------------------------------------------------------
  # 2. Asegurar que la carpeta de figuras se copia SIEMPRE a docs/
  #
  # bookdown genera las figuras junto al .Rmd fusionado, que con este
  # flujo vive en _build/. A veces no completa el copiado a docs/ (es
  # frecuente cuando el proyecto está en una carpeta sincronizada con
  # OneDrive: el bloqueo de archivos durante la sincronización puede
  # interrumpirlo). Por eso buscamos en los dos sitios posibles.
  # --------------------------------------------------------------------

  candidatos <- c("_build/RStarS_files/figure-html",
                  "RStarS_files/figure-html")
  existentes <- candidatos[dir.exists(candidatos)]
  fig_dst    <- "docs/RStarS_files/figure-html"

  if (length(existentes) > 0) {

    fig_src <- existentes[1]
    if (!dir.exists(fig_dst)) dir.create(fig_dst, recursive = TRUE)

    archivos <- list.files(fig_src, full.names = TRUE)
    ok <- file.copy(archivos, fig_dst, overwrite = TRUE)

    if (!all(ok)) {
      warning(sum(!ok), " figura(s) no se pudieron copiar a docs/. ",
              "Revisa si OneDrive las tiene bloqueadas y vuelve a ",
              "ejecutar build_book().", call. = FALSE)
    } else {
      message(length(archivos), " figura(s) copiadas correctamente a ",
              fig_dst, " (origen: ", fig_src, ").")
    }

  } else {
    message("Aviso: no se encontró la carpeta de figuras ni en '_build/' ",
            "ni en la raíz. Si el libro no usa gráficos generados por ",
            "chunks, ignora este aviso.")
  }

  # --------------------------------------------------------------------
  # 3. GitHub Pages: evitar el procesado Jekyll
  # --------------------------------------------------------------------
  if (!dir.exists("docs")) dir.create("docs", recursive = TRUE)
  if (!file.exists("docs/.nojekyll")) file.create("docs/.nojekyll")

  # --------------------------------------------------------------------
  # 4. Comprobación final: la hoja de estilos debe haber llegado a docs/
  # --------------------------------------------------------------------
  css_docs <- list.files("docs", pattern = "\\.css$", recursive = TRUE)
  if (length(css_docs) == 0) {
    warning("No hay ninguna hoja de estilos en docs/. El libro se verá ",
            "con el tema por defecto. Comprueba que \"assets\" figura en ",
            "`asset_dirs` dentro de render.R.", call. = FALSE)
  } else {
    message("Hojas de estilo publicadas: ",
            paste(css_docs, collapse = ", "), ".")
  }

  message("Libro compilado, figuras sincronizadas y .nojekyll listo en ",
          "docs/. Ahora haz Commit + Push en GitHub Desktop.")
}

build_book()
