# build.R  (guárdalo en la carpeta del proyecto)

build_book <- function() {

  bookdown::render_book("index.Rmd", "bookdown::bs4_book")

  # --------------------------------------------------------------------
  # Asegurar que la carpeta de figuras (generada en la raíz del proyecto,
  # junto al RStarS.Rmd fusionado) se copia SIEMPRE a docs/, por si
  # bookdown no completa ese paso internamente (frecuente cuando el
  # proyecto vive en una carpeta sincronizada con OneDrive: el bloqueo de
  # archivos durante la sincronización puede interrumpir el copiado).
  # --------------------------------------------------------------------

  fig_src <- "RStarS_files/figure-html"
  fig_dst <- "docs/RStarS_files/figure-html"

  if (dir.exists(fig_src)) {

    if (!dir.exists(fig_dst)) dir.create(fig_dst, recursive = TRUE)

    archivos <- list.files(fig_src, full.names = TRUE)
    ok <- file.copy(archivos, fig_dst, overwrite = TRUE)

    if (!all(ok)) {
      warning(sum(!ok), " figura(s) no se pudieron copiar a docs/. ",
              "Revisa si OneDrive las tiene bloqueadas y vuelve a ejecutar build_book().")
    } else {
      message(length(archivos), " figura(s) copiadas correctamente a ", fig_dst, ".")
    }

  } else {
    message("Aviso: no se encontró la carpeta '", fig_src,
            "' en la raíz. Si el libro no usa gráficos generados por chunks, ignora este aviso.")
  }

  if (!dir.exists("docs")) dir.create("docs", recursive = TRUE)
  if (!file.exists("docs/.nojekyll")) file.create("docs/.nojekyll")

  message("Libro compilado, figuras sincronizadas y .nojekyll listo en docs/. ",
          "Ahora haz Commit + Push en GitHub Desktop.")
}

build_book()
