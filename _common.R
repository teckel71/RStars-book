# _common.R
# Funciones auxiliares compartidas por todos los capítulos del libro.
# Cárgalo desde el chunk `setup` de cada capítulo con:  source("_common.R")

#' Inserta un vídeo de YouTube adaptado al formato de salida.
#'
#' En salida HTML embebe el vídeo en un iframe responsive al 100% del ancho
#' disponible, manteniendo la proporción 16:9.
#' En cualquier otro formato (PDF, EPUB, Word...) sustituye el vídeo por un
#' enlace clicable al mismo, para no dejar huecos vacíos ni provocar errores.
#'
#' Uso, dentro de un chunk con `results='asis'`:
#'   insertar_youtube("jzyDcOVfXMQ", "Título descriptivo del vídeo")
#'
#' @param id      ID del vídeo de YouTube (la parte tras `?v=` o `/embed/`).
#' @param titulo  Título descriptivo (accesibilidad, SEO y texto del enlace).
insertar_youtube <- function(id, titulo = "Ver vídeo en YouTube") {
  url_embed <- paste0("https://www.youtube.com/embed/", id)
  url_watch <- paste0("https://youtu.be/", id)

  if (knitr::is_html_output()) {
    titulo_esc <- htmltools::htmlEscape(titulo)
    cat(sprintf(
'<div style="position: relative; padding-bottom: 56.25%%; height: 0; overflow: hidden; max-width: 100%%; margin: 1em 0;">
  <iframe src="%s"
    title="%s"
    frameborder="0"
    allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
    referrerpolicy="strict-origin-when-cross-origin"
    allowfullscreen
    style="position: absolute; top: 0; left: 0; width: 100%%; height: 100%%;">
  </iframe>
</div>',
      url_embed, titulo_esc
    ))
  } else {
    # Fallback para PDF, EPUB, Word, etc.
    cat(sprintf("\n> **Vídeo:** [%s](%s)\n", titulo, url_watch))
  }
}
