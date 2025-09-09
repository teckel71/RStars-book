# build.R  (guárdalo en la carpeta del proyecto)
build_book <- function() {
  bookdown::render_book("index.Rmd", "bookdown::bs4_book")
  if (!dir.exists("docs")) dir.create("docs", recursive = TRUE)
  if (!file.exists("docs/.nojekyll")) file.create("docs/.nojekyll")
  message("Libro compilado y .nojekyll listo en docs/. Ahora haz Commit + Push en GitHub Desktop.")
}

build_book()
