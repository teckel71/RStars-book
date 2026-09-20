#!/usr/bin/env Rscript
# =====================================================================
# render.R - Wrapper de renderizado del libro con pre-procesado.
# ---------------------------------------------------------------------
# Para cada uso:
#   1. Crea (o limpia) una carpeta _build/ con los ficheros del libro.
#   2. Copia config, assets, y los .Rmd numerados ya procesados por
#      numerar_tablas_figuras.R.
#   3. Lanza bookdown::render_book() desde _build/, dejando el resultado
#      en _book/ junto al proyecto.
#
# El número de capítulo se infiere del prefijo del nombre del fichero,
# por ejemplo "05-analisis.Rmd" -> capítulo 5.
#
# Uso:
#   Rscript render.R                 # Formato por defecto
#   Rscript render.R bookdown::pdf_book
# =====================================================================

source("numerar_tablas_figuras.R")

# --- 1. Configuración --------------------------------------------------

# Carpeta de trabajo donde se prepara y renderiza el libro
build_dir  <- "_build"

# Carpeta de salida del libro. Si _bookdown.yml define `output_dir`, ese
# valor manda (se respeta el flujo existente del proyecto). Si no, _book.
output_dir <- "_book"
if (file.exists("_bookdown.yml") &&
    requireNamespace("yaml", quietly = TRUE)) {
  yml_data <- yaml::read_yaml("_bookdown.yml")
  if (!is.null(yml_data$output_dir)) output_dir <- yml_data$output_dir
}

# Ficheros de configuración / portada / preámbulo que hay que llevar
# tal cual a _build/. Si tu proyecto usa otros nombres, edítalos aquí.
config_files <- c(
  "index.Rmd",          # capítulo 0, lleva el YAML de cabecera
  "_bookdown.yml",
  "_output.yml",
  "preamble.tex",
  "style.css",
  "_common.R",
  "DESCRIPTION"
)

# Subcarpetas con assets (imágenes, datos, descargas, hojas de estilo...).
# IMPORTANTE: "assets" debe figurar aquí, porque _output.yml declara
# `css: assets/styles.css`. Si la carpeta no se copia a _build/, bookdown
# no encuentra la hoja de estilos, no da ningún error, y el libro sale
# con el tema desnudo (otra tipografía y otro tamaño de letra).
asset_dirs <- c(
  "assets",
  "figuras",
  "images",
  "data",
  "download",
  "css",
  "js"
)

# Dominios cuyos autoenlaces de downlit se desactivan tras renderizar.
# downlit enlaza automaticamente paquetes y funciones; los que tienen
# sitio pkgdown (tidyverse, knitr, kableExtra...) funcionan bien, pero
# los que no caen en rdrr.io o, en el caso de MATrstars, en URLs
# inexistentes. Anade aqui cualquier otro dominio que no te convenza.
# Con SIMULAR_AUTOENLACES <- TRUE no se modifica nada: solo se imprime
# el inventario de dominios, util para decidir que vetar.
DOMINIOS_VETADOS    <- c("rdrr.io",
                         "opensci.org",
                         "stats.ox.ac.uk")
SIMULAR_AUTOENLACES <- FALSE

# Formato de salida. Si no se pasa por argumento, se deja NULL para que
# bookdown use el primer formato definido en _output.yml (esto preserva
# bs4_book, gitbook, pdf_book... según lo que tengas configurado en el
# proyecto).
args <- commandArgs(trailingOnly = TRUE)
formato <- if (length(args) >= 1) args[1] else NULL

# --- 2. Limpiar y crear _build/ ---------------------------------------

if (dir.exists(build_dir)) unlink(build_dir, recursive = TRUE)
dir.create(build_dir)

cat(sprintf("[1/4] Carpeta '%s/' creada.\n", build_dir))

# --- 3. Copiar config y assets ----------------------------------------

copiados_config <- 0
for (f in config_files) {
  if (file.exists(f)) {
    file.copy(f, file.path(build_dir, f), overwrite = TRUE)
    copiados_config <- copiados_config + 1
  }
}

copiados_assets <- 0
for (d in asset_dirs) {
  if (dir.exists(d)) {
    file.copy(d, build_dir, recursive = TRUE)
    copiados_assets <- copiados_assets + 1
  }
}

# Comprobacion: verificar que las hojas de estilo declaradas en
# _output.yml han llegado realmente a _build/. Un `css:` que apunta a un
# fichero inexistente NO aborta el render: simplemente se ignora, y el
# libro sale sin los estilos propios. Mejor avisar en voz alta.
if (file.exists("_output.yml") &&
    requireNamespace("yaml", quietly = TRUE)) {
  out_yml <- try(yaml::read_yaml("_output.yml"), silent = TRUE)
  if (!inherits(out_yml, "try-error")) {
    css_decl <- unlist(lapply(out_yml, function(fmt) {
      if (is.list(fmt) && !is.null(fmt$css)) fmt$css else NULL
    }), use.names = FALSE)
    for (css in unique(css_decl)) {
      if (file.exists(file.path(build_dir, css))) {
        cat(sprintf("      CSS localizado: %s\n", css))
      } else {
        warning("La hoja de estilos declarada en _output.yml (\"", css,
                "\") NO esta en ", build_dir, "/. El libro se generara ",
                "SIN esos estilos. Revisa `asset_dirs` o la ruta del `css:`.",
                call. = FALSE, immediate. = TRUE)
      }
    }
  }
}

# Ficheros sueltos de datos en la raíz (xlsx, csv, bib, etc.) que los chunks
# o pandoc podrían requerir por nombre simple sin path.
data_files <- list.files(
  pattern = "\\.(xlsx|xls|csv|tsv|rds|RData|rda|txt|json|parquet|bib|csl)$",
  ignore.case = TRUE
)
copiados_datos <- 0
for (f in data_files) {
  file.copy(f, file.path(build_dir, f), overwrite = TRUE)
  copiados_datos <- copiados_datos + 1
}

cat(sprintf("[2/4] Copiados %d archivos de config, %d carpetas de assets y %d ficheros de datos.\n",
            copiados_config, copiados_assets, copiados_datos))

# --- 4. Procesar Rmd numerados ---------------------------------------

# Ficheros de capítulo: empiezan por dígitos y terminan en .Rmd
rmds <- list.files(pattern = "^[0-9]+.*\\.Rmd$")

if (length(rmds) == 0) {
  warning("No se encontraron ficheros tipo 'NN-titulo.Rmd' en la raíz.")
}

for (f in rmds) {
  procesar_rmd(f,
               ruta_salida = file.path(build_dir, f),
               verbose = FALSE)
}

cat(sprintf("[3/4] Procesados %d capítulos.\n", length(rmds)))

# --- 5. Renderizar ----------------------------------------------------

cat(sprintf("[4/4] Renderizando con %s ...\n",
            if (is.null(formato)) "formato del _output.yml" else formato))

# Antes de renderizar, cambiamos el directorio de trabajo a _build/ para
# que knitr resuelva los paths relativos de los chunks (lecturas de
# Excel, CSVs, etc.) desde ahí.
#
# Guardamos el directorio del proyecto en `.proyecto_dir` (con punto
# inicial). Los chunks del libro suelen ejecutar `rm(list = ls())`, que
# NO afecta a nombres ocultos (los que empiezan por punto). Así nuestra
# variable sobrevive al render aunque algún chunk limpie el global env.

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
output_dir_abs <- normalizePath(output_dir)
.proyecto_dir <- normalizePath(getwd())

# Encapsulamos en función para que `on.exit` funcione correctamente
# (a diferencia de cuando se usa a nivel top-level con `source()`).
render_libro <- function(build_dir, formato, output_dir_abs) {
  on.exit(setwd(.proyecto_dir), add = TRUE)
  setwd(build_dir)
  bookdown::render_book(
    input         = ".",
    output_format = formato,
    output_dir    = output_dir_abs
  )
}

render_libro(build_dir, formato, output_dir_abs)

# --- 6. Limpieza de autoenlaces de downlit ----------------------------
#
# El formato bs4_book usa el paquete downlit, que enlaza automaticamente
# el codigo en linea y el de los bloques resaltados. Reconoce, entre
# otras formas, `fun()`, `pkg::fun()`, `library(pkg)` y `{pkg}`. Cuando
# encuentra un sitio pkgdown del paquete enlaza alli (tidyverse, knitr,
# kableExtra...), pero si no lo encuentra recurre a rdrr.io, y con los
# paquetes que no estan publicados (MATrstars) genera URLs que no
# existen.
#
# Esta funcion recorre el HTML ya generado y DESACTIVA unicamente los
# autoenlaces cuyo destino figure en `dominios_vetados`, dejando intacto
# el resto:
#
#   - Una funcion dentro de codigo  ->  se queda como texto: kable_rstars()
#   - Un paquete citado como {pkg}  ->  recupera las llaves: {MATrstars}
#   - Los enlaces propios del autor ->  NO se tocan nunca (solo se actua
#     sobre anclas que esten dentro de <code> o <pre>, o cuyo texto sea
#     exactamente el nombre de un paquete citado con llaves en el fuente)
#
# `simular = TRUE` no modifica nada: solo imprime el inventario de
# dominios encontrados, para decidir con datos que vetar.

limpiar_autoenlaces <- function(dir_html,
                                dominios_vetados = c("rdrr.io"),
                                paquetes         = character(0),
                                simular          = FALSE,
                                verbose          = TRUE) {

  if (!requireNamespace("xml2", quietly = TRUE)) {
    warning("El paquete xml2 no esta disponible: se omite la limpieza ",
            "de autoenlaces.")
    return(invisible(NULL))
  }

  ficheros <- list.files(dir_html, pattern = "\\.html$", full.names = TRUE)
  if (length(ficheros) == 0) {
    if (verbose) cat("     (sin ficheros .html que revisar)\n")
    return(invisible(NULL))
  }

  dominio_de <- function(href) sub("^https?://([^/]+).*$", "\\1", href)
  vetado <- function(dom) {
    any(vapply(dominios_vetados,
               function(d) grepl(d, dom, fixed = TRUE), logical(1)))
  }

  inventario <- character(0)
  n_fun <- 0L; n_paq <- 0L; n_fich <- 0L

  for (f in ficheros) {
    doc     <- xml2::read_html(f, encoding = "UTF-8")
    anclas  <- xml2::xml_find_all(doc, ".//a[@href]")
    tocado  <- FALSE

    for (a in anclas) {
      href <- xml2::xml_attr(a, "href")
      if (is.na(href) || !grepl("^https?://", href)) next   # enlace interno

      dom <- dominio_de(href)

      # Solo nos interesan las anclas que downlit ha generado: las que
      # viven dentro de codigo, o las que sustituyen a un `{paquete}`.
      ancestro  <- xml2::xml_find_first(a, "ancestor::code|ancestor::pre")
      en_codigo <- !inherits(ancestro, "xml_missing")
      texto     <- xml2::xml_text(a)
      es_paquete <- (!en_codigo) && (texto %in% paquetes)

      if (!en_codigo && !es_paquete) next   # enlace propio del autor

      inventario <- c(inventario, dom)
      if (simular || !vetado(dom)) next

      # Quitar el enlace conservando el texto. Se renombra el nodo en
      # lugar de eliminarlo, de modo que la posicion y el contenido se
      # mantienen exactamente igual.
      xml2::xml_set_attr(a, "href",  NULL)
      xml2::xml_set_attr(a, "class", NULL)
      if (es_paquete) {
        xml2::xml_set_name(a, "code")
        xml2::xml_text(a) <- paste0("{", texto, "}")
        n_paq <- n_paq + 1L
      } else {
        xml2::xml_set_name(a, "span")
        n_fun <- n_fun + 1L
      }
      tocado <- TRUE
    }

    if (tocado && !simular) {
      xml2::write_html(doc, f)
      n_fich <- n_fich + 1L
    }
  }

  if (verbose) {
    if (length(inventario) == 0) {
      cat("     No se han encontrado autoenlaces de downlit.\n")
    } else {
      tab <- sort(table(inventario), decreasing = TRUE)
      cat("     Autoenlaces por dominio:\n")
      for (k in names(tab)) {
        marca <- if (vetado(k)) "  [VETADO]" else ""
        cat(sprintf("       %-42s %5d%s\n", k, tab[[k]], marca))
      }
    }
    if (simular) {
      cat("     (modo simulacion: no se ha modificado ningun fichero)\n")
    } else {
      cat(sprintf("     Enlaces desactivados: %d en codigo, %d de paquete. Ficheros reescritos: %d.\n",
                  n_fun, n_paq, n_fich))
    }
  }

  invisible(list(dominios = table(inventario),
                 n_funciones = n_fun, n_paquetes = n_paq,
                 n_ficheros = n_fich))
}

# Lista de paquetes citados en el fuente con la convencion `{paquete}`.
# Se extrae de los propios .Rmd, de modo que no hay que mantenerla a mano.
paquetes_citados <- unique(unlist(lapply(rmds, function(f) {
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  m <- regmatches(txt, gregexpr("`\\{[A-Za-z][A-Za-z0-9._]*\\}`", txt))
  gsub("[`{}]", "", unlist(m))
})))

cat(sprintf("[5/5] Depurando autoenlaces (%d paquetes citados con llaves)...\n",
            length(paquetes_citados)))

limpiar_autoenlaces(
  dir_html         = output_dir_abs,
  dominios_vetados = DOMINIOS_VETADOS,
  paquetes         = paquetes_citados,
  simular          = SIMULAR_AUTOENLACES
)

cat(sprintf("\n[OK] Libro renderizado en '%s/'\n", output_dir))
