# =====================================================================
#  anadir_ids_encabezados.R
#
#  Añade identificadores explícitos {#id} a los encabezados de los .Rmd
#  de un libro bookdown, para que el menú lateral derecho (ScrollSpy de
#  Bootstrap) resalte el apartado activo y despliegue los subapartados.
#
#  - Deja INTACTOS los encabezados de nivel 1 (#), es decir, los títulos
#    de capítulo: así no cambian los nombres de los archivos HTML ni las
#    URLs ya publicadas.
#  - Solo toca los de nivel 2 en adelante (##, ###, ####...).
#  - Ignora los comentarios de R dentro de los bloques de código
#    (esas líneas que empiezan por # dentro de ```{r ... }```).
#  - Ignora la cabecera YAML.
#  - Respeta los encabezados que YA tengan un {#id}.
#
#  USO:
#    1. Copia este archivo a la RAÍZ del proyecto (junto a _bookdown.yml).
#    2. Ábrelo en RStudio con el proyecto abierto y ejecútalo entero
#       (Ctrl+Shift+Enter) con SIMULAR = TRUE. No modifica nada: solo
#       imprime la lista de ids que propondría y la guarda en
#       ids_propuestos.csv.
#    3. Revisa esa lista. Si te convence, cambia SIMULAR a FALSE y vuelve
#       a ejecutar. Antes de escribir, hace una copia de seguridad de
#       todos los .Rmd en una carpeta _backup_rmd_AAAAMMDD_HHMMSS/.
#    4. Compila el libro y comprueba el resultado.
# =====================================================================


# ---------------------------------------------------------------------
# OPCIONES
# ---------------------------------------------------------------------

SIMULAR <- FALSE          # TRUE = solo informa; FALSE = modifica los .Rmd

carpeta <- "."           # carpeta con los .Rmd (la raíz del proyecto)

nivel_minimo <- 2        # nivel mínimo de encabezado al que poner id.
                         # 2 = desde ## en adelante (deja los # intactos)

prefijo <- "numero"      # cómo prefijar los ids para que sean únicos
                         # en todo el libro:
                         #   "numero"  -> c04-medidas-de-concentracion
                         #   "nombre"  -> descriptiva-medidas-de-concentracion
                         #   "ninguno" -> medidas-de-concentracion

max_long <- 45           # longitud máxima del texto del id (sin prefijo)

copia_seguridad <- TRUE  # crear carpeta de backup antes de modificar

excluir <- c("index.Rmd", "99-licencia.Rmd")           # .Rmd que no quieres tocar,
                         # p. ej. c("index.Rmd", "99-licencia.Rmd")


# ---------------------------------------------------------------------
# FUNCIONES AUXILIARES
# ---------------------------------------------------------------------

# Convierte el texto de un título en un slug ASCII válido como id CSS
slug <- function(txt) {

  x <- txt

  # 1) quitar imágenes ![alt](ruta){attr} y sus atributos
  x <- gsub("!\\[[^]]*\\]\\([^)]*\\)(\\{[^}]*\\})?", " ", x)
  # 2) enlaces [texto](url) -> texto
  x <- gsub("\\[([^]]*)\\]\\([^)]*\\)", "\\1", x)
  # 3) notas al pie ^[...]
  x <- gsub("\\^\\[[^]]*\\]", " ", x)
  # 4) etiquetas HTML
  x <- gsub("<[^>]*>", " ", x)
  # 5) fórmulas $...$
  x <- gsub("\\$[^$]*\\$", " ", x)
  # 6) marcas de código, énfasis y comillas
  x <- gsub("[`*_~\"'\u201c\u201d\u2018\u2019]", "", x)

  # 7) transliterar acentos y caracteres propios del castellano
  de <- "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1\u00e0\u00e8\u00ec\u00f2\u00f9\u00e2\u00ea\u00ee\u00f4\u00fb\u00e7\u00c1\u00c9\u00cd\u00d3\u00da\u00dc\u00d1\u00c0\u00c8\u00cc\u00d2\u00d9\u00c2\u00ca\u00ce\u00d4\u00db\u00c7"
  a  <- "aeiouunaeiouaeiouacAEIOUUNAEIOUAEIOUAC"
  x <- chartr(de, a, x)

  # 8) minúsculas y sustitución de todo lo que no sea [a-z0-9] por "-"
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)

  # 9) recortar a max_long sin partir palabras
  if (nchar(x) > max_long) {
    corte <- substr(x, 1, max_long)
    if (grepl("-", corte)) corte <- sub("-[^-]*$", "", corte)
    x <- corte
  }
  x <- gsub("^-+|-+$", "", x)

  if (x == "") x <- "seccion"
  x
}

# Prefijo del archivo: "04-descriptiva.Rmd" -> "c04" o "descriptiva"
prefijo_de <- function(archivo) {
  base <- sub("\\.[Rr][Mm][Dd]$", "", basename(archivo))
  if (prefijo == "numero") {
    num <- regmatches(base, regexpr("^[0-9]+", base))
    if (length(num) == 1 && nzchar(num)) return(paste0("c", num))
    return(slug(base))
  }
  if (prefijo == "nombre") {
    return(slug(sub("^[0-9]+[-_]*", "", base)))
  }
  ""
}

# ¿La línea abre o cierra un bloque de código (```{r} o ```)?
es_valla <- function(linea) grepl("^\\s*(```|~~~)", linea)


# ---------------------------------------------------------------------
# PROCESO
# ---------------------------------------------------------------------

archivos <- list.files(carpeta, pattern = "\\.[Rr][Mm][Dd]$", full.names = TRUE)
archivos <- archivos[!basename(archivos) %in% excluir]
archivos <- sort(archivos)

if (length(archivos) == 0) stop("No se han encontrado archivos .Rmd en: ", normalizePath(carpeta))

# ids ya existentes en el libro, para no duplicar
ids_usados <- character(0)
for (f in archivos) {
  con <- file(f, encoding = "UTF-8"); l <- readLines(con, warn = FALSE); close(con)
  ya <- regmatches(l, regexpr("\\{#[A-Za-z][A-Za-z0-9_:.-]*", l))
  ids_usados <- c(ids_usados, sub("\\{#", "", ya))
}

informe <- data.frame(archivo = character(0), linea = integer(0),
                      nivel = integer(0), titulo = character(0),
                      id = character(0), stringsAsFactors = FALSE)

modificados <- character(0)
nuevo_contenido <- list()

for (f in archivos) {

  # lectura conservando el tipo de salto de línea original
  crudo <- readBin(f, "raw", file.info(f)$size)
  eol <- if (any(crudo == as.raw(13))) "\r\n" else "\n"
  con <- file(f, encoding = "UTF-8"); lineas <- readLines(con, warn = FALSE); close(con)

  en_chunk <- FALSE
  en_yaml  <- FALSE
  cambios  <- 0

  for (i in seq_along(lineas)) {

    linea <- lineas[i]

    # cabecera YAML (solo al principio del archivo)
    if (grepl("^---\\s*$", linea)) {
      if (!en_chunk && i <= 3 && !en_yaml) { en_yaml <- TRUE; next }
      if (en_yaml) { en_yaml <- FALSE; next }
    }
    if (en_yaml) next

    # bloques de código: dentro de ellos, "#" es un comentario de R
    if (es_valla(linea)) { en_chunk <- !en_chunk; next }
    if (en_chunk) next

    # ¿es un encabezado del nivel que nos interesa?
    m <- regexec("^(#{1,6})[ \t]+(.*?)[ \t]*$", linea)
    g <- regmatches(linea, m)[[1]]
    if (length(g) == 0) next

    almohadillas <- g[2]
    texto <- g[3]
    nivel <- nchar(almohadillas)
    if (nivel < nivel_minimo) next
    if (!nzchar(texto)) next

    # ¿bloque de atributos al final? (debe ir precedido de un espacio,
    # para no confundirlo con el {.hicon} pegado a una imagen)
    attrs <- NA_character_
    texto_limpio <- texto
    ma <- regexec("^(.*\\S)[ \t]+\\{([^}]*)\\}$", texto)
    ga <- regmatches(texto, ma)[[1]]
    if (length(ga) > 0) {
      texto_limpio <- ga[2]
      attrs <- ga[3]
      # si ya tiene id, no tocamos este encabezado
      if (grepl("#[A-Za-z]", attrs)) next
    }

    # construir el id
    base_id <- slug(texto_limpio)
    pre <- prefijo_de(f)
    id <- if (nzchar(pre)) paste0(pre, "-", base_id) else base_id

    # garantizar unicidad
    if (id %in% ids_usados) {
      k <- 2
      while (paste0(id, "-", k) %in% ids_usados) k <- k + 1
      id <- paste0(id, "-", k)
    }
    ids_usados <- c(ids_usados, id)

    # reescribir la línea
    if (is.na(attrs)) {
      nueva <- paste0(almohadillas, " ", texto, " {#", id, "}")
    } else {
      resto <- trimws(attrs)
      resto <- gsub("(^|\\s)-(\\s|$)", "\\1.unnumbered\\2", resto)  # {-} -> {.unnumbered}
      nueva <- paste0(almohadillas, " ", texto_limpio, " {#", id,
                      if (nzchar(resto)) paste0(" ", resto) else "", "}")
    }

    lineas[i] <- nueva
    cambios <- cambios + 1

    informe <- rbind(informe, data.frame(
      archivo = basename(f), linea = i, nivel = nivel,
      titulo = texto_limpio, id = id, stringsAsFactors = FALSE))
  }

  if (cambios > 0) {
    modificados <- c(modificados, f)
    nuevo_contenido[[f]] <- list(lineas = lineas, eol = eol)
  }
}


# ---------------------------------------------------------------------
# SALIDA
# ---------------------------------------------------------------------

cat("\n=============================================================\n")
cat("Encabezados detectados sin id (nivel >=", nivel_minimo, "):",
    nrow(informe), "\n")
cat("Archivos afectados:", length(modificados), "\n")
cat("=============================================================\n\n")

if (nrow(informe) > 0) {
  print(utils::head(informe, 40), row.names = FALSE)
  if (nrow(informe) > 40) cat("... (", nrow(informe) - 40, " más)\n", sep = "")
  utils::write.csv(informe, "ids_propuestos.csv", row.names = FALSE,
                   fileEncoding = "UTF-8")
  cat("\nListado completo guardado en: ids_propuestos.csv\n")
}

if (SIMULAR) {
  cat("\n*** MODO SIMULACIÓN: no se ha modificado ningún archivo. ***\n")
  cat("Cambia SIMULAR <- FALSE y vuelve a ejecutar para aplicarlo.\n\n")
} else if (length(modificados) > 0) {

  if (copia_seguridad) {
    dir_bk <- format(Sys.time(), "_backup_rmd_%Y%m%d_%H%M%S")
    dir.create(dir_bk, showWarnings = FALSE)
    file.copy(archivos, dir_bk, overwrite = TRUE)
    cat("Copia de seguridad de los .Rmd en:", dir_bk, "\n")
  }

  for (f in modificados) {
    x <- nuevo_contenido[[f]]
    con <- file(f, open = "wb")
    writeLines(x$lineas, con, sep = x$eol, useBytes = TRUE)
    close(con)
  }
  cat("Archivos modificados:\n  ", paste(basename(modificados), collapse = "\n  "), "\n\n")
  cat("Listo. Compila el libro y comprueba el menú lateral derecho.\n\n")

} else {
  cat("No había nada que cambiar.\n\n")
}
