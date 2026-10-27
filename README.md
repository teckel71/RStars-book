# R-Stars: La Guía

Manual de R y técnicas multivariantes, de Miguel Ángel Tarancón Morán
(Universidad de Castilla-La Mancha).

- Libro en línea: <https://teckel71.github.io/RStars-book/>
- Licencia: CC BY-NC-ND 4.0 (véase `LICENSE.md`).

## Estructura del proyecto

| Elemento | Contenido |
|---|---|
| `index.Rmd`, `NN-*.Rmd` | Fuentes del libro (portada, capítulos y apéndices) |
| `_bookdown.yml`, `_output.yml` | Configuración de bookdown (salida en `docs/`) |
| `_common.R` | Funciones auxiliares que cargan los capítulos |
| `Build.R` | **Punto de entrada para compilar** el libro |
| `render.R`, `numerar_tablas_figuras.R` | Flujo de compilación (los usa `Build.R`) |
| `assets/styles.css` | Hoja de estilos del libro web |
| `figuras/`, `images/` | Imágenes del libro |
| `download/` | Material de prácticas para los estudiantes (scripts y Excel). **Fuente única de los datos**: el libro también los lee de aquí |
| `lola.bib`, `packages.bib`, `apa.csl` | Bibliografía |
| `preamble.tex` | Diseño de la versión PDF (A4): tipografía, márgenes, títulos, código |
| `pdf/` | Filtro, portada y ajustes de R que solo usa la versión PDF |
| `herramientas/` | Scripts auxiliares (generación de figuras didácticas, ids de encabezados). Se ejecutan desde la raíz, p. ej. `source("herramientas/gen7.R")` |
| `docs/` | Libro publicado (GitHub Pages), incluido `RStars-La-Guia.pdf`. Se genera, no se edita a mano |

## Compilar

En RStudio, con el proyecto abierto: `source("Build.R")`. Genera la web y,
si `generar_pdf <- TRUE` (al principio de `Build.R`), también el PDF en A4,
enlazado desde la portada de la web. El PDF necesita LaTeX
(`tinytex::install_tinytex()`). Si el PDF falla, la web se genera igual y
el diagnóstico está en `_build/RStarS.log`.
Después, *commit* y *push* (GitHub Desktop) para publicar.

Las carpetas `_build/` y `_pdf/` son temporales y se regenera en cada compilación.
