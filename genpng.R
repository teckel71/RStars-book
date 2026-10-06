# =============================================================================
#  Genera las dos figuras del apartado 6.5 (correlaciones parciales y KMO).
#  Ejecutar desde la raíz del proyecto; deja los PNG en figuras/.
#  Solo hay que volver a ejecutarlo si cambian los datos o el filtrado.
# =============================================================================
suppressMessages({library(readxl); library(dplyr); library(ggplot2); library(patchwork)})

# Mismos datos y mismo filtrado de outliers que usa el capitulo.
  datos <- read_excel("interestelar_100.xlsx", sheet = "Datos")
  d <- datos[, c("IDIVERSE", "IFIDE", "IDIG")]
  d[] <- lapply(d, function(x) as.numeric(as.character(x)))  # por si vienen como texto
  d <- d[complete.cases(d), ]
  M <- mahalanobis(d, colMeans(d), cov(d))
  q <- quantile(M, c(.25, .75)); iqr <- q[2] - q[1]
  seleccion_so <- as.data.frame(d[M <= q[2] + 1.5*iqr & M >= q[1] - 1.5*iqr, ])

# ===== FIGURA 1: correlacion parcial, en VERTICAL =====
  rA <- resid(lm(IFIDE ~ IDIVERSE, data = seleccion_so))
  rB <- resid(lm(IDIG  ~ IDIVERSE, data = seleccion_so))
  r0 <- cor(seleccion_so$IFIDE, seleccion_so$IDIG)
  rp <- cor(rA, rB)
  dr <- data.frame(rA = rA, rB = rB)
  mk <- function(p, tit, sub, xl, yl) p +
    geom_smooth(method = "lm", se = FALSE, colour = "red3", linewidth = .7) +
    ggtitle(tit, subtitle = sub) + xlab(xl) + ylab(yl) +
    theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())
  g1 <- mk(ggplot(seleccion_so, aes(IFIDE, IDIG)) +
             geom_point(colour = "steelblue4", size = 1.6, alpha = .7),
           "Correlación simple", sprintf("r = %.3f", r0), "IFIDE", "IDIG")
  g2 <- mk(ggplot(dr, aes(rA, rB)) +
             geom_point(colour = "steelblue4", size = 1.6, alpha = .7),
           "Correlación parcial", sprintf("r = %.3f", rp),
           "Residuos de IFIDE | IDIVERSE", "Residuos de IDIG | IDIVERSE")
  # Para que las dos areas de dibujo queden exactamente alineadas pese a tener
  # etiquetas de eje Y de distinta longitud, se convierten a "gtable" y se
  # igualan sus anchuras columna a columna antes de apilarlas.
  gt1 <- ggplotGrob(g1); gt2 <- ggplotGrob(g2)
  ancho <- grid::unit.pmax(gt1$widths, gt2$widths)
  gt1$widths <- ancho; gt2$widths <- ancho
  png("figuras/correlacion_parcial.png", width = 6.5, height = 7,
      units = "in", res = 150)
  grid::grid.draw(rbind(gt1, gt2, size = "first"))
  invisible(dev.off())

# ===== FIGURA 2: estructura comun vs parejas. MISMAS 3 variables en ambos lados =====
  marcoK <- function(p, tit, sub) p + ggtitle(tit, subtitle = sub) +
    coord_cartesian(xlim = c(.1, 3.9), ylim = c(.4, 4.2)) +
    theme_void(base_size = 11) +
    theme(plot.title = element_text(size = 12, face = "bold"),
          plot.subtitle = element_text(size = 10, colour = "grey30", lineheight = 1.1))
  cajaK <- function(df, relleno, tinta, formula = FALSE)
    geom_label(data = df, aes(x, y, label = lab), parse = formula,
               fill = relleno, colour = tinta, size = 4.2,
               label.padding = unit(.32, "lines"),
               label.r = unit(.12, "lines"), label.size = .4)
  flechaK <- function(x0, y0, x1, y1)
    annotate("segment", x = x0, y = y0, xend = x1, yend = y1,
             colour = "steelblue4", linewidth = .8,
             arrow = arrow(length = unit(.18, "cm")))

  vA <- data.frame(x = c(.7, 2.0, 3.3), y = 1.4, lab = c("x[1]","x[2]","x[3]"))
  nA <- data.frame(x = 2.0, y = 3.3, lab = "estructura\ncomún")
  pA <- marcoK(ggplot() +
      flechaK(2, 2.85, .75, 1.8) + flechaK(2, 2.85, 2, 1.8) + flechaK(2, 2.85, 3.25, 1.8) +
      cajaK(nA, "grey92", "steelblue4") + cajaK(vA, "white", "grey20", TRUE),
    "Una estructura común",
    "Las parciales se desploman al descontar\nel resto  →  KMO alto")

  # Panel derecho: LAS MISMAS x1, x2, x3. Una pareja (x1-x2) y la tercera suelta.
  # Mismas posiciones x que el panel izquierdo, para que ambos queden centrados.
  vB <- data.frame(x = c(.7, 2.0, 3.3), y = 2.3, lab = c("x[1]","x[2]","x[3]"))
  pB <- marcoK(ggplot() +
      annotate("segment", x = .95, xend = 1.75, y = 2.3, yend = 2.3,
               colour = "red3", linewidth = .9) +
      annotate("text", x = 3.3, y = 1.55, label = "(sin relación)",
               colour = "grey45", size = 3.4, fontface = "italic") +
      cajaK(vB, "white", "grey20", TRUE),
    "Una pareja aislada",
    "x1 y x2 se relacionan solo entre sí.\nDescontar x3 no cambia nada  →  KMO bajo")
  ggsave("figuras/estructura_vs_parejas.png", pA / pB, width = 6, height = 6, dpi = 150)
  cat("simple:", round(r0,3), "parcial:", round(rp,3), "cae:", round(100*(1-rp/r0)), "%\n")
