# =============================================================================
#  Figura didactica del apartado 9.3: outlier frente a caso influyente.
#  Datos construidos (no son los de TMI): en una muestra real no es posible
#  localizar a voluntad un caso con Cook muy alta y residuo diminuto.
#  Disposicion VERTICAL. Ejecutar desde la raiz; deja el PNG en figuras/.
# =============================================================================
options(bitmapType = "cairo")
suppressMessages(library(ggplot2))

  set.seed(11)
  n  <- 24
  mu <- data.frame(x = runif(n, 2, 8))
  mu$y <- 2 + 0.8 * mu$x + rnorm(n, 0, 0.55)
  b0 <- coef(lm(y ~ x, mu))

  # (2) residuo grande, en el CENTRO de la nube (leverage minimo).
  c2 <- data.frame(x = mean(mu$x), y = 2 + 0.8 * mean(mu$x) + 4.0)
  # (3) influyente clasico: alejado y fuera de la tendencia.
  c3 <- data.frame(x = 14, y = 2 + 0.8 * 14 - 5.0)
  # (4) influyente que NO se delata: tan alejado que la recta va hacia el.
  c4 <- data.frame(x = 40, y = 2 + 0.8 * 40 - 2.5)

  dg <- function(extra) {
    d <- rbind(mu, extra); m <- lm(y ~ x, d); i <- nrow(d)
    c(pend = unname(coef(m)[2])) }
  p2 <- dg(c2); p3 <- dg(c3); p4 <- dg(c4)
  f3 <- function(z) formatC(z, format = "f", digits = 3)

  panel <- function(extra, tit, sub, xmax) {
    d <- if (is.null(extra)) mu else rbind(mu, extra)
    b <- coef(lm(y ~ x, d))
    g <- ggplot() +
      geom_abline(intercept = b0[1], slope = b0[2],
                  colour = "grey60", linetype = "dashed", linewidth = 0.8) +
      geom_point(data = mu, aes(x, y), colour = "steelblue4", size = 1.9, alpha = 0.8) +
      geom_abline(intercept = b[1], slope = b[2], colour = "steelblue4", linewidth = 1)
    if (!is.null(extra))
      g <- g + geom_point(data = extra, aes(x, y), colour = "red3", size = 3.6)
    g + coord_cartesian(xlim = c(0, xmax), ylim = c(1, 2 + 0.8 * xmax + 2)) +
      ggtitle(tit, subtitle = sub) + xlab("x") + ylab("y") +
      theme_bw(base_size = 10) +
      theme(plot.title = element_text(size = 11, face = "bold"),
            plot.subtitle = element_text(size = 9.5, colour = "grey30")) }

  apilar <- function(..., archivo, w, h) {
    gl <- lapply(list(...), ggplotGrob)
    anchos <- do.call(grid::unit.pmax, lapply(gl, function(g) g$widths))
    for (i in seq_along(gl)) gl[[i]]$widths <- anchos
    png(archivo, width = w, height = h, units = "in", res = 150)
    grid::grid.draw(do.call(rbind, c(gl, list(size = "first"))))
    invisible(dev.off()) }

  apilar(
    panel(NULL, "1. Muestra limpia",
          paste0("Pendiente: ", f3(b0[2])), 15),
    panel(c2, "2. Caso con residuo grande",
          paste0("Pendiente: ", f3(p2["pend"]), ". la recta no gira"), 15),
    panel(c3, "3. Caso influyente",
          paste0("Pendiente: ", f3(p3["pend"]), ". la recta sí gira"), 15),
    panel(c4, "4. Influyente con residuo diminuto",
          paste0("Pendiente: ", f3(p4["pend"]), ". la recta sí gira"), 44),
    archivo = "figuras/outlier_vs_influyente.png", w = 5.5, h = 13)
  cat("ok | pendientes:", f3(b0[2]), f3(p2["pend"]), f3(p3["pend"]), f3(p4["pend"]), "\n")
