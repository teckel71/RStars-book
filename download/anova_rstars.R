# =============================================================================
# Análisis ANOVA de un factor — El Informe Crestfall
# R-Stars: La Guía | Capítulo 8
# =============================================================================
# Base de datos: astilleros_48.xlsx
# Casos: 48 naves cargueras interestelares (16 por astillero)
# Factor: ASTILLERO (KORRIGAN / SELENE / ARCTURUS)
# Variables métricas:
#   IRE — Índice de Rendimiento Energético (tn / unidad de especia)
#   IIG — Índice de Incidencias Graves (nº de averías mayores)
# =============================================================================

rm(list = ls())

# DATOS -----------------------------------------------------------------------

library(readxl)
datos <- read_excel("astilleros_48.xlsx", sheet = "Datos")
datos <- data.frame(datos, row.names = 1)
summary(datos)


# =============================================================================
# EJEMPLO 1: ¿Influye el astillero en el rendimiento energético (IRE)?
# =============================================================================

# --- Missing values -----------------------------------------------------------

library(dplyr)
library(visdat)
vis_miss(datos)

datos %>% filter(is.na(IRE) | is.na(ASTILLERO)) %>%
          select(IRE, ASTILLERO)

muestra <- datos %>%
           filter(!is.na(IRE) & !is.na(ASTILLERO))

# --- Outliers -----------------------------------------------------------------

library(ggplot2)
ggplot(data = muestra,
       aes(y = IRE)) +
geom_boxplot(fill = "steelblue", alpha = 0.7) +
ggtitle("ÍNDICE DE RENDIMIENTO ENERGÉTICO",
        subtitle = "48 naves cargueras interestelares") +
ylab("IRE (tn / unidad de especia)")

Q1 <- quantile(muestra$IRE, 0.25)
Q3 <- quantile(muestra$IRE, 0.75)

muestra %>%
  filter(IRE > Q3 + 1.5 * IQR(IRE) |
         IRE < Q1 - 1.5 * IQR(IRE)) %>%
  select(IRE, ASTILLERO)

muestra_so <- muestra %>%
              filter(IRE <= Q3 + 1.5 * IQR(IRE) &
                     IRE >= Q1 - 1.5 * IQR(IRE))

# --- Tabla de medias por astillero -------------------------------------------

library(knitr)
library(kableExtra)
knitr.table.format = "html"

tablamedias <- muestra_so %>%
               group_by(ASTILLERO) %>%
               summarise(observaciones = n(),
                         media         = round(mean(IRE), 3))

tablamedias %>%
  kable(format    = knitr.table.format,
        caption   = "IRE. Medias por astillero.",
        col.names = c("Astillero", "Observaciones", "IRE medio")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(tablamedias), bold = F, align = "c")

# --- Gráficos exploratorios --------------------------------------------------

# Diagrama de densidad
gdensidad <- ggplot(data = muestra_so,
                    aes(x = IRE, color = ASTILLERO, fill = ASTILLERO)) +
  geom_density(alpha = 0.3) +
  geom_vline(data     = tablamedias,
             aes(xintercept = media, color = ASTILLERO),
             linetype  = "dashed",
             linewidth = 1) +
  labs(title = "Diagramas de densidad por astillero con medias",
       x     = "IRE (tn / unidad de especia)",
       y     = "Densidad") +
  theme_grey()

# Diagrama de caja
gbox <- ggplot(data = muestra_so,
               aes(y = ASTILLERO, x = IRE,
                   color = ASTILLERO, fill = ASTILLERO)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  stat_summary(fun      = "mean",
               geom     = "point",
               size     = 3,
               aes(col  = ASTILLERO),
               alpha    = 0.60) +
  geom_jitter(width = 0.1, size = 1,
              aes(col = ASTILLERO), alpha = 0.40) +
  labs(title = "Diagramas de caja por astillero con medias",
       x     = "IRE (tn / unidad de especia)",
       y     = "Astillero")

library(patchwork)
gdensidad / gbox

# --- PRERREQUISITOS DEL ANOVA ------------------------------------------------

# Normalidad — gráficos QQ

ggplot(data = muestra_so,
       aes(sample = IRE)) +
  stat_qq(colour = "red") +
  stat_qq_line(colour = "dark blue") +
  ggtitle("IRE: QQ-PLOT",
          subtitle = "48 naves cargueras (sin outliers)") +
  facet_grid(. ~ ASTILLERO)

# Normalidad — Shapiro-Wilk por grupo

normalidad_ire <- muestra_so %>%
  group_by(ASTILLERO) %>%
  summarise(shapiro_p_value = round(shapiro.test(IRE)$p.value, 3)) %>%
  mutate(decide = if_else(shapiro_p_value > 0.05,
                          "NORMALIDAD",
                          "NO-NORMALIDAD"))

normalidad_ire %>%
  kable(format    = knitr.table.format,
        caption   = "Normalidad del IRE por astillero (Shapiro-Wilk)",
        col.names = c("Astillero", "p-valor", "Conclusión")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(normalidad_ire), bold = F, align = "c")

# Homogeneidad de varianzas — Bartlett

bartlett.test(muestra_so$IRE ~ muestra_so$ASTILLERO)

# --- TEST F DE ANOVA ---------------------------------------------------------

Datos.aov <- aov(muestra_so$IRE ~ muestra_so$ASTILLERO)
summary_aov <- summary(Datos.aov)

aov_table <- as.data.frame(summary_aov[[1]])

aov_table %>%
  kable(format    = knitr.table.format,
        caption   = "Resultados del ANOVA — IRE por astillero",
        col.names = c("Grados de libertad",
                      "Suma de cuadrados",
                      "Media de cuadrados",
                      "Estadístico F",
                      "p-valor")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(aov_table), bold = F, align = "c")

# --- COMPARACIONES MÚLTIPLES (HSD de Tukey) ----------------------------------

library(emmeans)
medias_ire <- emmeans(Datos.aov, "ASTILLERO")
pares_ire  <- pairs(medias_ire)
pares_ire_df <- as.data.frame(pares_ire)

pares_ire_df %>%
  kable(format    = knitr.table.format,
        caption   = "Comparaciones múltiples — IRE (HSD de Tukey)",
        col.names = c("Grupos",
                      "Diferencia estimada",
                      "Error típico",
                      "Grados de libertad",
                      "Estadístico t",
                      "p-valor")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(pares_ire_df), bold = F, align = "c")


# =============================================================================
# EJEMPLO 2: ¿Influye el astillero en la fiabilidad de las naves (IIG)?
#            — Cuando el ANOVA no es el método adecuado
# =============================================================================

# --- Missing values -----------------------------------------------------------

datos %>% filter(is.na(IIG) | is.na(ASTILLERO)) %>%
          select(IIG, ASTILLERO)

muestra2 <- datos %>%
            filter(!is.na(IIG) & !is.na(ASTILLERO))

# --- Outliers -----------------------------------------------------------------

ggplot(data = muestra2,
       aes(y = IIG)) +
geom_boxplot(fill = "coral", alpha = 0.7) +
ggtitle("ÍNDICE DE INCIDENCIAS GRAVES",
        subtitle = "48 naves cargueras interestelares") +
ylab("IIG (nº de averías mayores)")

Q1b <- quantile(muestra2$IIG, 0.25)
Q3b <- quantile(muestra2$IIG, 0.75)

muestra2 %>%
  filter(IIG > Q3b + 1.5 * IQR(IIG) |
         IIG < Q1b - 1.5 * IQR(IIG)) %>%
  select(IIG, ASTILLERO)

muestra2_so <- muestra2 %>%
               filter(IIG <= Q3b + 1.5 * IQR(IIG) &
                      IIG >= Q1b - 1.5 * IQR(IIG))

# --- Tabla de medias por astillero -------------------------------------------

tablamedias2 <- muestra2_so %>%
                group_by(ASTILLERO) %>%
                summarise(observaciones = n(),
                          media         = round(mean(IIG), 3))

tablamedias2 %>%
  kable(format    = knitr.table.format,
        caption   = "IIG. Medias por astillero.",
        col.names = c("Astillero", "Observaciones", "IIG medio")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(tablamedias2), bold = F, align = "c")

# --- Gráficos exploratorios --------------------------------------------------

gdensidad2 <- ggplot(data = muestra2_so,
                     aes(x = IIG, color = ASTILLERO, fill = ASTILLERO)) +
  geom_density(alpha = 0.3) +
  geom_vline(data     = tablamedias2,
             aes(xintercept = media, color = ASTILLERO),
             linetype  = "dashed",
             linewidth = 1) +
  labs(title = "Diagramas de densidad por astillero con medias",
       x     = "IIG (nº de averías mayores)",
       y     = "Densidad") +
  theme_grey()

gbox2 <- ggplot(data = muestra2_so,
                aes(y = ASTILLERO, x = IIG,
                    color = ASTILLERO, fill = ASTILLERO)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  stat_summary(fun      = "mean",
               geom     = "point",
               size     = 3,
               aes(col  = ASTILLERO),
               alpha    = 0.60) +
  geom_jitter(width = 0.1, size = 1,
              aes(col = ASTILLERO), alpha = 0.40) +
  labs(title = "Diagramas de caja por astillero con medias",
       x     = "IIG (nº de averías mayores)",
       y     = "Astillero")

gdensidad2 / gbox2

# --- PRERREQUISITOS DEL ANOVA ------------------------------------------------

# Normalidad — gráficos QQ

ggplot(data = muestra2_so,
       aes(sample = IIG)) +
  stat_qq(colour = "red") +
  stat_qq_line(colour = "dark blue") +
  ggtitle("IIG: QQ-PLOT",
          subtitle = "48 naves cargueras (sin outliers)") +
  facet_grid(. ~ ASTILLERO)

# Normalidad — Shapiro-Wilk por grupo

normalidad_iig <- muestra2_so %>%
  group_by(ASTILLERO) %>%
  summarise(shapiro_p_value = round(shapiro.test(IIG)$p.value, 3)) %>%
  mutate(decide = if_else(shapiro_p_value > 0.05,
                          "NORMALIDAD",
                          "NO-NORMALIDAD"))

normalidad_iig %>%
  kable(format    = knitr.table.format,
        caption   = "Normalidad del IIG por astillero (Shapiro-Wilk)",
        col.names = c("Astillero", "p-valor", "Conclusión")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(normalidad_iig), bold = F, align = "c")

# Homogeneidad de varianzas — Bartlett

bartlett.test(muestra2_so$IIG ~ muestra2_so$ASTILLERO)

# --- TEST F DE ANOVA (a efectos ilustrativos) --------------------------------

Datos.aov2  <- aov(muestra2_so$IIG ~ muestra2_so$ASTILLERO)
summary_aov2 <- summary(Datos.aov2)

aov_table2 <- as.data.frame(summary_aov2[[1]])

aov_table2 %>%
  kable(format    = knitr.table.format,
        caption   = "Resultados del ANOVA — IIG por astillero (solo ilustrativo)",
        col.names = c("Grados de libertad",
                      "Suma de cuadrados",
                      "Media de cuadrados",
                      "Estadístico F",
                      "p-valor")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(aov_table2), bold = F, align = "c")

# --- TEST DE KRUSKAL-WALLIS --------------------------------------------------

library(pgirmess)

Datos.K <- kruskal.test(muestra2_so$IIG ~ muestra2_so$ASTILLERO)
Datos.K

# --- COMPARACIONES MÚLTIPLES (Kruskal-Wallis) --------------------------------

Datos.kmc <- kruskalmc(muestra2_so$IIG ~ muestra2_so$ASTILLERO)

Datos.kmc.df <- as.data.frame(Datos.kmc$dif.com)

Datos.kmc.df %>%
  kable(format    = knitr.table.format,
        caption   = "Kruskal-Wallis. Comparaciones múltiples — IIG",
        col.names = c("Diferencia de rangos medios",
                      "Diferencia crítica",
                      "Significativo")) %>%
  kable_styling(full_width        = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position          = "center",
                font_size         = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(Datos.kmc.df), bold = F, align = "c")

# Fin del script :)
