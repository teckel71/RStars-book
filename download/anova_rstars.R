### ANOVA de un factor: El Informe Crestfall ###

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)
library(car)          # test de Levene
library(emmeans)      # comparaciones múltiples (Tukey)
library(effectsize)   # tamaño del efecto (eta², omega²)
library(FSA)          # comparaciones múltiples (Dunn)

# Paquete MATrstars: funciones auxiliares del libro R-Stars.
# Contiene, entre otras, las funciones explora_na(), explora_outliers()
# y kable_rstars(), utilizadas más adelante.
# Si no está instalado, se instala desde GitHub (solo la primera vez).
if (!requireNamespace("MATrstars", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("teckel71/MATrstars")
}
library(MATrstars)


## DATOS

# Importando datos desde Excel
datos <- read_excel("astilleros_48.xlsx", sheet = "Datos")
datos <- data.frame(datos, row.names = 1)
summary(datos)


## EJEMPLO 1: ¿Influye el astillero en el rendimiento energético (IRE)?

# Missing values
muestra <- explora_na(
  datos,
  variables = c(IRE, ASTILLERO),
  accion    = "eliminar",
  titulo    = "Astilleros: valores ausentes",
  subtitulo = "48 naves cargueras interestelares"
)

# Outliers
muestra_so <- explora_outliers(
  muestra,
  variables = IRE,
  accion    = "eliminar",
  titulo    = "ÍNDICE DE RENDIMIENTO ENERGÉTICO",
  subtitulo = "48 naves cargueras interestelares"
)

# Tabla de medias por astillero
tablamedias <- muestra_so %>%
               group_by(ASTILLERO) %>%
               summarise(observaciones = n(),
                         media         = round(mean(IRE), 3))

tablamedias %>%
  kable_rstars(caption   = "IRE. Medias por astillero.",
               col.names = c("Astillero", "Observaciones", "IRE medio"))

# Gráficos exploratorios

# Diagrama de densidad
gdensidad <- ggplot(data = muestra_so,
                    aes(x = IRE, color = ASTILLERO, fill = ASTILLERO)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = tablamedias,
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
  stat_summary(fun   = "mean",
               geom  = "point",
               size  = 3,
               aes(col = ASTILLERO),
               alpha = 0.60) +
  geom_jitter(width = 0.1, size = 1,
              aes(col = ASTILLERO), alpha = 0.40) +
  labs(title = "Diagramas de caja por astillero con medias",
       x     = "IRE (tn / unidad de especia)",
       y     = "Astillero")

gdensidad / gbox

# Prerrequisitos del ANOVA

# Normalidad: gráficos QQ
ggplot(data = muestra_so,
       aes(sample = IRE)) +
  stat_qq(colour = "red") +
  stat_qq_line(colour = "dark blue") +
  ggtitle("IRE: QQ-PLOT",
          subtitle = "48 naves cargueras (sin outliers)") +
  facet_grid(. ~ ASTILLERO)

# Normalidad: Shapiro-Wilk por grupo
normalidad_ire <- muestra_so %>%
  group_by(ASTILLERO) %>%
  summarise(shapiro_p_value = round(shapiro.test(IRE)$p.value, 3)) %>%
  mutate(decide = if_else(shapiro_p_value > 0.05,
                          "NORMALIDAD",
                          "NO-NORMALIDAD"))

normalidad_ire %>%
  kable_rstars(caption   = "Normalidad del IRE por astillero (Shapiro-Wilk)",
               col.names = c("Astillero", "p-valor", "Conclusión"))

# Homogeneidad de varianzas: Levene (test recomendado por su robustez
# ante desviaciones de la normalidad).
leveneTest(IRE ~ ASTILLERO, data = muestra_so)

# Test F de ANOVA
Datos.aov <- aov(muestra_so$IRE ~ muestra_so$ASTILLERO)
summary_aov <- summary(Datos.aov)

aov_table <- as.data.frame(summary_aov[[1]])

aov_table %>%
  kable_rstars(caption   = "Resultados del ANOVA — IRE por astillero",
               col.names = c("Grados de libertad",
                             "Suma de cuadrados",
                             "Media de cuadrados",
                             "Estadístico F",
                             "p-valor"))

# Tamaño del efecto (eta² y omega²)
eta_squared(Datos.aov,   partial = FALSE, ci = 0.95)
omega_squared(Datos.aov, partial = FALSE, ci = 0.95)

# Comparaciones múltiples (HSD de Tukey)
medias_ire <- emmeans(Datos.aov, "ASTILLERO")
pares_ire  <- pairs(medias_ire)
pares_ire_df <- as.data.frame(pares_ire)

pares_ire_df %>%
  kable_rstars(caption   = "Comparaciones múltiples — IRE (HSD de Tukey)",
               col.names = c("Grupos",
                             "Diferencia estimada",
                             "Error típico",
                             "Grados de libertad",
                             "Estadístico t",
                             "p-valor"))


## EJEMPLO 2: ¿Influye el astillero en la fiabilidad de las naves (IIG)?
##            Cuando el ANOVA no es el método adecuado.

# Missing values
muestra2 <- explora_na(
  datos,
  variables = c(IIG, ASTILLERO),
  accion    = "eliminar",
  titulo    = "Astilleros: valores ausentes en IIG",
  subtitulo = "48 naves cargueras interestelares"
)

# Outliers
muestra2_so <- explora_outliers(
  muestra2,
  variables = IIG,
  accion    = "eliminar",
  titulo    = "ÍNDICE DE INCIDENCIAS GRAVES",
  subtitulo = "48 naves cargueras interestelares"
)

# Tabla de medias por astillero
tablamedias2 <- muestra2_so %>%
                group_by(ASTILLERO) %>%
                summarise(observaciones = n(),
                          media         = round(mean(IIG), 3))

tablamedias2 %>%
  kable_rstars(caption   = "IIG. Medias por astillero.",
               col.names = c("Astillero", "Observaciones", "IIG medio"))

# Gráficos exploratorios
gdensidad2 <- ggplot(data = muestra2_so,
                     aes(x = IIG, color = ASTILLERO, fill = ASTILLERO)) +
  geom_density(alpha = 0.3) +
  geom_vline(data = tablamedias2,
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
  stat_summary(fun   = "mean",
               geom  = "point",
               size  = 3,
               aes(col = ASTILLERO),
               alpha = 0.60) +
  geom_jitter(width = 0.1, size = 1,
              aes(col = ASTILLERO), alpha = 0.40) +
  labs(title = "Diagramas de caja por astillero con medias",
       x     = "IIG (nº de averías mayores)",
       y     = "Astillero")

gdensidad2 / gbox2

# Prerrequisitos del ANOVA

# Normalidad: gráficos QQ
ggplot(data = muestra2_so,
       aes(sample = IIG)) +
  stat_qq(colour = "red") +
  stat_qq_line(colour = "dark blue") +
  ggtitle("IIG: QQ-PLOT",
          subtitle = "48 naves cargueras (sin outliers)") +
  facet_grid(. ~ ASTILLERO)

# Normalidad: Shapiro-Wilk por grupo
normalidad_iig <- muestra2_so %>%
  group_by(ASTILLERO) %>%
  summarise(shapiro_p_value = round(shapiro.test(IIG)$p.value, 3)) %>%
  mutate(decide = if_else(shapiro_p_value > 0.05,
                          "NORMALIDAD",
                          "NO-NORMALIDAD"))

normalidad_iig %>%
  kable_rstars(caption   = "Normalidad del IIG por astillero (Shapiro-Wilk)",
               col.names = c("Astillero", "p-valor", "Conclusión"))

# Homogeneidad de varianzas: Levene
leveneTest(IIG ~ ASTILLERO, data = muestra2_so)

# Test F de ANOVA (a efectos ilustrativos)
Datos.aov2   <- aov(muestra2_so$IIG ~ muestra2_so$ASTILLERO)
summary_aov2 <- summary(Datos.aov2)

aov_table2 <- as.data.frame(summary_aov2[[1]])

aov_table2 %>%
  kable_rstars(caption   = "Resultados del ANOVA — IIG por astillero (solo ilustrativo)",
               col.names = c("Grados de libertad",
                             "Suma de cuadrados",
                             "Media de cuadrados",
                             "Estadístico F",
                             "p-valor"))

# Test de Kruskal-Wallis: prueba no paramétrica que compara distribuciones
# (rangos medios) entre los grupos. No requiere normalidad ni homocedasticidad.
Datos.K <- kruskal.test(muestra2_so$IIG ~ muestra2_so$ASTILLERO)
Datos.K

# Tamaño del efecto (no paramétrico): eta² para KW.
rank_epsilon_squared(IIG ~ ASTILLERO, data = muestra2_so)

# Comparaciones múltiples: test de Dunn.
# Alternativa moderna a kruskalmc(). Devuelve p-valores explícitos con
# ajuste por comparaciones múltiples (Bonferroni por defecto).
Datos.dunn <- dunnTest(IIG ~ ASTILLERO, data = muestra2_so,
                       method = "bonferroni")

Datos.dunn$res %>%
  kable_rstars(caption   = "Dunn. Comparaciones múltiples — IIG (Bonferroni)",
               col.names = c("Comparación",
                             "Estadístico Z",
                             "p-valor sin ajustar",
                             "p-valor ajustado"))

# Fin del script :)
