# ══════════════════════════════════════════════════════════════════════════════
# MODELO LOGIT BINOMIAL — Licencias de operación en la ruta Coruscant–Arrakis
# ══════════════════════════════════════════════════════════════════════════════
# Script de práctica del capítulo 10 del libro R-Stars.
#
# Objetivo: modelizar qué factores determinan que la Autoridad Galáctica de
# Transporte Interestelar (AGTI) conceda o deniegue una licencia de operación
# a las compañías del sector que solicitan operar en una ruta intergaláctica.
# ══════════════════════════════════════════════════════════════════════════════


# Limpiando el Global Environment

rm(list = ls())


# Cargando paquetes

library(readxl)
library(dplyr)
library(ggplot2)
library(aod)       # Para el contraste de Wald sobre factores

# Paquete MATrstars: funciones auxiliares del libro R-Stars.
# Contiene, entre otras, las funciones presenta_logit(), kable_rstars(),
# explora_na() y create_patchwork(), utilizadas más adelante.
# Si el paquete no está instalado, se instala desde GitHub (una sola vez).
if (!requireNamespace("MATrstars", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("teckel71/MATrstars")
}
library(MATrstars)


# ══════════════════════════════════════════════════════════════════════════════
# 1. CARGA Y PREPARACIÓN DE DATOS
# ══════════════════════════════════════════════════════════════════════════════

LICENCIAS <- read_excel("licencias_100.xlsx", sheet = "Datos")
LICENCIAS <- as.data.frame(LICENCIAS, row.names = 1)

summary(LICENCIAS)
str(LICENCIAS)


# 1a. Detección y tratamiento de valores perdidos

LICENCIAS <- explora_na(
  LICENCIAS,
  accion    = "eliminar",
  titulo    = "Modelo logit binomial.",
  subtitulo = "Licencias de operación — Ruta Coruscant–Arrakis"
)


# 1b. Conversión de variables categóricas a factor

# EFLO: estado de la flota (categórico ordinal).
# Fijamos el nivel de referencia en ANTIGUA (la categoría "peor"),
# de modo que los coeficientes de MADURA y RENOVADA indiquen la mejora
# respecto a ese nivel base.

LICENCIAS$EFLO <- factor(LICENCIAS$EFLO,
                         levels = c("ANTIGUA", "MADURA", "RENOVADA"))

# LICENCIA: variable dependiente (binaria).
# 0 = denegada, 1 = concedida.

LICENCIAS$LICENCIA <- factor(LICENCIAS$LICENCIA)

# Verificamos la conversión y la codificación de contrastes
is.factor(LICENCIAS$EFLO)
is.factor(LICENCIAS$LICENCIA)
contrasts(LICENCIAS$EFLO)
contrasts(LICENCIAS$LICENCIA)


# 1c. Análisis descriptivo previo

# Distribución de la variable dependiente
table(LICENCIAS$LICENCIA)

# Tabla cruzada: LICENCIA por EFLO
table(LICENCIAS$EFLO, LICENCIAS$LICENCIA)


# 1d. Visualización de las variables categóricas frente a LICENCIA

# Gráfico de barras apiladas: EFLO × LICENCIA (proporciones)
g_eflo <- ggplot(LICENCIAS, aes(x = EFLO, fill = LICENCIA)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "#E74C3C", "1" = "#2ECC71"),
                    labels = c("0" = "Denegada", "1" = "Concedida")) +
  labs(title = "Licencia según estado de la flota",
       x = "EFLO", y = "Proporción", fill = "Licencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11))

# Gráfico de barras apiladas: GALAXIA × LICENCIA (proporciones)
g_gal <- ggplot(LICENCIAS, aes(x = GALAXIA, fill = LICENCIA)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("0" = "#E74C3C", "1" = "#2ECC71"),
                    labels = c("0" = "Denegada", "1" = "Concedida")) +
  labs(title = "Licencia según galaxia de operación",
       x = "GALAXIA", y = "Proporción", fill = "Licencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 11),
        axis.text.x = element_text(angle = 25, hjust = 1, size = 8))

# Composición de los dos gráficos categóricos
g_eflo + g_gal + patchwork::plot_layout(guides = "collect")


# 1e. Visualización de las variables métricas por grupo de LICENCIA

# Tabla de medias
LICENCIAS %>%
  group_by(LICENCIA) %>%
  summarise(across(c(SOLVENCIA, IPUNT, TINCID, IFIDE, FLOTA, RUTAS, EXPERIENCIA),
                   ~ round(mean(.), 2))) %>%
  kable_rstars(caption = "Medias por grupo de licencia",
               col.names = c("LICENCIA", "SOLVENCIA", "IPUNT", "TINCID",
                              "IFIDE", "FLOTA", "RUTAS", "EXPERIENCIA"))

# Gráficos de barras agrupadas para cada variable métrica
vars_metricas <- c("SOLVENCIA", "IPUNT", "TINCID", "IFIDE",
                   "FLOTA", "RUTAS", "EXPERIENCIA")

graficos_metricas <- lapply(vars_metricas, function(var) {
  df_medias <- LICENCIAS %>%
    group_by(LICENCIA) %>%
    summarise(media = mean(.data[[var]], na.rm = TRUE), .groups = "drop")

  ggplot(df_medias, aes(x = LICENCIA, y = media, fill = LICENCIA)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = round(media, 1)), vjust = -0.5, size = 3) +
    scale_fill_manual(values = c("0" = "#E74C3C", "1" = "#2ECC71"),
                      labels = c("0" = "Denegada", "1" = "Concedida")) +
    labs(title = var, x = NULL, y = "Media", fill = "Licencia") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
          legend.position = "bottom",
          legend.title = element_text(size = 9),
          legend.text = element_text(size = 8))
})

# Composición en patchwork (2x2)
pw_metricas <- create_patchwork(graficos_metricas)
for (p in pw_metricas) print(p)


# ══════════════════════════════════════════════════════════════════════════════
# 2. PARTICIÓN EN MUESTRA DE ESTIMACIÓN Y MUESTRA DE TEST
# ══════════════════════════════════════════════════════════════════════════════

# Usamos las primeras 80 observaciones para estimar y las 20 últimas para
# validar. La alternativa es una partición aleatoria (con set.seed para
# reproducibilidad), pero la partición fija simplifica la exposición.

ESTIMA <- LICENCIAS[1:80, ]
TEST   <- LICENCIAS[81:100, ]

cat("Muestra de estimación:", nrow(ESTIMA), "observaciones\n")
cat("Muestra de test:",       nrow(TEST),   "observaciones\n")
table(ESTIMA$LICENCIA)
table(TEST$LICENCIA)


# ══════════════════════════════════════════════════════════════════════════════
# 3. ESTIMACIÓN DEL MODELO LOGIT COMPLETO
# ══════════════════════════════════════════════════════════════════════════════

modelo <- glm(LICENCIA ~ SOLVENCIA + IPUNT + TINCID + IFIDE +
                          FLOTA + RUTAS + EXPERIENCIA + EFLO,
              data   = ESTIMA,
              family = binomial(link = "logit"))

summary(modelo)


# ══════════════════════════════════════════════════════════════════════════════
# 4. PRESENTACIÓN DE RESULTADOS CON presenta_logit()
# ══════════════════════════════════════════════════════════════════════════════

# presenta_logit() genera cuatro tablas formateadas:
#   $coef  → coeficientes con contraste de Wald individual (z, p-valor)
#   $gof   → bondad del ajuste (R² de Nagelkerke, AIC, devianzas)
#   $or    → odd ratios e interpretación del cambio en la ventaja
#   $conf  → matriz de confusión (si se pasa el argumento "datos")

salida <- presenta_logit(modelo, datos = ESTIMA)

salida$coef    # Tabla 1: Coeficientes del modelo
salida$gof     # Tabla 2: Bondad del ajuste
salida$or      # Tabla 3: Odd ratios


# ══════════════════════════════════════════════════════════════════════════════
# 5. CONTRASTE DE WALD PARA EL FACTOR EFLO
# ══════════════════════════════════════════════════════════════════════════════

# EFLO tiene 3 niveles (ANTIGUA, MADURA, RENOVADA), lo que genera 2 dummies.
# El contraste individual (z) de cada dummy puede no ser significativo, pero
# el factor en su conjunto sí podría serlo. El contraste de Wald conjunto
# evalúa H0: beta_MADURA = beta_RENOVADA = 0 simultáneamente.

# Identificamos qué posiciones del vector de coeficientes corresponden a EFLO.
# (Intercept)=1, SOLVENCIA=2, IPUNT=3, TINCID=4, IFIDE=5,
# FLOTA=6, RUTAS=7, EXPERIENCIA=8, EFLOMADURA=9, EFLORENOVADA=10
names(coef(modelo))

wald.test(b     = coef(modelo),
          Sigma = vcov(modelo),
          Terms = 9:10)


# ══════════════════════════════════════════════════════════════════════════════
# 6. INTERPRETACIÓN DE LOS ODD RATIOS
# ══════════════════════════════════════════════════════════════════════════════

# La tabla de odd ratios ($or) ya muestra el cambio porcentual en la
# ventaja (odds) por cada unidad de incremento en la variable.
# Mostramos la tabla y la interpretamos:

salida$or

# Ejemplo de lectura:
#   - TINCID tiene un OR < 1 (coef. negativo): cada incidencia adicional
#     por 1.000 años-luz REDUCE la ventaja de obtener la licencia.
#   - IPUNT tiene un OR > 1 (coef. positivo): cada punto adicional de
#     puntualidad AUMENTA la ventaja de obtener la licencia.


# ══════════════════════════════════════════════════════════════════════════════
# 7. BONDAD DEL AJUSTE
# ══════════════════════════════════════════════════════════════════════════════

# 7a. Tabla de bondad del ajuste

salida$gof

# 7b. Mejora de la devianza (análisis de devianza tipo chi-cuadrado)

anova(modelo, test = "Chisq")

# 7c. Comparación de AIC con el modelo nulo

modelo_nulo <- glm(LICENCIA ~ 1, data = ESTIMA,
                   family = binomial(link = "logit"))
AIC(modelo_nulo, modelo)


# ══════════════════════════════════════════════════════════════════════════════
# 8. CAPACIDAD PREDICTIVA
# ══════════════════════════════════════════════════════════════════════════════

# 8a. Predicción in-sample (sobre la muestra de estimación)

salida$conf

# 8b. Predicción sobre la muestra de test (datos no vistos)

salida_test <- presenta_logit(modelo, datos = TEST,
                              captions = c("Modelo Logit Binomial",
                                           "Bondad del ajuste",
                                           "Odd ratios",
                                           "Matriz de confusión (TEST)"))
salida_test$conf


# ══════════════════════════════════════════════════════════════════════════════
# 9. SIMULACIÓN DE ESCENARIOS
# ══════════════════════════════════════════════════════════════════════════════

# Los escenarios se definen en una hoja del archivo Excel (hoja "Escenarios").
# La función predict_from_excel_scenarios() los lee, calcula la probabilidad
# estimada de obtener la licencia, construye un intervalo de confianza sobre
# la probabilidad y clasifica cada escenario según el umbral (0.5).
#
# La función detecta automáticamente que el modelo es un logit binomial y
# adapta su comportamiento: en lugar de intervalos de predicción (propios de
# la regresión lineal), calcula la probabilidad estimada con su intervalo de
# confianza, construido en la escala del logit y transformado a probabilidades.

simulacion <- predict_from_excel_scenarios(
  model      = modelo,
  train_df   = ESTIMA,
  excel_path = "escenarios_logit_rstars.xlsx",
  sheet      = "Escenarios",
  id_col     = "ESCENARIO",
  umbral     = 0.5,
  caption    = "Simulación de escenarios"
)

simulacion$table    # Tabla formateada con probabilidades y clasificación
simulacion$data     # Data frame con los resultados numéricos


# ══════════════════════════════════════════════════════════════════════════════
# 10. SELECCIÓN AUTOMÁTICA DE ESPECIFICACIÓN
# ══════════════════════════════════════════════════════════════════════════════

# step() aplica un algoritmo de selección por pasos basado en AIC,
# eliminando las variables que no aportan al modelo.

modelo_step <- step(modelo)
summary(modelo_step)

# Presentación del modelo reducido
salida_red <- presenta_logit(modelo_step, datos = ESTIMA,
                             captions = c("Modelo reducido (step)",
                                          "Bondad del ajuste",
                                          "Odd ratios",
                                          "Matriz de confusión"))
salida_red$coef
salida_red$gof
salida_red$or
salida_red$conf

# Predicción del modelo reducido sobre la muestra de test
salida_red_test <- presenta_logit(modelo_step, datos = TEST,
                                  captions = c("Modelo reducido",
                                               "Bondad del ajuste",
                                               "Odd ratios",
                                               "Confusión TEST (reducido)"))
salida_red_test$conf

# Comparación de AIC: modelo completo vs. modelo reducido
AIC(modelo, modelo_step)

