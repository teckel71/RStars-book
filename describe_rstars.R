# Script para la construcción de tablas de datos 
# y trabajo con distribuciones de frecuencias univariantes.

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(gtExtras)
library(knitr)
library(kableExtra)
library(dplyr)
library(DescTools)
library(moments)
library(ggplot2)

## DATOS

# Importando datos desde Excel
datos <- read_excel("trabajadores.xlsx",
                    sheet = "Datos",
                    na = c("n.d."))
datos <- data.frame(datos, row.names = 1)

# visualizando el data frame de modo elegante con {gtExtras}
datos_df_graph <- gt_plt_summary(datos)
datos_df_graph

# Tabla de datos
datos %>%
  kable(caption = "Trabajadores de Shuttlepod Movers",
        col.names = c("Trabajador", "Salario", "Nivel de estudios",
                      "Departamento")) %>%
  kable_styling(full_width = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(datos)), bold= F, align = "c")

# Distribución de frecuencias del salario de los trabajadores de la empresa.

# Colocar los datos
datos <- datos %>% arrange(SALARIO, row.names(datos))

# Contar frecuencias
conteo <- table(datos$SALARIO)
conteo

# Convertir el resultado a un data frame para una mejor visualización
conteo_df <- as.data.frame(conteo)
conteo_df

# Renombrar las columnas para mayor claridad
colnames(conteo_df) <- c("Valor", "Frecuencia")

# Calcular y guardar la frecuencia total
N <- sum(conteo_df$Frecuencia)

# Calcular frecuencias absolutas acumuladas
conteo_df$Frecuencia_acum <- cumsum(conteo_df$Frecuencia)

# Calcular frecuencias relativas
conteo_df$Frecuencia_R <- conteo_df$Frecuencia / N

# Calcular frecuencias relativas acumuladas
conteo_df$Frecuencia_R_acum <- cumsum(conteo_df$Frecuencia_R)

# Mostrar el resultado
conteo_df %>%
  kable(caption = "Distribución de frecuencias de salarios. Shuttlepod Movers",
        col.names = c("x(i) = Salario", "Frecuencia absoluta n(i)",
                      "Frecuencia absoluta acum. N(i)", "Frecuencia relativa f(i)",
                      "Frecuencia relativa acum. F(i)"),
        digits  = c(0, 0, 0, 2, 2),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(conteo_df)), bold= F, align = "c")

# Distribución de frecuencias agrupadas en intervalos
# del salario de los trabajadores de la empresa.

# Crear los intervalos (método de Sturges)
k <- nclass.Sturges(datos$SALARIO)
datos$intervalos <- cut(datos$SALARIO, breaks = k, include.lowest = TRUE)
levels(datos$intervalos)

# Contar las frecuencias de cada intervalo
conteo_intervalos <- table(datos$intervalos)

# Convertir el resultado a un data frame para una mejor visualización
conteo_intervalos_df <- as.data.frame(conteo_intervalos)

# Renombrar las columnas para mayor claridad
colnames(conteo_intervalos_df) <- c("Intervalo", "Frecuencia")

# Calcular la marca de clase de cada intervalo
breaks <- seq(min(datos$SALARIO), max(datos$SALARIO), length.out = k + 1)
marca_clase <- (breaks[-length(breaks)] + breaks[-1]) / 2

# Agregar la columna "marca_clase" al data frame
conteo_intervalos_df$marca_clase <- marca_clase

# Cambiar el orden de las columnas en el data frame con dplyr
conteo_intervalos_df <- conteo_intervalos_df %>%
  select(Intervalo, marca_clase, Frecuencia)

# Calcular y guardar la frecuencia total
N_agre <- sum(conteo_intervalos_df$Frecuencia)

# Calcular frecuencias absolutas acumuladas
conteo_intervalos_df$Frecuencia_acum <- cumsum(conteo_intervalos_df$Frecuencia)

# Calcular frecuencias relativas
conteo_intervalos_df$Frecuencia_R <- conteo_intervalos_df$Frecuencia / N_agre

# Calcular frecuencias relativas acumuladas
conteo_intervalos_df$Frecuencia_R_acum <- cumsum(conteo_intervalos_df$Frecuencia_R)

# Mostrar el resultado
conteo_intervalos_df %>%
  kable(caption = "Distribución de frecuencias agrupadas en intervalos de salarios. Shuttlepod Movers",
        col.names = c("Intervalo salarial",
                      "Marca de clase x(i)",
                      "Frecuencia absoluta n(i)",
                      "Frecuencia absoluta acum. N(i)",
                      "Frecuencia relativa f(i)",
                      "Frecuencia relativa acum. F(i)"),
        digits  = c(NA, 2, 0, 0, 2, 2),
        format.args = list(decimal.mark = ".", scientific = FALSE)) %>%
  kable_styling(full_width = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(conteo_intervalos_df)), bold= F, align = "c")

## MEDIDAS

# Media aritmética
media <- mean(datos$SALARIO)
media

# Mediana
mediana <- median(datos$SALARIO)
mediana

# Moda
moda <- Mode(datos$SALARIO)
moda

# Calcular los cuartiles
cuartiles <- quantile(datos$SALARIO, probs = c(0.25, 0.5, 0.75))
cuartiles

# Varianza
varianza <- var(datos$SALARIO)*(N-1)/N # recordar que la frecuencia total N ya fue calculada
varianza

# Desviación típica
desv <- varianza ^ (1/2)
desv

# Cuasivarianza
cuasivarianza <- var(datos$SALARIO)
cuasivarianza

# Coeficiente de variación
cvariacion <- desv / abs(media)
cvariacion

# Coeficiente de asimetría de Fisher
asimetria <- skewness(datos$SALARIO)
asimetria

ggplot(data = datos, aes(x = SALARIO)) +
  geom_histogram(bins = k,
                 colour = "red",
                 fill = "orange") +
  geom_vline(aes(xintercept = mean(SALARIO)),
             colour = "blue",
             linetype = "dashed",
             linewidth = 1) +
  ggtitle("SALARIO MENSUAL",
          subtitle = "trabajadores de Shuttlepod Movers")+
  xlab("Salario (cientos de PAVOs)") +
  ylab("Frecuencias")

# Coeficiente de apuntamiento o curtosis de Fisher
curtosis <- kurtosis(datos$SALARIO) - 3
curtosis

ggplot(data = datos, aes(x = SALARIO)) +
  geom_histogram(bins = k,
                 colour = "red",
                 fill = "orange",
                 aes(y = after_stat(density))) +
  stat_function(fun = dnorm,
                args = list(mean = moda, sd = sd(datos$SALARIO)),
                colour = "darkblue",
                linewidth = 1) +
  ggtitle("SALARIO MENSUAL",
          subtitle = "trabajadores de Shuttlepod Movers")+
  xlab("Salario (cientos de PAVOs)") +
  ylab("Densidad")

# Tabla resumen de medidas descriptivas
resumen <- data.frame(
  Medida = c("Media aritmética", "Mediana", "Moda",
             "Cuartil 1 (Q1)", "Cuartil 3 (Q3)", "Rango intercuartílico (IQR)",
             "Varianza", "Desviación típica", "Cuasivarianza",
             "Coef. de variación", "Coef. de asimetría (Fisher)",
             "Coef. de curtosis (Fisher)"),
  Valor = round(c(media, mediana, as.numeric(moda),
                  cuartiles[1], cuartiles[3], cuartiles[3] - cuartiles[1],
                  varianza, desv, cuasivarianza,
                  cvariacion, asimetria, curtosis), 3)
)

resumen %>%
  kable(caption = "Resumen del análisis descriptivo de SALARIO. Shuttlepod Movers",
        col.names = c("Medida", "Valor")) %>%
  kable_styling(full_width = F,
                bootstrap_options = c("striped", "bordered", "condensed"),
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold = T, align = "c") %>%
  row_spec(1:nrow(resumen), bold = F, align = "c")

# Fin del script :)
