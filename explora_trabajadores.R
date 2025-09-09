# Script para la construcción de tablas de datos 
# y trabajo con distribucionesde frecuencias univariantes.
#

rm(list = ls())

## DATOS

# Importando

library(readxl)
datos <- read_excel("trabajadores.xlsx", sheet = "Datos")
datos <- data.frame(datos, row.names = 1)
summary (datos)

# Tabla de datos

library (knitr)
library (kableExtra)
knitr.table.format = "html"

datos %>%
  kable(caption = "Trabajadores asalariados de la empresa",
        col.names = c("Trabajador", "Salario", "Nivel de estudios",
                      "Departamento")) %>%
  kable_styling(full_width = F, bootstrap_options = "striped", "bordered",
                "condensed", position = "center", font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(datos)), bold= F, align = "c")

# Distribución de frecuencias del salario de los trabajadores de la empresa.

# Colocar los datos

library(dplyr)
datos <- datos %>% arrange(SALARIO, row.names(datos))

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
  kable(caption = "Distribución de frecuencias de los salarios de la empresa",
        col.names = c("x(i) = Salario", "Frecuencia absoluta n(i)",
                      "Frecuencia absoluta acum. N(i)", "Frecuencia relativa f(i)",
                      "Frecuencia relativa acum. F(i)"),
                      format.args = list(decimal.mark = ".", digits = 2)) %>%
  kable_styling(full_width = F, bootstrap_options = "striped", "bordered", "condensed", position = "center", font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:(nrow(conteo_df)), bold= F, align = "c")

# Distribución de frecuencias agrupadas en intervalos
# del salario de los trabajadores de la empresa.

# Crear los intervalos

datos$intervalos <- cut(datos$SALARIO, breaks = 4, include.lowest = TRUE)
levels(datos$intervalos)

# Contar las frecuencias de cada intervalo

conteo_intervalos <- table(datos$intervalos)

# Convertir el resultado a un data frame para una mejor visualización

conteo_intervalos_df <- as.data.frame(conteo_intervalos)

# Renombrar las columnas para mayor claridad

colnames(conteo_intervalos_df) <- c("Intervalo", "Frecuencia")

# Calcular el punto medio de cada intervalo

marca_clase <- sapply(strsplit(as.character(conteo_intervalos_df$Intervalo), ",|\\[|\\(|\\]"), function(x) {
  mean(as.numeric(x[2:3]))
})

# Agregar la columna "marca_clase" al data frame

conteo_intervalos_df$marca_clase <- marca_clase

#Cambiar el orden de las columnas en el data frame con dplyr

conteo_intervalos_df <- conteo_intervalos_df %>% select(Intervalo, marca_clase, Frecuencia)

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
  kable(caption = "Distribución de frecuencias agrupadas en intervalos de los salarios de la empresa",
        col.names = c("Intervalo salarial", "Marca de clase x(i)", "Frecuencia absoluta n(i)",
                      "Frecuencia absoluta acum. N(i)", "Frecuencia relativa f(i)",
                      "Frecuencia relativa acum. F(i)"),
        format.args = list(decimal.mark = ".", digits = 2)) %>%
  kable_styling(full_width = F, bootstrap_options = "striped", "bordered", "condensed", position = "center", font_size = 11) %>%
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

library(DescTools)
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

cuasivarianza <- var (datos$SALARIO)
cuasivarianza

# Coeficiente de variación

cvariacion <- desv / abs(media)
cvariacion

# Coeficiente de asimetría de Fisher

library(moments)
asimetria <- skewness(datos$SALARIO)
asimetria

library (ggplot2)
ggplot(data = datos, map = aes(x = SALARIO)) +
  geom_histogram(bins = 7, colour = "red", fill = "orange") +
  geom_vline(aes(xintercept = mean(SALARIO)), colour = "darkblue", linetype = "dashed", size = 1) +
  ggtitle("SALARIO MENSUAL", subtitle = "trabajadores de la empresa XXX")+
  xlab("Salario (cientos de euros)") +
  ylab("Frecuencias")

# Coeficiente de apuntamiento o curtosis de Fisher

curtosis <- kurtosis(datos$SALARIO)
curtosis

ggplot(data = datos, map = aes(x = SALARIO)) +
  geom_histogram(bins = 7, colour = "red", fill = "orange", aes(y = ..density..)) +
  stat_function(fun = dnorm, args = list(mean = moda, sd = sd(datos$SALARIO)), colour = "darkblue", size = 1) +
  ggtitle("SALARIO MENSUAL", subtitle = "trabajadores de la empresa XXX")+
  xlab("Salario (cientos de euros)") +
  ylab("Densidad")
