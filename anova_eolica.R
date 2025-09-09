# Análisis ANOVA de un factor.

rm(list = ls())

# DATOS

library (readxl)
datos <- read_excel("eolica_50.xlsx", sheet = "Datos",
                    na = c("n.d.", "s.d."))
datos <- data.frame(datos, row.names = 1)
summary (datos)

  # Missing values

  library (dplyr)
  library(visdat)
  vis_miss(datos)

  datos %>% filter(is.na(RENECO) | is.na(DIMENSION)) %>%
            select(RENECO, DIMENSION)
  muestra <- datos %>%
             filter(! is.na(RENECO) & ! is.na(DIMENSION))

  # Outliers

  library (ggplot2)
  ggplot(data = muestra,
         map = (aes(y = RENECO))) +
  geom_boxplot(fill = "orange") +
  ggtitle("RENTABILIDAD ECONÓMICA",
          subtitle = "100 empresas eólicas") +
  ylab("Rentabilidad Económica (%)")

  Q1 <- quantile (muestra$RENECO, c(0.25))
  Q3 <- quantile (muestra$RENECO, c(0.75))

  muestra %>%
    filter(RENECO > Q3 + 1.5*IQR(RENECO) |
           RENECO < Q1 - 1.5*IQR(RENECO)) %>%
    select(RENECO)
  
  muestra_so <- muestra %>% filter(RENECO <= Q3 + 1.5*IQR(RENECO) &
                                   RENECO >= Q1 - 1.5*IQR(RENECO))

  # Visualizando número de frecuencias y medias de los grupos con dplyr:
  
  library (knitr)
  library (kableExtra)
  knitr.table.format = "html" 

  tablamedias <-  muestra_so %>%
                    group_by(DIMENSION) %>%
                    summarise (observaciones = length(DIMENSION),
                               media = mean(RENECO))

  tablamedias %>%
    kable(format = knitr.table.format,
      caption = "Rentabilidad Económica. Medias por grupos (tamaño matriz).",
      col.names = c("Tamaño", "Observaciones", "Rentabilidad Económica")) %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped", "bordered", "condensed",
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:nrow(tablamedias), bold= F, align = "c")

  # Graficando casos y medias.

  # Crear el gráfico de densidad
  gdensidad <- ggplot(data= muestra_so, aes(x = RENECO, color = DIMENSION, fill = DIMENSION)) +
    geom_density(alpha = 0.3) +
    geom_vline(data = tablamedias, aes(xintercept = media, color = DIMENSION), linetype = "dashed", size = 1) +
    labs(title = "Diagramas de Densidad por Grupo con Medias",
         x = "Rentabilidad Económica (%)",
         y = "Densidad") +
    theme_grey()
  
  # Crear box-plot
  gbox <- ggplot(data = muestra_so,
         map = (aes(y = DIMENSION,
                    x = RENECO,
                    color = DIMENSION,
                    fill = DIMENSION))) +
  geom_boxplot(outlier.shape = NA,
               alpha = 0.3) +
  stat_summary(fun = "mean",
               geom = "point",
               size = 3,
               map = aes(col = DIMENSION),
               alpha = 0.60) +
  geom_jitter(width = 0.1,
              size = 1,
              map = (aes(col = DIMENSION)),
              alpha = 0.40) +
  labs(tittle ="Diagramas de caja por Grupo con Medias",
       xlab = "Rentabilidad Económica (%)",
       ylab = "Submuestras")

  # Combinar gráficos.
  
  library(patchwork)
  
  gdensidad / gbox

# PRERREQUISITOS / HIPÓTESIS ANOVA  

    # Normalidad / Gráfico QQ

    ggplot(data = muestra_so,
           aes(sample = RENECO)) +
    stat_qq(colour = "red") + 
    stat_qq_line(colour = "dark blue") +
    ggtitle("RENTABILIDAD ECONÓMICA: QQ-PLOT",
            subtitle = "Empresas eólicas") +
    facet_grid(. ~ DIMENSION)

    # Normalidad: Shapiro-Wilk para cada grupo
    normalidad <- muestra_so %>%
      group_by(DIMENSION) %>%
    summarise(shapiro_p_value = round(shapiro.test(RENECO)$p.value, 3)) %>%
    mutate(decide = if_else(shapiro_p_value > 0.05,
                            "NORMALIDAD",
                            "NO-NORMALIDAD"))

    tablashapiro <- normalidad %>%
    kable(format = knitr.table.format,
          caption = "Normalidad (Shapiro-Wilks)",
          col.names = c("Dimensión", "p-valor", "Conclusión")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped", "bordered", "condensed",
                  position = "center",
                  font_size = 11) %>%
    row_spec(0, bold= T, align = "c") %>%
    row_spec(1:nrow(normalidad), bold= F, align = "c")

    tablashapiro

    # Homogeneidad en las varianzas

    bartlett.test(muestra_so$RENECO ~ muestra_so$DIMENSION)

# Test F de ANOVA

Datos.aov <- aov(muestra_so$RENECO ~ muestra_so$DIMENSION)
summary_aov <- summary(Datos.aov)

  # Extraer los resultados del ANOVA
  aov_table <- as.data.frame(summary_aov[[1]])

  # Convertir la tabla en una tabla de kable
  aov_table %>%
    kable(format = knitr.table.format,
          caption = "Resultados del ANOVA",
          col.names = c("Grados Libertad",
                        "Suma cuadrados",
                        "Media suma cuadrados",
                        "Estadístico F",
                        "p-valor")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = "striped", "bordered", "condensed",
                  position = "center",
                  font_size = 11) %>%
    row_spec(0, bold= T, align = "c") %>%
    row_spec(1:nrow(aov_table), bold= F, align = "c")

# COMPARACIONES MÚLTIPLES

library(emmeans)
medias <- emmeans(Datos.aov, "DIMENSION")
pares <- pairs(medias)
pares_df <- as.data.frame(pares)
pares_df %>%
  kable(format = knitr.table.format,
        caption = "Resultados comparaciones múltiples",
        col.names = c("Grupos",
                      "Diferencia estimada",
                      "Desviación Típica",
                      "Grados de libertad",
                      "Estadístico t",
                      "p-valor")) %>%
  kable_styling(full_width = F,
                bootstrap_options = "striped", "bordered", "condensed",
                position = "center",
                font_size = 11) %>%
  row_spec(0, bold= T, align = "c") %>%
  row_spec(1:nrow(pares_df), bold= F, align = "c")

#Cuando se incumplen las hipotesis de ANOVA (test robusto)

library(pgirmess)

Datos.K <- kruskal.test(muestra_so$RENECO ~ muestra_so$DIMENSION)
Datos.K

  # Comparaciones múltiples

  Datos.kmc <- kruskalmc(muestra_so$RENECO ~ muestra_so$DIMENSION)

  # Convertir los resultados a un data frame

  Datos.kmc.df <- as.data.frame(Datos.kmc$dif.com)

  Datos.kmc.df %>%
    kable(format = knitr.table.format,
          caption = "Kruskal-Wallis. Múltiples diferencias",
          col.names = c("Diferencias medias",
                        "Diferencias críticas",
                        "Significación")) %>%
    kable_styling(full_width = F,
                  bootstrap_options = c("striped",
                                        "bordered",
                                        "condensed"),
                  position = "center",
                  font_size = 11) %>%
    row_spec(0, bold = T, align = "c") %>%
    row_spec(1:nrow(Datos.kmc.df), bold = F, align = "c")

# Fin del script :)
  
  