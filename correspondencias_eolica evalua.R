## Analisis de correspondencias simple de eolicas
## Disculpen por la falta de tildes!

rm(list = ls())

## DATOS

## Importando datos

library (readxl)
eolicas <- read_excel("teae_evalua_05.xlsx", sheet = "GRUPO_01")
eolicas <- data.frame(eolicas, row.names = 1)
summary (eolicas)

## Seleccionando factores/atributos para el analisis

library(dplyr)
originales<-select(eolicas, EMPLEADOS, FORMAJ)
summary (originales)

## Identificando missing values.

library(visdat)
vis_miss(originales)
originales %>% filter(is.na(EMPLEADOS) | is.na(FORMAJ)) %>%
  select(EMPLEADOS, FORMAJ)  
originales <- originales %>%
  filter(! is.na(EMPLEADOS) & ! is.na(FORMAJ))  

## Construyendo Tabla de contingencia

tab.originales <- table(originales)

library(knitr)
library(kableExtra)
knitr.table.format = "html"

addmargins(tab.originales) %>%
  kable(caption="Empresas e?licas") %>%
  kable_styling(full_width = F, bootstrap_options = "striped", "bordered", "condensed", position = "center", font_size = 12) %>%
  add_header_above(c("AUTOFINANCIACION"= 1, DIMENSION=3, " "=1), bold=T, line=T) %>%
  row_spec(0, bold= T, align = "c") %>%
  column_spec(1, bold = T)

library (vcd)
mosaic(tab.originales,
       main="E?licas: Capac. autofinanciaci?n y Dim. grupo empresarial.",
       shade=T,
       gp= shading_Marimekko(tab.originales),
       main_gp = gpar(fontsize = 14), sub_gp = gpar(fontsize = 12))

traspuesta = t(tab.originales)
traspuesta
mosaic(traspuesta,
       main="E?licas: Capac. autofinanciaci?n y Dim. grupo empresarial (2).",
       shade=T,
       gp= shading_Marimekko(traspuesta),
       main_gp = gpar(fontsize = 14), sub_gp = gpar(fontsize = 12))

## Representando frecuencias de categorias en factores

library (ggplot2)
library (patchwork)

g1 <- ggplot(originales, mapping= aes(x= EMPLEADOS, fill = EMPLEADOS)) +
  geom_bar() +
  ggtitle("Tama?o del grupo empresarial", subtitle = "Empresas e?licas") + 
  ylab("Frecuencias") +
  xlab("Dimensi?n") 

g2 <- ggplot(originales, mapping= aes(x= FORMAJ, fill = FORMAJ)) +
  geom_bar() +
  ggtitle("Capacidad de autofinanciaci?n", subtitle = "Empresas e?licas") + 
  ylab("Frecuencias") +
  xlab("Capacidad de autofinanciaci?n") 

(g1 + g2) + plot_annotation(title = "Frecuencias Marginales.",
                            theme = theme(plot.title = element_text(size = 14)))


#TEST DE INDEPENDENCIA / ASOCIACION

chisq.test(tab.originales)

library (DescTools)
CramerV(tab.originales)

assoc(tab.originales,
      main="E?licas: Capac. autofinanciaci?n y Dim. grupo empresarial.",
      sub = "Asociaci?n",
      compress= F,
      gp= shading_Friendly(tab.originales),
      main_gp = gpar(fontsize = 14), sub_gp = gpar(fontsize = 12),
      legend=TRUE)


## ANALISIS DE CORRESPONDENCIAS

library (FactoMineR)
aceolicas<-CA(X = tab.originales, graph = F)
summary (aceolicas)

library(factoextra)

gcontrib <- fviz_screeplot(aceolicas, addlabels= F, barcolor= "darkblue", barfill= "orange", linecolor= "red") +
  labs(title= "Contribuci?n de los ejes a la Inercia Total.", subtitle = "E?licas: C. Autofinanciaci?n y Dimensi?n grupo empresarial.") +
  ylab("Porcentaje de Inercia Total") +
  xlab("Eje") +
  theme(text = element_text(size = 12))

gcontrib

## Contribuci?n filas a Dimensiones

grow_dim1 <- fviz_contrib(aceolicas,
                          choice = "row",
                          axes = 1,
                          fill = "orange",
                          color = "darkblue") +
  theme(text = element_text(size = 8))

grow_dim2 <- fviz_contrib(aceolicas,
                          choice = "row",
                          axes = 2,
                          fill = "orange",
                          color = "darkblue") +
  theme(text = element_text(size = 8))

## Contribuci?n columnas a Dimensiones

gcol_dim1 <-fviz_contrib(aceolicas,
                         choice = "col",
                         axes = 1,
                         fill = "orange",
                         color = "darkblue") +
  theme(text = element_text(size = 8))

gcol_dim2 <-fviz_contrib(aceolicas,
                         choice = "col",
                         axes = 2,
                         fill = "orange",
                         color = "darkblue") +
  theme(text = element_text(size = 8))

(grow_dim1 + grow_dim2) / (gcol_dim1 + gcol_dim2)

## Gr?fico bidimensional

gbiplot <- fviz_ca_biplot (aceolicas,
                           axes= c(1,2),
                           label= "all",
                           repel = T,
                           col.col= "orange", col.row= "darkblue",
                           map= "symmetric") +
  labs(title= "Gr?fico de dispersi?n de categor?as.", subtitle = "E?licas: C. Autofinanciaci?n y Dimensi?n grupo empresarial.") +
  theme(text = element_text(size = 12))

gbiplot

gcontrib / (grow_dim1 + grow_dim2) / (gcol_dim1 + gcol_dim2) / gbiplot

