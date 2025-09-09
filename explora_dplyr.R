# Manipulando data frames con Dplyr. Disculpad la falta de tildes!

rm(list = ls())

# Importando datos y visualizando

library(readxl)
eolica_20 <- read_excel("eolica_20.xlsx", sheet = "Top 20")
summary (eolica_20)
eolica_20 <- data.frame(eolica_20, row.names = 1)
summary (eolica_20)

# Cargando dplyr

library (dplyr)

# Seleccionando variables

eolica_20 <-select(eolica_20, -MATRIZ)
summary (eolica_20)
eolica_20

select(eolica_20, ACTIVO, FPIOS, LIQUIDEZ, MARGEN, SOLVENCIA, APALANCA)

eolica_20A <-select(eolica_20, ACTIVO, FPIOS, LIQUIDEZ, MARGEN, SOLVENCIA, APALANCA)
summary (eolica_20A)

eolica_20A <-select(eolica_20, -RES, -RENECO, -RENFIN)
eolica_20A

eolica_20A <-select(eolica_20, -(starts_with("RE")))
summary (eolica_20A)

eolica_20_replica <-select(eolica_20, everything())
summary (eolica_20_replica)

# Seleccionando casos

filter(eolica_20, RES >= 50000)

eolica_20B <-filter(eolica_20, RES >= 50000 & RENECO < 6)
eolica_20B

# Ordenando casos

arrange(eolica_20, RENECO)
arrange(eolica_20, desc(RENECO))
arrange(eolica_20, RENECO, LIQUIDEZ)

# Renombrando variables

eolica_20 <- rename(eolica_20, SOLVE = SOLVENCIA)
summary (eolica_20)

# Añadiendo variables como transformacion de otras variables

eolica_20 <- mutate (eolica_20, RATIO = RES / ACTIVO)
summary(eolica_20)

eolica_20 <- arrange(eolica_20, ACTIVO)
eolica_20
eolica_20 <- mutate (eolica_20, ACTIVOS_ACUM = cumsum(ACTIVO))
eolica_20

eolica_20 <- mutate(eolica_20, TAM = cut(ACTIVO, breaks = c(-Inf, 1000000, Inf), labels = c("P", "G")))
eolica_20
select(eolica_20, ACTIVO, TAM)

eolica_20 <- eolica_20 %>% mutate(TAM = cut(ACTIVO, breaks = c(-Inf, 1000000, Inf), labels = c("P", "G")))
eolica_20
select(eolica_20, ACTIVO, TAM)


# Extrayendo información de las variables de un data frame

summarise(eolica_20, RENFIN_media = mean(RENFIN))

eolica_20 %>%  group_by(TAM) %>% summarise(RENFIN_media = mean(RENFIN))

# Fin del script :)