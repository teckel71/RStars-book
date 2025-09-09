# Explorando a las empresas eolicas (disculpad la falta de tildes)

rm(list = ls())

# DATOS

library(readxl)
eolica_20 <- read_excel("eolica_20.xlsx", sheet = "Top 20")
summary (eolica_20)
eolica_20 <- data.frame(eolica_20, row.names = 1)
summary (eolica_20)

# Fin de script :)