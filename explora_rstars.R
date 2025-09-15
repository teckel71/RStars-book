## Importando datos de las empresas R-Stars

# Limpiando el Global Environment
rm(list = ls())

# Cargando paquetes
library(readxl)
library(gtExtras)

# Importando
interestelar_100 <- read_excel("interestelar_100.xlsx", sheet = "Datos", na = c("n.d."))
summary (interestelar_100)
interestelar_100 <- data.frame(interestelar_100, row.names = 1)
summary (interestelar_100)

# visualizando el data frame de modo elegante con {gtExtras}
datos_df_graph <- gt_plt_summary(interestelar_100)
datos_df_graph

# Cargando dplyr
library (dplyr)

# Seleccionando variables
interestelar_100 <-select(interestelar_100, -FJUR)
summary (interestelar_100)

select(interestelar_100, ACTIVO, FPIOS, LIQUIDEZ, MARGEN, SOLVENCIA, APALANCA)
interestelar_100A <-select(interestelar_100, ACTIVO, FPIOS, LIQUIDEZ, MARGEN, SOLVENCIA, APALANCA)
summary (interestelar_100A)

interestelar_100_replica <-select(interestelar_100, everything())

# Seleccionando casos
filter(interestelar_100, RES >= 500)

interestelar_100B <-filter(interestelar_100, RES >= 500 & RENECO < 40)
interestelar_100B

# Ordenando casos
arrange(interestelar_100, RENECO)
arrange(interestelar_100, desc(RENECO))
arrange(interestelar_100, EFLO, desc(RENECO))

# Renombrando variables
interestelar_100 <- rename(interestelar_100, SOLVE = SOLVENCIA)

# Añadiendo variables como transformacion de otras variables
interestelar_100 <- mutate (interestelar_100, RATIO = RES / ACTIVO)
summary(interestelar_100)

interestelar_100 <- arrange(interestelar_100, ACTIVO)
interestelar_100 <- mutate (interestelar_100, ACTIVOS_ACUM = cumsum(ACTIVO))
select(interestelar_100, ACTIVO, ACTIVOS_ACUM)

interestelar_100 <- mutate(interestelar_100,
                           DIM = cut(ACTIVO,
                                     breaks = c(-Inf, 54, 216, Inf),
                                     labels = c("REDUCIDA", "MEDIA", "ALTA")))
select(interestelar_100, ACTIVO, DIM)

interestelar_100 <- interestelar_100 %>%
                    mutate(DIM = cut(ACTIVO,
                                     breaks = c(-Inf, 54, 216, Inf),
                                     labels = c("REDUCIDA", "MEDIA", "ALTA")))
select(ACTIVO, DIM)

#Extrayendo información de las variables de un data frame
summarise(interestelar_100, RENFIN_media = mean(RENFIN))

interestelar_100 %>%
  group_by(DIM) %>%
  summarise(RENFIN_media = mean(RENFIN))

# Exportando data frame a formato R (.RData)
save(interestelar_100, file = "interestelar_100.RData")

# Borrando el data frame interestelar_100
rm(interestelar_100)

# Importando el archivo .RData con los mismos datos
load("interestelar_100.RData")
summary (interestelar_100)

# Exportando el data frame interestelar_100 a Microsoft (R) Excel (R)
library(writexl)
NOMBRE <- row.names(interestelar_100)
interestelar_100n <- cbind(NOMBRE, interestelar_100)
write_xlsx(interestelar_100n, path = "interestelar_100_new.xlsx")

# Fin de script :)