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
                                 
# Fin de script :)