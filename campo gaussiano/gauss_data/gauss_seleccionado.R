# Paquetes útiles
install.packages("tidyverse")
library(dplyr)
library(readr)
# Ruta donde están tus CSV
ruta <- "D:/UNAP/10mo/estadistica espacial/OneDrive_2025-09-10/BBDD_ENA 2014-2024/2024/gauss_data"
# Lista de archivos CSV dentro de esa carpeta
archivos <- list.files(ruta, pattern = "\\.csv$", full.names = TRUE)
# Variables de interés
vars_interes <- c( "ANIO", "P1002B_TOTAL",   # Características financieras
                   "CCPP", "NOMBREPV",       # Provincia
                   "CCDI", "NOMBREDI")       # Distrito

test <- read_csv(archivos[1], n_max = 5, show_col_types = FALSE)
names(test)
# Leer y filtrar cada archivo
lista_datos <- lapply(archivos, function(x) {
  read_csv(x, show_col_types = FALSE) %>%
    select(any_of(vars_interes))
})
# Unir todo en un solo dataframe
datos_final <- bind_rows(lista_datos)

# Guardar el resultado en CSV
write_csv(datos_final, file.path(ruta, "datos_gauss.csv"))

# Vista rápida
glimpse(datos_final)

# 1. Recuento de valores faltantes por columna
colSums(is.na(datos_final))

colMeans(is.na(datos_final)) * 100
summary(datos_final)


write_csv(datos_final, file.path(ruta, "datos_gauss_final.csv"))
