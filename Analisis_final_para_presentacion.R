
# Cargar las bibliotecas necesarias
library(readxl)  # Para leer archivos Excel
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Para visualización de datos
library(tidyr)   # Para manipular la estructura de los datos

# Leer el archivo de Excel
path_file = "polizas.xlsx"
datos <- read_excel(path_file)

# Generar un data.frame en donde se indica el tipo de dato y la cantidad de valores no nulos por columna
df_tipos <- data.frame(
  Tipo_Dato = sapply(datos, class),
  Valores_no_nulos = sapply(datos, function(x) sum(!is.na(x)))
)

df_tipos


