#theme: [default, custom.scss]

install.packages("MASS") 
install.packages("reshape2") 
install.packages("vcd") 

# Cargar las bibliotecas necesarias
library(readxl)  # Para leer archivos Excel
library(dplyr)   # Para manipulación de datos
library(ggplot2) # Para visualización de datos
library(tidyr)   # Para manipular la estructura de los datos
library(MASS) 
library(reshape2) 
library(reshape) 

# Leer el archivo de Excel
datos <- read_excel("polizas.xlsx")

tabla_contingencia <- datos %>%
  group_by(sucursal, ramo_comercial) %>%
  summarise(prima_total = sum(prima_emitida, na.rm = TRUE), .groups = 'drop')

matriz_contingencia <- xtabs(prima_total ~ sucursal + ramo_comercial, data = tabla_contingencia)

prueba_chi <- chisq.test(matriz_contingencia)
print(prueba_chi)

residuos <- residuals(prueba_chi, type = "pearson")
print(residuos)

n <- sum(matriz_contingencia)  # Total de observaciones
min_dim <- min(dim(matriz_contingencia) - 1)
cramers_v <- sqrt(prueba_chi$statistic / (n * min_dim))
print(cramers_v)

library(reshape2)
datos_ajustados <- melt(as.table(matriz_contingencia))
ggplot(data = datos_ajustados, aes(x = 'sucursal', y = 'ramo_comercial', fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(x = "Sucursal", y = "Ramo Comercial", fill = "Prima Total") +
  theme_minimal()

mosaicplot(matriz_contingencia, main = "Gráfico de Mosaico: Sucursal vs Ramo Comercial", color = TRUE)

library(vcd)
assocplot(matriz_contingencia)

# Generar un data.frame en donde se indica el tipo de dato y la cantidad de valores no nulos por columna
df_tipos <- data.frame(
  Tipo_Dato = sapply(datos, class),
  Valores_no_nulos = sapply(datos, function(x) sum(!is.na(x)))
)

# Para visualizar el nuevo dataframe para entender el tipo de datos
View(df_tipos)

View(datos)
# Estructura de los datos para ver los tipos de cada columna
str(datos)

# Contar valores no nulos por columna
datos_no_nulos <- datos %>% 
  summarise(across(everything(), ~sum(!is.na(.))))

# Transponer los resultados y convertirlos a un dataframe
datos_no_nulos_transpuesto <- as.data.frame(t(datos_no_nulos))

# Nombrar las columnas correctamente
colnames(datos_no_nulos_transpuesto) <- c("Cantidad_No_Nulos")

# Mostrar el dataframe transpuesto
View(datos_no_nulos_transpuesto)


