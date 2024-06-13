#Analisis de la base de datos: Polizas------------------------------------------

#Librerias a usar---------------------------------------------------------------
library("data.table")
library("tibble")
library("lubridate")
library("tidyverse")
library("dplyr")
library("ggplot2")

#Importar los datos a un data frame---------------------------------------------
datos <- read_excel("polizas.xlsx")
str(datos)

#----------------------------EJERCICIO N°01-------------------------------------
#Evaluar la calidad de los datos en campos como 'fecha_constitucion', 
#'prima_anual', y 'suma_asegurada'. Identificar y corregir inconsistencias o valores atípicos. 
#-------------------------------------------------------------------------------

#Verificacion de NA's en la base de datos:
colSums(is.na(datos))

#-------------------------------------------------------------------------------

#¿Como visualizar el elemento que esta con NA?

# Obtener índices de los registros con NA en 'nombre de la columna'
#         which(is.na(datos$nombre de la columna))

# Ver registros completos con NA en 'prima_anual'
#       datos[is.na(datos$nombre de la columna), ]

#------------------------------------------------------------------------------

#¿Como se maneja los valores con NA?
#Tenemos dos opciones:
#Eliminar los registros que los contienen
#                   datos_limpios <- na.omit(datos)
#Imputar valores basados en estadísticas como la media o la mediana.
# datos$prima_anual[is.na(datos$nombre de la columna)] <- mean(datos$nombre de la columna, na.rm = TRUE)

#-------------------------------------------------------------------------------

#Identificar los formatos (Fechas)----------------------------------------------
#Notemos que tenemos columnas con respecto a fechas: tenemos las siguientes columnas:
#fechas_constitucion
#anio_mes
#fecha_emision
#fecha de vigencia desde 
#fecha de vigencia hasta

#Conversion de fechas y verificacion de datos----------------------------------
datos$fecha_constitucion <- as.Date(datos$fecha_constitucion, origin = "1899-12-30")
datos$fecha_constitucion


#-------------------------------------------------------------------------------
#Identifiquemos valores atipicos------------------------------------------------
#Vamos a visualizar de manera grafica de la "prima anual" y de "suma asegurada"

#vamos hacer un grafico de dispersion para tener mas claro esto, donde vamos a
#mostrar la media para ver que tanto influye en los datos

media_prima_anual <- mean(datos$prima_anual, na.rm = TRUE)
media_suma_asegurada <- mean(datos$suma_aseg, na.rm = TRUE)
#--------------------------fucnciones----------------------------------------
#graficos de dispersion
calcular_binwidth <- function(data) {
  range <- max(data, na.rm = TRUE) - min(data, na.rm = TRUE)
  bins <- ceiling(log2(length(na.omit(data))) + 1)
  return(range / bins)
}

#histograma
crear_histograma <- function(data, columna, binwidth) {
  # Usar ggplot para crear el histograma
  p <- ggplot(data, aes_string(x = columna)) +
    geom_histogram(binwidth = binwidth, fill = "blue", color = "black") +
    labs(title = paste("Histograma de", columna),
         x = columna,
         y = "Frecuencia") +
    theme_minimal()
  
  # Imprimir el histograma
  print(p)
}
#-------------------------------------------------------------------------------
#suma asegurada
ggplot(datos, aes(x = rep('Suma Asegurada', nrow(datos)), y = suma_aseg)) +
  geom_point(alpha = 0.5, color = "blue") +  # Puntos para cada dato
  geom_point(aes(y = media_suma_asegurada), color = "red", size = 5) +  # Punto para la media
  labs(title = "Dispersión de Suma Asegurada con Media",
       x = "",
       y = "Suma Asegurada") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ajusta la orientación del texto en el eje x si es necesario

# Histograma para 'suma_asegurada' con un ancho de bin especificado
crear_histograma(datos, "suma_aseg", binwidth = 10000)


#------------------------------------prima anual 
ggplot(datos, aes(x = rep('Prima Anual', nrow(datos)), y = prima_anual)) +
  geom_point(alpha = 0.5, color = "blue") +  # Puntos para cada dato
  geom_point(aes(y = media_prima_anual), color = "green", size = 5) +  # Punto para la media
  labs(title = "Dispersión de Prima Aual con Media",
       x = "",
       y = "Primas Anuales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ajusta la orientación del texto en el eje x si es necesario


# Histograma para 'prima_anual' con un ancho de bin especificado
crear_histograma(datos, "prima_anual", binwidth = 100000)

hist(datos$prima_anual)
boxplot(datos$prima_anual)
boxplot(datos$prima_anual, main = "Boxplot de Prima Anual", ylab = "Prima Anual", col = "blue")



#Usaremos la forma mas comun mediante el calculo de los cuartiles y luego verificar
#valores fuera del rango intercuartilico (IQR)

#NOTA: Solo podemos verificar los datos atipicos de las columnas "prima_anual" y "suma_asegurada"

# Prima Anual
Q1_prima <- quantile(datos$prima_anual, 0.25)
Q3_prima <- quantile(datos$prima_anual, 0.75)
IQR_prima <- Q3_prima - Q1_prima

# Suma Asegurada
Q1_suma <- quantile(datos$suma_aseg, 0.25)
Q3_suma <- quantile(datos$suma_aseg, 0.75)
IQR_suma <- Q3_suma - Q1_suma

# Determinar valores atípicos
outliers_prima <- datos$prima_anual < (Q1_prima - 1.5 * IQR_prima) | datos$prima_anual > (Q3_prima + 1.5 * IQR_prima)
outliers_suma <- datos$suma_aseg < (Q1_suma - 1.5 * IQR_suma) | datos$suma_aseg > (Q3_suma + 1.5 * IQR_suma)

#Veamos cuantos valores atipicos exiten en cada columna
sum(outliers_prima)
sum(outliers_suma)

#Correcion de valores atipicos
# Tratamiento de valores atípicos para prima anual
datos$prima_anual[outliers_prima] <- ifelse(datos$prima_anual[outliers_prima] > (Q3_prima + 1.5 * IQR_prima), Q3_prima + 1.5 * IQR_prima, 
                                     ifelse(datos$prima_anual[outliers_prima] < (Q1_prima - 1.5 * IQR_prima), Q1_prima - 1.5 * IQR_prima,
                                     datos$prima_anual[outliers_prima]))

# Tratamiento de valores atípicos para suma asegurada
datos$suma_aseg[outliers_suma] <- ifelse(datos$suma_aseg[outliers_suma] > (Q3_suma + 1.5 * IQR_suma), Q3_suma + 1.5 * IQR_suma, 
                                       ifelse(datos$suma_aseg[outliers_suma] < (Q1_suma - 1.5 * IQR_suma), Q1_suma - 1.5 * IQR_suma,
                                       datos$suma_aseg[outliers_suma]))
#Revision y confirmacion 
summary(datos$prima_anual)
summary(datos$suma_aseg)

#-----------------------FIN DEL EJERCICIO N°01----------------------------------

#----------------------------EJERCICIO N°02-------------------------------------
#Análisis Exploratorio en Relación a la Sucursal y el Ramo Comercial: 
#Investigar si existe alguna correlación entre la 'sucursal' y el 'ramo_comercial' en términos de 'prima_emitida'. 
#Esto implicaría agrupar los datos por sucursal y ramo comercial, y luego realizar análisis estadísticos y gráficos 
#para identificar patrones.

#Agrupacion de los datos.
agrupado <- datos %>%
  group_by(sucursal, ramo_comercial) %>%
  summarise(prima_emitida_promedio = mean(prima_emitida, na.rm = TRUE)) #me va a mantener los grupos por sucursal 

#investigacion de correlacion mediante ANOVA
anova_result <- aov(prima_emitida_promedio ~ sucursal + ramo_comercial, data = agrupado)
summary(anova_result)

#Visualizacion de los datos
ggplot(agrupado, aes(x = sucursal, y = prima_emitida_promedio, fill = ramo_comercial)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_minimal() +
  labs(title = "Prima Emitida Promedio por Sucursal y Ramo Comercial",
       x = "Sucursal",
       y = "Prima Emitida Promedio")

















