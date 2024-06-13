library(rlang)

datos_modificados_cuartiles <- function(datos, nombre_columna, cuartil_inferior, cuartil_superior) {
  ci <- cuartil_inferior / 100
  cs <- cuartil_superior / 100
  
  # Acceder dinámicamente a la columna usando [[ ]]
  t <- datos[[nombre_columna]]
  
  # Calcular cuartiles y IQR basado en estos cuartiles personalizados
  Qinf <- quantile(t, ci, na.rm = TRUE)
  Qsup <- quantile(t, cs, na.rm = TRUE)
  IQR <- Qsup - Qinf
  
  # Determinar y corregir valores atípicos
  lower_bound <- Qinf - 1.5 * IQR
  upper_bound <- Qsup + 1.5 * IQR
  t <- ifelse(t < lower_bound, lower_bound, ifelse(t > upper_bound, upper_bound, t))
  
  # Actualizar la columna en el dataframe original
  datos[[nombre_columna]] <- t
  
  return(datos)
}



diagrama_caja_bigotes<- function(datos, nombre_columna, cuartil_inferior, cuartil_superior){
  # Convertir porcentajes a decimales
  ci <- cuartil_inferior / 100
  cs <- cuartil_superior / 100
  
  # Extraer la columna de interés
  columna_datos <- datos[[nombre_columna]]
  
  # Calcular los cuártiles personalizados
  Qinf <- quantile(columna_datos, ci, na.rm = TRUE)
  Qsup <- quantile(columna_datos, cs, na.rm = TRUE)
  
  # Crear el boxplot
  p <- ggplot(datos, aes_string(x = "1", y = nombre_columna)) +
    geom_boxplot() +
    geom_hline(yintercept = c(Qinf, Qsup), color = "red", linetype = "dashed") +
    labs(title = paste("Boxplot con Cuártiles Personalizados: ", nombre_columna),
         x = "",
         y = nombre_columna) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  
  # Imprimir el gráfico
  print(p)
}

datos<-read_excel("polizas.xlsx")
diagrama_caja_bigotes(datos, "prima_anual", 98, 99.999)
datos_prima<-datos_modificados_cuartiles(datos, "prima_anual" , 25, 80)
diagrama_caja_bigotes(datos_prima, "prima_anual", 25, 75)

#el valor p 
#pearson 
grafico 

pregunta2 <- function(datos, relacion1, relacion2, numerico1){
  # Convertir nombres de columnas de string a símbolos
  relacion1_sym <- sym(relacion1)
  relacion2_sym <- sym(relacion2)
  numerico1_sym <- sym(numerico1)
  
  # Agrupar y sumarizar para calcular el total
  tabla_conti <- datos %>%
    group_by(!!relacion1_sym, !!relacion2_sym) %>%
    summarise(total_numerico1 = sum(!!numerico1_sym, na.rm = TRUE), .groups = 'drop')
  
  # Convertir símbolos a nombres de columnas para usar en xtabs
  relacion1_char <- as.character(rlang::expr(!!relacion1_sym))
  relacion2_char <- as.character(rlang::expr(!!relacion2_sym))
  
  # Crear matriz de contingencia
  matriz_contingencia <- xtabs(total_numerico1 ~ get(relacion1_char) + get(relacion2_char), data = tabla_conti)
  
  # Realizar prueba chi-cuadrado
  prueba_chi <- chisq.test(matriz_contingencia)
  result1 <- prueba_chi$p.value
  
  # Calcular residuos
  residuos <- residuals(prueba_chi, type = "pearson")
  result2 <- residuos
  
  # Crear gráfico
  grafica <- ggplot(tabla_conti, aes(x = get(relacion1_char), y = total_numerico1, fill = get(relacion2_char))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_minimal() +
    labs(title = "Numerico1 Promedio por Relacion1 y Relacion2",
         x = relacion1,
         y = "Numerico1 Promedio")
  
  # Devolver resultados como una lista
  resultado <- list(Contigencia = matriz_contingencia,
                    Valorp = result1,
                    Residuos = result2,
                    Diagrama = grafica)
  
  return(resultado)
}
# Leer el archivo de Excel
datos <- read_excel("polizas.xlsx")

pregunta2(datos, "sucursal", "ramo_comercial", "prima_emitida")


#PREGUNTA 3---------------------------------------------------------------------


#Conversion de fechas y verificacion de datos----------------------------------
datos$fecha_constitucion <- as.Date(datos$fecha_emision, origin = "1899-12-30")
datos$fecha_constitucion

# Crear un gráfico de línea para la serie de tiempo
grafico_serie_tiempo <- ggplot(datos, aes(x = fecha, y = valor_numerico)) +
  geom_line(group = 1, colour = "blue") +  # Utiliza geom_line para crear una línea
  theme_minimal() +
  labs(title = "Serie de Tiempo de Valor Numérico",
       x = "Fecha",
       y = "Valor Numérico") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclinar las etiquetas de fecha si son demasiadas o muy largas

# Mostrar el gráfico
print(grafico_serie_tiempo)


pregunta3 <- function(datos, fechas, numeros) {
  # Convertir nombres de columnas de string a símbolos y evaluarlos
  fechas_sym <- rlang::sym(fechas)
  numeros_sym <- rlang::sym(numeros)
  
  # Conversion de fechas y verificacion de datos
  # Asegúrate de convertir la columna de fecha correcta
  datos[[fechas]] <- as.Date(datos[[fechas]], origin = "1899-12-30")
  
  # Crear un gráfico de línea para la serie de tiempo
  grafico_serie_tiempo <- ggplot(datos, aes(x = !!fechas_sym, y = !!numeros_sym)) +
    geom_line(group = 1, colour = "skyblue") +  # Utiliza geom_line para crear una línea
    theme_minimal() +
    labs(title = "Serie de Tiempo de Valor Numérico",
         x = "Fecha",
         y = "Valor Numérico") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Inclinar las etiquetas de fecha si son demasiadas o muy largas
  
  # Mostrar el gráfico
  print(grafico_serie_tiempo)
}

# Leer el archivo de Excel
datos <- read_excel("polizas.xlsx")
pregunta3(datos, "fecha_emision", "prima_anual")
