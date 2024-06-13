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






