#| context: server
library(readxl)  # Para leer archivos Excel
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
# Leer el archivo de Excel
path_file = "polizas.xlsx"
datos <- read_excel(path_file)
output$modifiedData <- renderTable({
  # Ensure that the data and column name are available
  if (!exists("datos") || !exists("nombre_columna")) {
    return(data.frame(Message = "Data or column name not defined"))
  }
  
  # Reactively read the slider input
  cuartil_inferior <- input$slider2[1]
  cuartil_superior <- input$slider2[2]
  
  # Call the function to modify the data based on slider inputs
  datos_modificados <- datos_modificados_cuartiles(datos, nombre_columna, cuartil_inferior, cuartil_superior)
  
  # Return the modified data for rendering
  datos_modificados
})