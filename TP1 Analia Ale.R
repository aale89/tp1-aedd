# Agregamos utilidad para levantar archivos desde subdirectorio /datos ----

# importa el paquete rstudioapi
install.packages(rstudioapi)
library("rstudioapi")  

# obtiene directorio del archivo actual
directorio_actual <- dirname(rstudioapi::getSourceEditorContext()$path)

## nombre del subdirectorio donde se encuentran nuestros archivos
subdirectorio <- "datos"

# asignar el directorio obtenido como el actual
dir_trabajo = paste(directorio_actual, "/", subdirectorio, sep = "")
setwd(dir_trabajo)
getwd()
#########################################################

# Parte I: Lectura del archivo .csv del SNIC ----

### Filtra del .csv original los valores que nos interesan
### provincia_id = 2 (CABA),
### anio >= 2014
### codigo_delito_snic_id = 1 (Homicidio doloso)
filtrar_csv_grande <- function(archivo_csv) {
  
  cat("=== FILTRANDO ARCHIVO CSV ===\n")

  # Leer todo el archivo
  datos <- read.csv(archivo_csv, header = TRUE, sep=";")

  # Aplicar filtros
  datos_filtrados <- subset(datos, provincia_id == 2 & anio >= 2014 & codigo_delito_snic_id == 1)
  
  cat("Filas totales:", nrow(datos), "\n")
  cat("Filtros aplicados: provincia_id == 2 (CABA), año >= 2014, código de delito snic == 1 (Homicidio doloso)")
  cat("Filas filtradas:", nrow(datos_filtrados), "\n")
  
  return(datos_filtrados)
}

datos_filtrados <- filtrar_csv_grande("snic-departamentos-mes-sexo.csv")
View(datos_filtrados)

# Parte III: Función utilizada para exportar los datos a un nuevo archivo .csv ----
validacion_datos_para_exportar <- function(datos) {
  # Verificar que el data frame tenga las columnas necesarias
  columnas_requeridas <- c("anio", "cantidad_victimas_masc", "cantidad_victimas_fem", 
                           "cantidad_victimas_sd", "cantidad_victimas", "porcentaje_masc", 
                           "porcentaje_fem", "porcentaje_sd", "tasa_victimas_100k", "poblacion")
  
  # Comprobar si faltan columnas
  columnas_faltantes <- setdiff(columnas_requeridas, names(datos))
  if(length(columnas_faltantes) > 0) {
    stop(paste("Faltan las siguientes columnas:", paste(columnas_faltantes, collapse = ", ")))
  }
}

exportar <- function(datos, nombre_archivo = "caba_homicidios_dolosos_2014_2023.csv") {
  # Crear el data frame con las columnas en el orden especificado
  datos_exportar <- data.frame(
    anio = as.numeric(datos$anio),
    cantidad_victimas_masc = as.numeric(datos$cantidad_victimas_masc),
    cantidad_victimas_fem = as.numeric(datos$cantidad_victimas_fem),
    cantidad_victimas_sd = as.numeric(datos$cantidad_victimas_sd),
    cantidad_victimas = as.numeric(datos$cantidad_victimas),
    porcentaje_masc = round(as.numeric(datos$porcentaje_masc), 4),
    porcentaje_fem = round(as.numeric(datos$porcentaje_fem), 4),
    porcentaje_sd = round(as.numeric(datos$porcentaje_sd), 4),
    tasa_victimas_100k = round(as.numeric(datos$tasa_victimas_100k), 4),
    poblacion = as.numeric(datos$poblacion)
  )
  
  # Exportar a CSV
  write.csv(datos_exportar, 
            file = nombre_archivo, 
            row.names = FALSE, 
            na = "")
}
