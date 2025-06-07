# TP 1 Analía ALe

# Agregamos utilidad para levantar archivos desde subdirectorio /datos ----

# importa el paquete rstudioapi, si se lo necesita
if(!require(rstudioapi)) {
  install.packages("rstudioapi")
  library(rstudioapi)  
}

# importa el paquete tidyverse, si se lo necesita
if(!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

# importa el paquete dplyr, si se lo necesita
if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

#importa el paquete readxl, si se lo necesita
if(!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

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

# Parte II:  Transformación de los datos filtrados
transformar_datos <- function(datos) {
  
  datos_agrupados_por_año = 
  datos |> summarise(
    cantidad_victimas_masc = sum(cantidad_victimas_masc),
    cantidad_victimas_fem = sum(cantidad_victimas_fem),
    cantidad_victimas_sd = sum(cantidad_victimas_sd),
    cantidad_victimas = sum(cantidad_victimas),
    .by = anio
  ) |> mutate(
    porcentaje_masculino = cantidad_victimas_masc / cantidad_victimas * 100,
    porcentaje_femenino = cantidad_victimas_fem / cantidad_victimas * 100,
    porcentaje_sd = cantidad_victimas_sd / cantidad_victimas * 100
  )
  
  return(datos_agrupados_por_año)
}

# Parte III: Procesado de datos de proyección de población desde CABA
# Se pivotean los datos del Excel para lograr un formato similar al CSV del SNIC de la parte I
procesar_datos_caba <- function(datos) {
  datos <- datos |> pivot_longer(cols = -Comuna,
               names_to = "anio",
               values_to = "poblacion"
              ) |> 
    mutate(anio = as.numeric(anio)) |>
    filter(Comuna == "Total")
  return(datos)
}

# Parte IV: Fusión de los datos de ambos data frames
fusionar_datos <- function(agrupado_snic, proyeccion_caba) {
  datos_fusionados <- agrupado_snic |> left_join(proyeccion_caba, by = "anio") |> 
    mutate(tasa_victimas_100k = round(100000 * cantidad_victimas / poblacion, 2), Comuna = NULL)
  return(datos_fusionados)
}

# Parte V: Función utilizada para exportar los datos a un nuevo archivo .csv ----
exportar <- function(datos, nombre_archivo = "caba_homicidios_dolosos_2014_2023.csv") {
  write.csv(datos, file = nombre_archivo, row.names = FALSE)
}


#######
snic_historico_filtrado <- filtrar_csv_grande("snic-departamentos-mes-sexo.csv")
snic_datos_agrupados <-transformar_datos(snic_historico_filtrado)

caba_proyeccion_poblacion_2025 <- read_excel("caba-poblacion.xls", range="A3:Q19", col_names=TRUE)
caba_datos_agrupados <- procesar_datos_caba(caba_proyeccion_poblacion_2025)
datos_fusionados <- fusionar_datos(snic_datos_agrupados, caba_datos_agrupados)
View(datos_fusionados)
exportar(datos_fusionados)
##

