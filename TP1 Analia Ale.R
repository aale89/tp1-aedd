# TP 1 Analía Ale

# Importado de paquetes ----
if(!require(rstudioapi)) {
  install.packages("rstudioapi")
  library(rstudioapi)  
}

if(!require(tidyverse)) {
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}

if(!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

if(!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(gt)) {
  install.packages("gt")
  library(gt)
}

if(!require(sf)) {
  install.packages("sf")
  library(sf)
}

if(!require(ggspatial)) {
  install.packages("ggspatial")
  library(ggspatial)
}

if(!require(patchwork)) {
  install.packages("patchwork")
  library(patchwork)
}

# Utilidad para obtener directorio del archivo actual ----

# Asignar el directorio actual para leer desde allí los CSVs y Excels
directorio_actual <- dirname(rstudioapi::getSourceEditorContext()$path)
dir_trabajo = paste(directorio_actual, sep = "")
setwd(dir_trabajo)
getwd()


# Funciones ----

# I) Lectura del archivo .csv del SNIC

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

# II)  Transformación de los datos filtrados
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

# III) Procesado de datos de proyección de población desde CABA
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

# IV) Fusión de los datos de ambos data frames
fusionar_datos <- function(agrupado_snic, proyeccion_caba) {
  datos_fusionados <- agrupado_snic |> left_join(proyeccion_caba, by = "anio") |> 
    mutate(tasa_victimas_100k = round(100000 * cantidad_victimas / poblacion, 2), Comuna = NULL)
  return(datos_fusionados)
}

# V) Función utilizada para exportar los datos a un nuevo archivo .csv
exportar <- function(datos, nombre_archivo) {
  write.csv(datos, file = nombre_archivo, row.names = FALSE)
}

# IV) Gráfico de homicidios dolosos CABA:
crear_grafico_homicidios <- function(datos_fusionados) {
  grafico <- ggplot(datos_fusionados, aes(x = anio)) +
    geom_col(aes(y = cantidad_victimas, fill = "Víctimas"), 
             width = 0.7, alpha = 0.8) +
    geom_smooth(aes(y = cantidad_victimas + 15, color = "Tasa"), 
                method = "loess", se = FALSE, span = 0.5, size = 1.2) +
    geom_point(aes(y = cantidad_victimas + 15, color = "Tasa"), size = 3) +
    geom_text(aes(y = cantidad_victimas/2, label = cantidad_victimas),
              color = "black", fontface = "bold", size = 3.5) +
    geom_text(aes(y = cantidad_victimas + 25, label = gsub("\\.", ",", tasa_victimas_100k)),
              color = "darkgreen", fontface = "bold", size = 3.5) +
    scale_fill_manual(name = "", values = c("Víctimas" = "lightgreen")) +
    scale_color_manual(name = "", values = c("Tasa" = "darkgreen")) +
    scale_x_continuous(breaks = seq(2014, 2023, 1)) +
    scale_y_continuous(name = "", limits = c(0, 250), labels = NULL, 
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Gráfico 1. Homicidios dolosos",
         subtitle = "Víctimas de homicidios dolosos por año. Valores absolutos y tasas cada 100.000 habitantes.\nCiudad Autónoma de Buenos Aires. Años 2014-2023",
         x = "Años",
         caption = "Fuente: Sistema Nacional de Información Criminal -Sistema Alerta Temprana (SNIC-SAT), Ministerio de Seguridad Nacional e INDEC.") +
    theme_minimal() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray30"),
          plot.caption = element_text(hjust = 0.5),
          legend.position = "bottom", legend.direction = "horizontal",
          legend.background = element_blank(), legend.key = element_blank(),
          axis.line.y = element_blank(), panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.spacing.y = unit(0.02, "cm"),    
          legend.box.spacing = unit(0.02, "cm"),  
          legend.margin = margin(0, 0, 0, 0))     
  return(grafico)
}

# VII) Tabla de víctimas de homocidios dolosos por año CABA:
generar_tabla_homicidios_dolosos_caba <- function(datos_agrupados) {
  snic_datos_agrupados <- datos_agrupados |>  
    mutate(
      porcentaje_masculino = round(porcentaje_masculino, 2),
      porcentaje_femenino = round(porcentaje_femenino, 2),
      porcentaje_sd = round(porcentaje_sd, 2)
    ) |>
    rename(
      "Año" = anio,
      "Víctimas Masculinas" = cantidad_victimas_masc,
      "Víctimas Femeninas" = cantidad_victimas_fem,
      "Víctimas Sin Datos" = cantidad_victimas_sd,
      "Total Víctimas" = cantidad_victimas,
      "% Masculino" = porcentaje_masculino,
      "% Femenino" = porcentaje_femenino,
      "% Sin Determinar" = porcentaje_sd
    )
  
  datos_tabla <- snic_datos_agrupados |>
    gt() |>
    cols_align(align = "center") |>
    fmt_number(columns = "Año", decimals = 0, sep_mark = "") |>
    fmt_number(
      columns = c("Víctimas Masculinas", "Víctimas Femeninas", "Víctimas Sin Datos", "Total Víctimas"),
      decimals = 0,
      sep_mark = "."
    ) |>
    fmt_number(
      columns = contains("%"),
      decimals = 2,
      dec_mark = ",",
      sep_mark = "."
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    ) |>
    opt_row_striping() |>
    tab_header(
      title = "Tabla 1. Víctimas de homicidios dolosos por año. Valores absolutos y participación. Ciudad Autónoma de Buenos Aires. Años: 2014-2023."
    ) |>
    tab_source_note(
      source_note = "Fuente: Sistema Nacional de Información Criminal - Sistema Alerta Temprana (SNIC - SAT), Ministerio de Seguridad de la Nación e INDEC."
    )
  
  return(datos_tabla)
}

# VIII) Lectura de geojson
leer_geojson <- function() {
  datos_geo <- st_read("comunas.geojson")
  
  datos_geo <- datos_geo |>
    mutate(
      comuna = as.numeric(str_extract(COMUNAS, "\\d+"))
    )
  
  return(datos_geo)
}

# IX) Generación de mapa con valores absolutos de víctimas
mapa_victimas <- function(datos) {
  return (ggplot(datos) +
            geom_sf(aes(fill = total_victimas), color = "white", size = 0.5) +
            scale_fill_gradient2(
              low = "white", mid = "yellow", high = "red", midpoint = 10,
              name = "Víctimas", breaks = c(0, 5, 10, 15, 20), na.value = "grey90"
            ) +
            geom_sf_text(aes(label = total_victimas), size = 3.5, color = "black", fontface = "bold") +
            annotation_scale(location = "bl", width_hint = 0.2, style = "bar", 
                             line_col = "black", text_col = "black") +
            labs(title = "Valores absolutos por comuna") +
            theme_void() +
            theme(
              plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
              legend.position = "right",
              legend.title = element_text(size = 10, face = "bold"),
              legend.text = element_text(size = 9),
              legend.key.size = unit(0.6, "cm"),
              legend.key.width = unit(0.8, "cm"),
              plot.margin = margin(10, 5, 10, 10)
            ))
}

# X) Generación de mapa con tasa cada 100k habitantes
mapa_tasas <- function(datos){
  return(
    ggplot(datos) +
      geom_sf(aes(fill = tasa_homicidios_100k), color = "white", size = 0.5) +
      scale_fill_gradient2(
        low = "white", mid = "yellow", high = "red", midpoint = 4,
        name = "Tasa cada\n100.000 hab.", breaks = c(0, 2, 4, 6, 8), na.value = "grey90"
      ) +
      geom_sf_text(aes(label = tasa_homicidios_100k), size = 3.5, color = "black", fontface = "bold") +  
      labs(title = "Tasa cada 100.000 habitantes por comuna") +  
      theme_void() +
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 9),
        legend.key.size = unit(0.6, "cm"),
        legend.key.width = unit(0.8, "cm"),
        plot.margin = margin(10, 10, 10, 5)
      )
  )
}

# Datos generales para partes I y II
snic_historico_filtrado <- filtrar_csv_grande("snic-departamentos-mes-sexo.csv")

# Datos generales ----

# Obtenemos los datos del CSV grande que usaremos en ambas partes 
snic_datos_agrupados_original <- transformar_datos(snic_historico_filtrado)

# Parte I ----
caba_proyeccion_poblacion_2025 <- read_excel("caba-poblacion.xls", range="A3:Q19", col_names=TRUE)
caba_datos_agrupados <- procesar_datos_caba(caba_proyeccion_poblacion_2025)
datos_fusionados <- fusionar_datos(snic_datos_agrupados_original, caba_datos_agrupados)
grafico <- crear_grafico_homicidios(datos_fusionados)
tabla_final <- generar_tabla_homicidios_dolosos_caba(snic_datos_agrupados_original)
exportar(datos_fusionados, "caba_homicidios_dolosos_2014_2023.csv")

# Parte II ----

# I) Filtrado de histórico de SNIC por víctimas totales, comuna y año
victimas_por_comuna_2023 <- snic_historico_filtrado |>
  filter(
    anio == 2023,                        
    departamento_nombre != "Departamento sin determinar" 
  ) |>
  group_by(departamento_nombre) |>
  summarise(
    total_victimas = sum(cantidad_victimas, na.rm = TRUE),
    total_hechos = sum(cantidad_hechos, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  mutate(
    numero_comuna = as.numeric(str_extract(departamento_nombre, "\\d+"))
  ) |>
  arrange(numero_comuna) |>  
  select(-numero_comuna)

# II) Leer el archivo excel
# Leer archivo Excel desde la fila 3 y filtrar solo Comuna y 2023
poblacion_por_comuna_2023 <- read_excel("caba-poblacion.xls", 
                                        range="A3:O19",
                                        col_names=TRUE) |>
  select(Comuna, `2023`) |>              # Seleccionar solo Comuna y 2023
  filter(Comuna != "Total") |>          # Excluir la fila Total
  mutate(
    Comuna = as.numeric(Comuna),
    poblacion = as.numeric(`2023`),
    departamento_nombre = paste("Comuna", Comuna)
  ) |>
  select(departamento_nombre, poblacion)

# III) Combinar ambos archivos para tener los datos completos del 2023
caba_homicidios_dolosos_2023_desordenado <- victimas_por_comuna_2023 |>
  left_join(poblacion_por_comuna_2023, by = "departamento_nombre") |>
  mutate(
    tasa_homicidios_100k = round((total_victimas / poblacion) * 100000, 2)
  )

# Ordenar el archivo CSV según lo requerido
caba_homicidios_dolosos_2023 <- caba_homicidios_dolosos_2023_desordenado |>
  mutate(
    comuna = as.numeric(str_extract(departamento_nombre, "\\d+")),
    total_homicidios = total_hechos
  ) |>
  select(comuna, total_homicidios, total_victimas, poblacion, tasa_homicidios_100k) |>
  arrange(comuna)

exportar(caba_homicidios_dolosos_2023, "caba_homicidios_dolosos_2023.csv")

# IV) Generación de mapas usando funciones mapa_victimas y mapa_tasas

# Hacer el merge de datos entre el geojson y el data frame de homicidios dolosos 2023 CABA
mapa_datos_2023 <- leer_geojson() |>
  left_join(caba_homicidios_dolosos_2023, by = "comuna") |>  
  select(
    comuna,        
    total_victimas,          
    poblacion,                
    tasa_homicidios_100k,      
    geometry                   
  )

mapas_finales <- (mapa_victimas(mapa_datos_2023) | mapa_tasas(mapa_datos_2023)) +
  plot_annotation(
    title = "Mapa 1. Víctimas de homicidios dolosos por comuna. Ciudad Autónoma de Buenos Aires. Año 2023.",
    caption = "Fuente: Sistema Nacional de Información Criminal - Sistema Alerta Temprana (SNIC - SAT), Ministerio de Seguridad de la Nación e INDEC.",
    theme = theme(
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.caption = element_text(size = 7, hjust = 0.5)
    )
  )


# Resultados ----

# Pueden ejecutarse los prints siguientes una vez que demos run al código de todas las secciones anteriores
 
# Generar gráfico y tabla de parte I

print(tabla_final)
print(grafico)

# Generar mapas con valores absolutos y cada 100k habitantes de parte II
print(mapas_finales)
