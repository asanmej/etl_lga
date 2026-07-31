
# 00_config.R: 

## Carga de librerías

pkg <- c(
  
  # 1. Creación de entidades (ETL)
  ## Manipulación de datos y joins, manejo de fechas y lectura de archivos csv
  "tidyverse",
  ## Limpieza de nombres de variables
  "janitor",
  
  # 2. Análisis de calidad (QA)
  ## Resúmenes descriptivos y calidad de datos
  "skimr",
  ## Valores perdidos (NAs)
  "naniar",
  ## Para guardar el HTML en un directorio distinto
  "rmarkdown",
  
  # 3. Análisis descriptivo
  ## Elaboración de tablas descriptivas
  "gtsummary",
  ## Formato y presentación de tablas
  "gt",
  ## Conversión de resultados estadísticos en tablas ordenadas (tidy)
  "broom",
  ##
  "scales",
  
  # 4. Análisis de sensibilidad
  ## Emparejamiento mediante propensity score
  "MatchIt",
  ## Evaluación del balance entre grupos tras el emparejamiento
  "cobalt"
)

lapply(pkg, function (x){if(!require(x, character.only = T)){install.packages(x , character.only = T)}})
rm(pkg)


## Rutas del proyecto empleadas en Main.R

PATH_PROYECTO <- "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo"

PATH_DATOS <- file.path(PATH_PROYECTO, "Datos")
PATH_R <- file.path(PATH_DATOS, "R")
PATH_RESULTS <- file.path(PATH_PROYECTO, "Results")

