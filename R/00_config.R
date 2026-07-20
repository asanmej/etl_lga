
# 00_config.R: Carga de librerías

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
  
  # 3. Análisis descriptivo
  
  # 4. Modelos de regresión
  
)

lapply(pkg, function (x){if(!require(x, character.only = T)){install.packages(x , character.only = T)}})
rm(pkg)
