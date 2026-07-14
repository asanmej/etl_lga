
# 00_config.R: Carga de librerías

# ---------------------------------------------------------------------------

# 1. Creación de entidades (ETL)

pkg <- c(
  
  ## Manipulación de datos y joins, manejo de fechas y lectura de archivos csv
  "tidyverse",
  ## Limpieza de nombres de variables
  "janitor"
  
)

lapply(pkg, function (x){if(!require(x, character.only = T)){install.packages(x , character.only = T)}})
rm(pkg)

# ---------------------------------------------------------------------------

# 2. Análisis descriptivo


# ---------------------------------------------------------------------------

# 3. Visualización de datos


# ---------------------------------------------------------------------------

# 4. Modelos de regresión


