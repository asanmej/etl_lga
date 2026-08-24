
# 00_config.R: 

## Carga de librerías

pkg <- c(
  
  # 0. Creación tabla de clasificación LGA (parámetros)  
  ## Extraer contenido de un pdf
  "pdftools",
  
  # 1. Creación de entidades (ETL)
  ## Manipulación de datos y joins, manejo de fechas y lectura de archivos csv
  "tidyverse",
  ## Limpieza de nombres de variables
  "janitor",
  ## Para establecer el directorio del proyecto
  "here",
  
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
  ## Formato de porcentajes, ejes y escalas
  "scales",
  
  # 4. Análisis de sensibilidad
  ## Emparejamiento mediante propensity score
  "MatchIt",
  ## Evaluación del balance entre grupos tras el emparejamiento
  "cobalt",
  
  # 5. Diagnóstico de modelos
  "car", # vif()
  "pscl", # pR2()
  "ResourceSelection", # hoslem.test()
  "pROC" # roc()
)

lapply(pkg, function (x){if(!require(x, character.only = T)){install.packages(x , character.only = T)}})
rm(pkg)

## Directorios
PATH_PROYECTO <- here()

PATH_DATOS <- file.path(PATH_PROYECTO, "Datos")
PATH_R <- file.path(PATH_PROYECTO, "R")
PATH_RESULTS <- file.path(PATH_PROYECTO, "Results")
PATH_TABLAS <- file.path(PATH_RESULTS, "tablas")

## Datos transformados
PATH_TRANSFORMADOS <- file.path(PATH_PROYECTO, "Datos_transformados")

## Parámetros
PATH_PARAMS <- file.path(PATH_PROYECTO, "Parametros")

## Crear automáticamente las carpetas de salida si no existen
dirs <- c(
  PATH_RESULTS,
  PATH_TRANSFORMADOS,
  PATH_PARAMS,
  PATH_TABLAS
)

invisible(
  lapply(
    dirs,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

## CSV originales
PATH_CSV_20240508 <- file.path(PATH_DATOS, "csv_20240508")
PATH_CSV_20240625 <- file.path(PATH_DATOS, "csv_20240625")
PATH_CSV_20260616 <- file.path(PATH_DATOS, "csv20260616")


## MADRE
FILE_MADRE_CARTILLA      <- file.path(PATH_CSV_20240625, "madre_cartilla.csv")
FILE_MADRE_DEMOGRAFICOS  <- file.path(PATH_CSV_20240508, "madre_demograficos.csv")
FILE_MADRE_DGP           <- file.path(PATH_CSV_20260616, "madre_dgp.csv")
FILE_MADRE_DIAG_OMI      <- file.path(PATH_CSV_20240508, "madre_diag_omi.csv")
FILE_MADRE_DIAG_CMBD     <- file.path(PATH_CSV_20240508, "madre_diag_cmbd.csv")

## HIJO
FILE_HIJO_NEOSOFT        <- file.path(PATH_CSV_20240508, "hijo_neosoft.csv")
FILE_HIJO_DEMOGRAFICOS   <- file.path(PATH_CSV_20240508, "hijo_demograficos.csv")
FILE_HIJO_DIAG_OMI      <- file.path(PATH_CSV_20240508, "hijo_diag_omi.csv")
