# -----------------------------------------------------------------------------
# MAIN
#
# Objetivo:
# Ejecutar de forma secuencial todo el flujo ETL, el control de calidad y el
# análisis estadístico del proyecto de salud perinatal.
#
# Flujo de ejecución:
#   1. Cargar la configuración general del proyecto
#   2. Construir las entidades del modelo E/R
#   3. Ejecutar el control de calidad de los datos
#   4. Generar el dataset analítico
#   5. Ejecutar el análisis descriptivo
#   6. Ajustar los modelos de regresión logística
# -----------------------------------------------------------------------------

# 1. Configuración del proyecto
source("R/00_config.R")

# 2. Construcción de entidades del modelo E/R

## Entidad MADRE
source(file.path(PATH_R, "01_build_madre.R"))

## Reconstrucción cronológica de embarazos
source(file.path(PATH_R, "02_reconstruccion_embarazos.R"))

## Entidad EMBARAZO
source(file.path(PATH_R, "03_build_embarazo.R"))

## Entidad HIJO
source(file.path(PATH_R, "04_build_hijo.R"))

## Entidad SITUACION_ADMIN_MADRE
source(file.path(PATH_R, "05_build_situacion_admin_madre.R"))

## Entidad SITUACION_ADMIN_HIJO
source(file.path(PATH_R, "06_build_situacion_admin_hijo.R"))

## Entidad DIAGNOSTICO
source(file.path(PATH_R, "07_build_diagnostico.R"))

## Entidad USO_SERVICIO
source(file.path(PATH_R, "08_build_uso_servicio.R"))


# 3. Control de calidad
render(file.path(PATH_R, "09_QA.Rmd"), output_dir = PATH_RESULTS)

# 4. Tabla de LGA
source(file.path(PATH_R, "LGA_tabla.R"))

# 5. Construcción del dataset analítico
source(file.path(PATH_R, "10_dataset_analitico.R"))


# 6. Análisis descriptivo
render(file.path(PATH_R, "11_analisis_descriptivo_macrosomia.Rmd"), 
       output_dir = PATH_RESULTS)

# 7. Modelos de regresión logística
render(file.path(PATH_R, "12_modelos_regresion_logistica.Rmd"),
       output_dir = PATH_RESULTS)
