
source("00_config.R")

# Importar los datos principales de la entidad SITUACION_ADMIN_HIJO
hijo_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/hijo_demograficos.csv", 
                                delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza y estandarización de los datos
hijo_demograficos <- hijo_demograficos %>%
  clean_names() %>% 
  distinct() %>% 
  mutate(
    altabdu_dt = as.Date(altabdu_dt, format = "%d/%m/%Y"),
    bajabdu_dt = as.Date(bajabdu_dt, format = "%d/%m/%Y")
  )

# Crear la clave primaria de la entidad
situacion_admin_hijo <- hijo_demograficos %>%
  mutate(
    id_admin_hijo = row_number()
    ) %>%
    relocate(id_admin_hijo)

# Adaptar los nombres de las variables al modelo E/R
situacion_admin_hijo <- situacion_admin_hijo %>% 
  rename(id_hijo = patient_id,
         indice_privacion = ind_privacion)

# Conservar únicamente las variables definidas para la entidad
# SITUACION_ADMIN_HIJO
situacion_admin_hijo <- situacion_admin_hijo %>%
  select(
    id_admin_hijo,
    id_hijo,
    tsi,
    zbs,
    indice_privacion,
    altabdu_dt,
    bajabdu_dt,
    motivo_baja
  )

#View(situacion_admin_hijo)


## EJECUTAR ESTO, SI DA 0, ESTA BIEN HECHO (1:1) PERO SINO HAY QUE CAMBIAR COSAS
situacion_admin_hijo %>%
  count(id_hijo) %>%
  filter(n > 1)
