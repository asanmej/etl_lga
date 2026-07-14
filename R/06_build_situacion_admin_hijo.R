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

# Adaptar los nombres de los atributos a la nomenclatura definida en el modelo E/R
situacion_admin_hijo <- hijo_demograficos %>% 
  rename(
    id_hijo = patient_id,
    indice_privacion = ind_privacion
  )

# Generar una clave primaria artificial para identificar de forma única
# cada registro administrativo
situacion_admin_hijo <- situacion_admin_hijo %>%
  mutate(
    id_admin_hijo = row_number()
  ) %>%
  relocate(id_admin_hijo)

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

# Adaptar las fechas al formato YYYYMMDD definido para la exportación
situacion_admin_hijo <- situacion_admin_hijo %>%
  mutate(
    altabdu_dt = format(altabdu_dt, "%Y%m%d"),
    bajabdu_dt = format(bajabdu_dt, "%Y%m%d")
  )

write_csv(situacion_admin_hijo, "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/situacion_admin_hijo.csv")
