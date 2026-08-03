# -----------------------------------------------------------------------------
# ENTIDAD SITUACION_ADMIN_HIJO
#
# Objetivo:
# Construir la información administrativa asociada a los recién nacidos
# incluidos en la cohorte de estudio.
#
# Únicamente se conservan los recién nacidos presentes en la entidad HIJO,
# garantizando la coherencia entre todas las entidades del modelo.
# -----------------------------------------------------------------------------

# 1. Importar los datos principales de la entidad 
hijo_demograficos <- read_delim(FILE_HIJO_DEMOGRAFICOS,
                                delim = "|", escape_double = FALSE, trim_ws = TRUE)

# 2. Limpieza y estandarización de los datos
hijo_demograficos <- hijo_demograficos %>%
  clean_names() %>% 
  distinct() %>% 
  rename(
    id_hijo = patient_id,
    indice_privacion = ind_privacion
  ) %>% 
  mutate(
    altabdu_dt = as.Date(altabdu_dt, format = "%d/%m/%Y"),
    bajabdu_dt = as.Date(bajabdu_dt, format = "%d/%m/%Y")
  ) %>%
  inner_join(
    hijo,
    by = "id_hijo"
  )

# 3. Filtrar los registros administrativos para conservar únicamente
#    los recién nacidos presentes en la entidad HIJO
situacion_admin_hijo <- hijo_demograficos %>%
  semi_join(
    hijo %>%
      distinct(id_hijo),
    by = "id_hijo"
  ) %>%
  
  # 4. Generar una clave primaria artificial para identificar de forma única
  #    cada registro administrativo
  mutate(
    id_admin_hijo = row_number()
  ) %>%
  relocate(id_admin_hijo) %>%
  
  # 5. Conservar únicamente las variables definidas para la entidad
  #    SITUACION_ADMIN_HIJO y reordenarlas según el modelo E/R
  select(
    id_admin_hijo,
    id_hijo,
    tsi,
    zbs,
    indice_privacion,
    altabdu_dt,
    bajabdu_dt,
    motivo_baja
  ) %>%
  
  # 6. Adaptar las fechas al formato definido (YYYYMMDD) para la exportación
  mutate(
    altabdu_dt = format(altabdu_dt, "%Y%m%d"),
    bajabdu_dt = format(bajabdu_dt, "%Y%m%d")
  ) 

# 7. Exportar la entidad SITUACION_ADMIN_HIJO
write_csv(
  situacion_admin_hijo, 
  file.path(PATH_TRANSFORMADOS,"situacion_admin_hijo.csv")
)
