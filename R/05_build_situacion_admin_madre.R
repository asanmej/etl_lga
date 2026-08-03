# -----------------------------------------------------------------------------
# ENTIDAD SITUACION_ADMIN_MADRE
#
# Objetivo:
# Construir la entidad SITUACION_ADMIN_MADRE a partir de la información
# administrativa anual registrada en BDU para las madres incluidas en el estudio
# -----------------------------------------------------------------------------

# 1. Importar la fuente de datos necesaria para construir la entidad
madre_demograficos <- read_delim(FILE_MADRE_DEMOGRAFICOS,
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

# 2. Limpieza y filtrado inicial
madre_demograficos <- madre_demograficos %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    altabdu_dt = as.Date(altabdu_dt, format = "%d/%m/%Y"),
    bajabdu_dt = as.Date(bajabdu_dt, format = "%d/%m/%Y")
  ) %>%
  filter(patient_id %in% embarazos_aux$id_madre)

# 3. Transformar las variables TSI de formato ancho (una columna por año)
#    a formato largo (un registro por madre y año)
tsi <- madre_demograficos %>%
  select(patient_id, altabdu_dt, bajabdu_dt, motivo_baja, starts_with("tsi_")) %>%
  pivot_longer(
    cols = starts_with("tsi_"),
    names_to = "anio",
    names_prefix = "tsi_",
    values_to = "tsi"
  )

# 4. Aplicar la misma transformación para la Zona Básica de Salud (ZBS)
zbs <- madre_demograficos %>%
  select(patient_id, starts_with("zbs_")) %>%
  pivot_longer(
    cols = starts_with("zbs_"),
    names_to = "anio",
    names_prefix = "zbs_",
    values_to = "zbs"
  )

# 5. Aplicar la misma transformación para el índice de privación
ind_priv <- madre_demograficos %>%
  select(patient_id, starts_with("ind_priv_")) %>%
  pivot_longer(
    cols = starts_with("ind_priv_"),
    names_to = "anio",
    names_prefix = "ind_priv_",
    values_to = "indice_privacion"
  )

# 6. Integrar la información administrativa anual mediante el identificador
#    de la madre y el año correspondiente
situacion_admin_madre <- tsi %>%
  left_join(
    zbs,
    by = c("patient_id", "anio")) %>%
  left_join(
    ind_priv,
    by = c("patient_id", "anio")
  )

# 7. Adaptar los nombres de los atributos a la nomenclatura definida en el modelo E/R
situacion_admin_madre <- situacion_admin_madre %>%
  rename(id_madre = patient_id) %>%
  mutate(
    anio = as.numeric(anio)
  )

# 8. La información administrativa se propaga hacia años posteriores cuando no
#    existe un registro para un determinado año, conservando el último valor
#    administrativo disponible para cada madre
situacion_admin_madre <- situacion_admin_madre %>%
  arrange(id_madre, anio) %>%
  group_by(id_madre) %>%
  fill(
    tsi,
    zbs,
    indice_privacion,
    .direction = "down"
  ) %>%
  ungroup()

# 9. Obtener el año de inicio de cada embarazo:

# La información administrativa se asociará al embarazo utilizando el
# año de referencia definido por la FUR. Cuando esta no esté disponible,
# se empleará la fecha estimada de inicio del embarazo.
embarazos_anio <- embarazos_aux %>%
  mutate(
    fecha_referencia = coalesce(
      fur,
      fecha_inicio_embarazo
    ),
    anio = lubridate::year(fecha_referencia),
    anio = if_else(anio == 2017, 2018, anio)
  ) %>%
  # Conservar una única combinación madre-año, ya que pueden existir
  # varios registros del mismo embarazo en el objeto auxiliar
  distinct(
    id_madre,
    anio
  )

# 10. Se conservan únicamente los registros administrativos correspondientes al
#    año de inicio de cada embarazo
situacion_admin_madre <- situacion_admin_madre %>%
  semi_join(
    embarazos_anio,
    by = c("id_madre", "anio")
  )

# 11. Generar una clave primaria artificial para identificar de forma única
#     cada registro administrativo
situacion_admin_madre <- situacion_admin_madre %>%
  mutate(
    id_admin_madre = row_number()) %>%
  relocate(id_admin_madre)

# 12. Ordenar las variables según la estructura definida para la entidad
situacion_admin_madre <- situacion_admin_madre %>%
  select(
    id_admin_madre,
    id_madre,
    anio,
    tsi,
    zbs,
    indice_privacion,
    altabdu_dt,
    bajabdu_dt,
    motivo_baja
  ) %>%
  mutate(
    altabdu_dt = format(altabdu_dt, "%Y%m%d"),
    bajabdu_dt = format(bajabdu_dt, "%Y%m%d")
  )

# 13. Exportar la entidad SITUACION_ADMIN_MADRE 
write_csv(
  situacion_admin_madre, 
  file.path(PATH_TRANSFORMADOS,"situacion_admin_madre.csv")
)
