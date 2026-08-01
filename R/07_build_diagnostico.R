# -----------------------------------------------------------------------------
# ENTIDAD DIAGNOSTICO
#
# Objetivo:
# Construir una única entidad de diagnósticos integrando la información
# procedente de OMI y CMBD para las madres y de OMI para los recién nacidos
#
# Cada registro conserva el origen del diagnóstico y el tipo de paciente
# (madre o hijo), permitiendo su trazabilidad
# -----------------------------------------------------------------------------

# 1. Importar los datos necesarios para construir la entidad:

# Diagnósticos de la madre
madre_diag_omi <- read_delim(FILE_MADRE_DIAG_OMI,
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_diag_cmbd <- read_delim(FILE_MADRE_DIAG_CMBD,
                              delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Diagnósticos del hijo
hijo_diag_omi <- read_delim(FILE_HIJO_DIAG_OMI,
                            delim = "|", escape_double = FALSE, trim_ws = TRUE)

# 2. Limpieza y filtrado de los diagnósticos:

# Se conservan únicamente las madres presentes en la entidad MADRE y los
# recién nacidos presentes en la entidad HIJO, garantizando la coherencia
# entre todas las entidades del modelo
madre_diag_cmbd <- madre_diag_cmbd %>%
  clean_names() %>%
  distinct() %>%
  rename(diag_dt = fecing) %>%
  semi_join(
    madre %>% select(id_madre),
    by = c("patient_id"="id_madre")
  )

madre_diag_omi <- madre_diag_omi %>%
  clean_names() %>%
  distinct() %>%
  semi_join(
    madre %>% select(id_madre),
    by = c("patient_id"="id_madre")
  )

hijo_diag_omi <- hijo_diag_omi %>%
  clean_names() %>%
  distinct() %>%
  semi_join(
    hijo %>% select(id_hijo),
    by = c("patient_id"="id_hijo")
  )

# 3. Identificar el tipo de paciente y el origen de cada diagnóstico
diag_omi <- madre_diag_omi %>%
  mutate(
    tipo_paciente = "Madre",
    origen = "OMI"
  )

diag_cmbd <- madre_diag_cmbd %>%
  mutate(
    tipo_paciente = "Madre",
    origen = "CMBD"
  )

diag_hijo <- hijo_diag_omi %>%
  mutate(
    tipo_paciente = "Hijo",
    origen = "OMI"
  )

# 4. Unificar los diagnósticos procedentes de las distintas fuentes de información
#    en una única entidad, conservando el origen de cada registro
diagnostico <- bind_rows(
  diag_omi,
  diag_cmbd,
  diag_hijo
)

# 5. Se crea el identificador único de diagnóstico
diagnostico <- diagnostico %>%
  mutate(
    id_diagnostico = row_number()
  ) %>%
  relocate(id_diagnostico)

# 6. Renombrar el identificador del paciente para ajustarlo al modelo E/R:
# En este punto las tres fuentes mantienen el identificador original
# (patient_id), que pasa a denominarse id_paciente

# El atributo id_paciente puede hacer referencia tanto a una madre como a un hijo,
# diferenciándose mediante la variable tipo_paciente
diagnostico <- diagnostico %>%
  rename(id_paciente = patient_id)


# 7. Se conservan únicamente las variables definidas para la entidad DIAGNOSTICO
diagnostico <- diagnostico %>%
  select(id_diagnostico,
         id_paciente,
         tipo_paciente,
         diag_dt,
         diag_cd,
         diag_st,
         origen
  )

# 8. Formatear las fechas siguiendo el estándar YYYYMMDD
diagnostico <- diagnostico %>%
  mutate(
    diag_dt = format(diag_dt, "%Y%m%d")
  )

# 9. Exportar la entidad DIAGNOSTICO 
write_csv(
  diagnostico,
  file.path(PATH_TRANSFORMADOS,"diagnostico.csv")
)
