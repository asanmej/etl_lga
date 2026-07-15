# Importar los archivos que contienen los diagnósticos registrados para madres e hijos
# Diagnósticos de la madre
madre_diag_omi <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_diag_omi.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_diag_cmbd <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_diag_cmbd.csv", 
                              delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Diagnósticos del hijo
hijo_diag_omi <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_diag_omi.csv", 
                            delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Archivo de hijos para filtrar a las madres
hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Creamos una tabla de madres válidas que emplearemos al final para filtrar
madres_validas <- hijo_neosoft %>%
  distinct(mother_patient_id) %>%
  rename(id_madre = mother_patient_id) %>%
  inner_join(
    madre_cartilla %>%
      distinct(patient_id) %>%
      rename(id_madre = patient_id),
    by = "id_madre"
  )

# Limpieza de los datos y filtramos haciendo uso de madres_validas para los diagnosticos
# y la entidad HIJO para los diagnosticos de los hijos (ejecutada anterior mente en Main.R)
madre_diag_omi <- madre_diag_omi %>%
  clean_names() %>%
  distinct() %>%
  rename(id_madre = patient_id) %>%
  semi_join(
    madres_validas,
    by = "id_madre"
  ) %>%
  rename(patient_id = id_madre)

madre_diag_cmbd <- madre_diag_cmbd %>%
  clean_names() %>%
  distinct() %>%
  rename(id_madre = patient_id) %>%
  semi_join(
    madres_validas,
    by = "id_madre"
  ) %>%
  rename(patient_id = id_madre)

hijo_diag_omi <- hijo_diag_omi %>%
  clean_names() %>%
  distinct()

hijo_diag_omi <- hijo_diag_omi %>%
  semi_join(
    hijo %>% select(id_hijo),
    by = c("patient_id" = "id_hijo")
  )

# Identificar el tipo de paciente y el origen de cada diagnóstico
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

# Se unifican los diagnósticos procedentes de las distintas fuentes de información
# en una única entidad, conservando el origen de cada registro.
diagnostico <- bind_rows(
  diag_omi,
  diag_cmbd,
  diag_hijo
)

# Se crea el identificador único de diagnóstico
diagnostico <- diagnostico %>%
  mutate(
    id_diagnostico = row_number()
  ) %>%
  relocate(id_diagnostico)

# Renombrar el identificador del paciente para ajustarlo al modelo E/R:
# El atributo id_paciente puede hacer referencia tanto a una madre como a un hijo,
# diferenciándose mediante la variable tipo_paciente.
diagnostico <- diagnostico %>%
  rename(id_paciente = patient_id)


# Se conservan únicamente las variables definidas para la entidad DIAGNOSTICO
diagnostico <- diagnostico %>%
  select(id_diagnostico,
         id_paciente,
         tipo_paciente,
         diag_dt,
         diag_cd,
         diag_st,
         origen
  )

# Formatear las fechas siguiendo el estándar YYYYMMDD
diagnostico <- diagnostico %>%
  mutate(
    diag_dt = format(diag_dt, "%Y%m%d")
  )

write_csv(diagnostico,"Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/diagnostico.csv")
