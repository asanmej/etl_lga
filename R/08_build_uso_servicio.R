source("00_config.R")

# Importar los datos principales de la entidad USO_SERVICIO
madre_diag_omi <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_diag_omi.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_diag_cmbd <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_diag_cmbd.csv", 
                              delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Archivo para filtrar las madres
hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza de datos
madre_diag_omi <- madre_diag_omi %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    diag_dt = as.Date(diag_dt, format="%d-%m-%Y")
  ) %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

madre_diag_cmbd <- madre_diag_cmbd %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    fecing = as.Date(fecing, format="%d-%m-%Y")
  ) %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

# Cada visita se identifica mediante una fecha distinta para una misma madre.
# Si existen varios diagnósticos registrados el mismo día, todos ellos se
# consideran pertenecientes a un único episodio asistencial.
uso_atencion_primaria <- madre_diag_omi %>%
  distinct(patient_id, diag_dt) %>%
  group_by(patient_id) %>%
  summarise(
    n_visitas_atencion_primaria = n(),
    .groups = "drop"
  ) 

uso_hospital <- madre_diag_cmbd %>%
  distinct(patient_id, fecing) %>%
  group_by(patient_id) %>%
  summarise(
    n_visitas_hospitalarias = n(),
    .groups = "drop"
  )

# Se utiliza un full_join para conservar todas las madres, independientemente
# de que tengan registros únicamente en Atención Primaria, únicamente en CMBD
# o en ambas fuentes de información.
uso_servicio <- uso_atencion_primaria %>%
  full_join(
    uso_hospital, 
    by = "patient_id"
  ) 

# Reemplazar NAs por 0
uso_servicio <- uso_servicio %>%
  mutate(
    n_visitas_atencion_primaria = replace_na(n_visitas_atencion_primaria, 0),
    n_visitas_hospitalarias = replace_na(n_visitas_hospitalarias, 0)
  )

# Crear la clave primaria de la entidad
uso_servicio <- uso_servicio %>%
  mutate(
    id_uso_servicio = row_number()
  ) %>%
  relocate(id_uso_servicio)

# Renombrar el identificador de la madre
uso_servicio <- uso_servicio %>%
  rename(id_madre = patient_id)

# Reordenar las variables según el modelo E/R
uso_servicio <- uso_servicio %>%
  select(
    id_uso_servicio,
    id_madre,
    n_visitas_atencion_primaria,
    n_visitas_hospitalarias
  )
