# Importar los datos principales de la entidad USO_SERVICIO
madre_diag_omi <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_diag_omi.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_diag_cmbd <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_diag_cmbd.csv", 
                              delim = "|", escape_double = FALSE, trim_ws = TRUE)

# hijo_neosoft.csv se importa y se limpia en el script 03_reconstruccion_embarazos.R

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
  rename(
    id_madre = patient_id
  ) %>%
  inner_join(
    embarazos_aux,
    by = "id_madre"
  ) %>%
  filter(
    diag_dt >= fecha_inicio,
    diag_dt <= fecha_parto
  ) %>%
  distinct(
    id_embarazo,
    diag_dt
  ) %>%
  group_by(id_embarazo) %>%
  summarise(
    n_visitas_atencion_primaria = n(),
    .groups = "drop"
  )

uso_hospital <- madre_diag_cmbd %>%
  rename(
    id_madre = patient_id
  ) %>%
  inner_join(
    embarazos_aux,
    by = "id_madre"
  ) %>%
  filter(
    fecing >= fecha_inicio,
    fecing <= fecha_parto
  ) %>%
  distinct(
    id_embarazo,
    fecing
  ) %>%
  group_by(id_embarazo) %>%
  summarise(
    n_visitas_hospitalarias = n(),
    .groups = "drop"
  )

# Unir la información de utilización de servicios por embarazo
uso_servicio <- uso_atencion_primaria %>%
  full_join(
    uso_hospital,
    by = "id_embarazo"
  )

# Incorporar todos los embarazos incluso aquellos sin visitas registradas
uso_servicio <- embarazos_aux %>%
  select(
    id_embarazo
  ) %>%
  distinct() %>%
  left_join(
    uso_servicio,
    by = "id_embarazo"
  )

# Reemplazar NAs
uso_servicio <- uso_servicio %>%
  mutate(
    n_visitas_atencion_primaria =
      replace_na(n_visitas_atencion_primaria, 0),
    
    n_visitas_hospitalarias =
      replace_na(n_visitas_hospitalarias, 0)
  )

# Número de visitas registradas en la cartilla para cada embarazo
visitas_embarazo <- madre_cartilla %>%
  left_join(
    embarazos_aux %>%
      select(
        id_embarazo,
        id_madre,
        fecha_referencia
      ),
    by = c(
      "patient_id" = "id_madre",
      "fecha_referencia"
    )
  ) %>%
  group_by(id_embarazo) %>%
  summarise(
    n_visitas_embarazo = n(),
    .groups = "drop"
  )

uso_servicio <- uso_servicio %>%
  left_join(
    visitas_embarazo,
    by = "id_embarazo"
  )

# Crear la clave primaria
uso_servicio <- uso_servicio %>%
  mutate(
    id_uso_servicio = row_number()
  ) %>%
  relocate(id_uso_servicio)

# Se ordenan las variables en la entidad, tabla, final
uso_servicio <- uso_servicio %>%
  select(
    id_uso_servicio,
    id_embarazo,
    n_visitas_embarazo,
    n_visitas_atencion_primaria,
    n_visitas_hospitalarias
  )

write_csv(uso_servicio, "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/uso_servicio.csv")
