# -----------------------------------------------------------------------------
# ENTIDAD USO_SERVICIO
#
# Objetivo:
# Construir la entidad USO_SERVICIO, que resume la utilización de servicios
# sanitarios durante cada embarazo
#
# Se contabilizan las visitas registradas en atención primaria (OMI),
# hospitalarias (CMBD) y en la cartilla obstétrica, asociándolas al embarazo
# correspondiente
# -----------------------------------------------------------------------------

# 1. Importar los datos necesarios para construir la entidad
madre_diag_omi <- read_delim(FILE_MADRE_DIAG_OMI,
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_diag_cmbd <- read_delim(FILE_MADRE_DIAG_CMBD,
                              delim = "|", escape_double = FALSE, trim_ws = TRUE)

# 2. Limpieza y filtrado de los datos
#
# Se conservan únicamente los registros correspondientes a las madres
# presentes en la entidad MADRE, garantizando la coherencia entre entidades
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

# 3. Calcular el número de visitas de atención primaria por embarazo:

# Si existen varios diagnósticos registrados el mismo día, todos ellos se
# consideran pertenecientes a un único episodio asistencial
uso_atencion_primaria <- madre_diag_omi %>%
  rename(
    id_madre = patient_id
  ) %>%
  inner_join(
    embarazos_aux,
    by = "id_madre"
  ) %>%
  filter(
    diag_dt >= fecha_inicio_embarazo,
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

# 4. Calcular el número de visitas hospitalarias por embarazo:

# Si existen varios diagnósticos registrados el mismo día, todos ellos se
# consideran pertenecientes a un único episodio asistencial
uso_hospital <- madre_diag_cmbd %>%
  rename(
    id_madre = patient_id
  ) %>%
  inner_join(
    embarazos_aux,
    by = "id_madre"
  ) %>%
  filter(
    diag_dt >= fecha_inicio_embarazo,
    diag_dt <= fecha_parto
  ) %>%
  distinct(
    id_embarazo,
    diag_dt
  ) %>%
  group_by(id_embarazo) %>%
  summarise(
    n_visitas_hospitalarias = n(),
    .groups = "drop"
  )

# 5. Integrar la información de utilización de servicios para cada embarazo
uso_servicio <- embarazos_aux %>%
  distinct(id_embarazo) %>%
  left_join(
    uso_atencion_primaria,
    by = "id_embarazo"
  ) %>%
  left_join(
    uso_hospital,
    by = "id_embarazo"
  )

# 6. Calcular el número de visitas registradas en la cartilla obstétrica
#    para cada embarazo:
#
# Cada registro de la cartilla corresponde a una visita obstétrica.
# La tabla madre_cartilla ya ha sido cargada y depurada en
# 02_reconstruccion_embarazos.R, por lo que se reutiliza directamente
visitas_embarazo <- madre_cartilla %>%
  distinct(patient_id, fur, fecha_visita) %>%
  left_join(
    embarazos_aux %>%
      select(
        id_embarazo,
        id_madre,
        fur
      ),
    by = c(
      "patient_id" = "id_madre",
      "fur"
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

# 7. Reemplazar los valores ausentes por cero:

# Los embarazos sin registros asistenciales se consideran con
# cero visitas en el servicio correspondiente
uso_servicio <- uso_servicio %>%
  mutate(
    n_visitas_atencion_primaria =
      replace_na(n_visitas_atencion_primaria, 0),
    
    n_visitas_hospitalarias =
      replace_na(n_visitas_hospitalarias, 0),
    
    n_visitas_embarazo = 
      replace_na(n_visitas_embarazo, 0)
  )

# 8. Generar una clave primaria artificial para identificar de forma única
#    cada registro de utilización de servicios
uso_servicio <- uso_servicio %>%
  mutate(
    id_uso_servicio = row_number()
  ) %>%
  relocate(id_uso_servicio)

# 9. Conservar únicamente las variables definidas para la entidad
#    USO_SERVICIO y reordenarlas según el modelo E/R
uso_servicio <- uso_servicio %>%
  select(
    id_uso_servicio,
    id_embarazo,
    n_visitas_embarazo,
    n_visitas_atencion_primaria,
    n_visitas_hospitalarias
  )

# 10. Exportar la entidad USO_SERVICIO
write_csv(
  uso_servicio, 
  file.path(PATH_TRANSFORMADOS,"uso_servicio.csv")
)
