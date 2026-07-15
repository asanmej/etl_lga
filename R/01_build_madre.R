# Importar los datos principales de la entidad MADRE
madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_demograficos.csv", 
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_dgp <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/madre_dgp.csv", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza y estandarización de los datos
madre_cartilla <- madre_cartilla %>%  
  clean_names() %>% # Convertir a formato estandar: minúsculas, sin tildes ni espacios
  distinct() %>% # Eliminar duplicados
  mutate(
    fecha_visita = as.Date(fecha_visita, format = "%d/%m/%Y"),
    fur = as.Date(fur, format = "%d/%m/%Y")
  )  %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

madre_demograficos <- madre_demograficos %>%
  clean_names() %>%
  distinct() %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

madre_dgp <- madre_dgp %>%
  clean_names() %>%
  distinct()%>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

# Cronstrucción de la entidad MADRE

# Creamos una tabla de madres válidas que emplearemos al final para filtrar
# Madres válidas son aquellas que aparecen en madre_cartilla y en hijo_neosoft
# (intersección de madres en ambos csv)
madres_validas <- hijo_neosoft %>%
  distinct(mother_patient_id) %>%
  rename(id_madre = mother_patient_id) %>%
  inner_join(
    madre_cartilla %>%
      distinct(patient_id) %>%
      rename(id_madre = patient_id),
    by = "id_madre"
  )

# Seleccionar las variables demográficas de interés
madre <- madre_demograficos %>%
  select(
    patient_id,
    ano_nac,
    pais_nac,
    nacionalidad
  ) %>%
  distinct()

# Extraer la talla registrada para cada madre

# Recuperar la talla registrada en DGP utilizando el código específico "TALLA"
talla_dgp <- madre_dgp %>%
  filter(str_detect(str_to_upper(dgp_st), "TALLA")) %>%
  mutate(
    result = as.numeric(result)
  ) %>%
  filter(between(result, 100, 210)) %>%
  group_by(patient_id) %>%
  summarise(
    talla_dgp = first(result),
    .groups = "drop"
  )

# Recuperar la talla registrada en la cartilla de embarazo
talla_cartilla <- madre_cartilla %>%
  mutate(
    talla = as.numeric(talla)
  ) %>%
  filter(between(talla,100,210)) %>%
  group_by(patient_id) %>%
  summarise(
    talla_cartilla = first(talla),
    .groups="drop"
  )

# Integrar las tallas procedentes de ambas fuentes de información
madre <- madre %>%
  left_join(
    talla_dgp,
    by = "patient_id"
  ) %>%
  left_join(
    talla_cartilla,
    by = "patient_id"
  )

# Cuando una madre dispone de talla en ambas fuentes, se prioriza la registrada
# en DGP. Si no existe, se utiliza la disponible en la cartilla de embarazo
madre <- madre %>%
  mutate(
    talla = coalesce(talla_dgp,talla_cartilla)
  )

# Conservar únicamente las variables finales y renombrarlas
madre <- madre %>%
  select(
    patient_id,
    ano_nac,
    pais_nac,
    nacionalidad,
    talla
  )

madre <- madre %>% 
  rename(id_madre = patient_id,
         anio_nacimiento = ano_nac,
         pais_nacimiento = pais_nac)

# Filtramos solo las madres que sean válidas
madre <- madre %>%
  semi_join(madres_validas, by = "id_madre")

# Ordenamos las variables
madre <- madre %>%
  select(
    id_madre,
    anio_nacimiento,
    pais_nacimiento,
    nacionalidad,
    talla
  )

write_csv(madre, "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/madre.csv")
