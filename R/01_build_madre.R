source("00_config.R")

# Importar los datos principales de la entidad MADRE
madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_demograficos.csv", 
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_dgp <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/madre_dgp.csv", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza y estandarización de los datos
madre_cartilla <- madre_cartilla %>%  
  clean_names() %>% # Convertir a formato estandar: minúsculas, sin tildes ni espacios
  distinct() %>% # Eliminar duplicados
  mutate(
    fecha_visita = as.Date(fecha_visita, format = "%d-%m-%Y"),
    fur = as.Date(fur, format = "%d-%m-%Y")
  )

madre_demograficos <- madre_demograficos %>%
  clean_names() %>%
  distinct()

madre_dgp <- madre_dgp %>%
  clean_names() %>%
  distinct()

# Crontrucción de la entidad MADRE

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
  group_by(patient_id) %>%
  summarise(
    talla_dgp = first(na.omit(result)),
    .groups = "drop"
  )

# Recuperar la talla registrada en la cartilla de embarazo
talla_cartilla <- madre_cartilla %>%
  group_by(patient_id) %>%
  summarise(
    talla_cartilla = first(na.omit(talla)),
    .groups = "drop"
  )

# Combinar ambas fuentes de información
madre <- madre %>%
  left_join(
    talla_dgp,
    by = "patient_id"
  ) %>%
  left_join(
    talla_cartilla,
    by = "patient_id"
  )

# Priorizar la talla procedente de DGP y, si no existe, utilizar la de cartilla
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
  ) %>%
  distinct()

madre <- madre %>% 
  rename(id_madre = patient_id,
         año_nacimiento = ano_nac,
         pais_nacimiento = pais_nac)
