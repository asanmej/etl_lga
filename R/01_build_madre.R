source("00_config.R")

# Importar los datos principales de la entidad MADRE
madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_demograficos.csv", 
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_dgp <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/madre_dgp.csv", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

# madre_dgp tiene este formato: 
# 	dgp_cd      dgp_st          dgp_dt     result    patient_id
#    1AA    TABACO (SI/NO)    2015-01-05     S        001
#    2AA    TABACO (SI/NO)    2015-07-08     S        002
#   ...     alcohol (SI/NI)
#   ...     peso (Kg) 
#   ...     Talla (cm) <- ESTO ES LO QUE NOS INTERESA EL RESTO DE FILAS TANTO POR ARRIBA COMO POR ABAJO NO.
# eN TALLA LO DE RESULTS ES 160, 165.500, ETC, ES DECIR, QUE SON VARIADOS PERO BUENO, ESTAN EN CENTIMETROS Y BIEN LO UNICO ESO LAS FILAS DE ARIBA Y ABAJO NOS DAN IGUAK 

# Limpieza y estandarización de los datos
madre_cartilla <- madre_cartilla %>%  
  clean_names() %>% # Convertir a formato estandar: minúsculas, sin tildes ni espacios
  distinct() %>% # Eliminar duplicados
  mutate(
    fecha_visita = as.Date(fecha_visita, format = "%d/%m/%Y"),
    fur = as.Date(fur, format = "%d/%m/%Y")
  )

madre_demograficos <- madre_demograficos %>%
  clean_names() %>%
  distinct()

madre_dgp <- madre_dgp %>%
  clean_names() %>%
  distinct()

# Crear la tabla
madre <- madre_demograficos %>%
  select(
    patient_id,
    año_nac,
    pais_nac,
    nacionalidad
  ) %>%
  distinct()

# Extraer la talla registrada para cada madre
talla <- madre_dgp %>%
  filter(dgp_st == "TALLA") %>%
  mutate(
    result = as.numeric(result)
  ) %>%
  group_by(patient_id) %>%
  summarise(
    talla = first(na.omit(result)),
    .groups = "drop"
  )

madre <- madre %>%
  left_join(
    talla,
    by = "patient_id"
  )

madre <- madre %>% 
  rename(id_madre = patient_id)

View(madre)
