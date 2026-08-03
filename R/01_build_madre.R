# -----------------------------------------------------------------------------
# ENTIDAD MADRE
#
# Objetivo:
# Construir la entidad MADRE integrando la información procedente de:
#   - Madre cartilla
#   - Datos demográficos
#   - DGP
#
# Se obtiene un único registro por madre válido para la cohorte.
# -----------------------------------------------------------------------------

# 1. Importación de las fuentes de datos
madre_cartilla <- read_delim(FILE_MADRE_CARTILLA, 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_demograficos <- read_delim(FILE_MADRE_DEMOGRAFICOS, 
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_dgp <- read_delim(FILE_MADRE_DGP, 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_neosoft <- read_delim(FILE_HIJO_NEOSOFT, 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

# 2. Limpieza, estandarización y filtrado inicial
madre_cartilla <- madre_cartilla %>%  
  clean_names() %>% # Convertir a formato estandar: minúsculas, sin tildes ni espacios
  distinct() %>% # Eliminar duplicados
  mutate(
    # Conversión de variables de fecha a formato Date
    fecha_visita = as.Date(fecha_visita, format = "%d/%m/%Y"),
    fur = as.Date(fur, format = "%d/%m/%Y")
  )  %>%
  # Conservar únicamente las madres con al menos un recién nacido
  # registrado en NeoSoft
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

madre_demograficos <- madre_demograficos %>%
  clean_names() %>%
  distinct() %>%
  # Conservar únicamente las madres presentes en la cartilla obstétrica.
  # Dado que madre_cartilla ya se encuentra restringida a madres con
  # recién nacidos en NeoSoft, este filtrado mantiene exactamente la
  # misma cohorte
  filter(patient_id %in% madre_cartilla$patient_id) 

madre_dgp <- madre_dgp %>%
  clean_names() %>%
  distinct()%>%
  # Conservar únicamente las madres presentes en la cartilla obstétrica,
  # manteniendo la misma cohorte utilizada para construir la entidad MADRE.
  filter(patient_id %in% madre_cartilla$patient_id)

# 3. Seleccionar las variables demográficas de interés
madre <- madre_demograficos %>%
  select(
    patient_id,
    ano_nac,
    pais_nac,
    nacionalidad
  ) %>%
  distinct()

# 4. Obtención de la talla materna: 
# Prioridad de las fuentes:
#   1. Cartilla obstétrica
#   2. DGP

# Extraer la talla registrada en DGP utilizando el código específico "TALLA"
talla_dgp <- madre_dgp %>%
  filter(str_detect(str_to_upper(dgp_st), "TALLA")) %>%
  mutate(
    result = as.numeric(result)
  ) %>%
  filter(between(result, 100, 210)) %>%
  group_by(patient_id) %>%
  summarise(
    # Si existen varias tallas válidas para una misma madre,
    # se conserva el primer registro disponible
    talla_dgp = first(result),
    .groups = "drop"
  )

# Extraer la talla registrada en la cartilla de embarazo
talla_cartilla <- madre_cartilla %>%
  mutate(
    talla = as.numeric(talla)
  ) %>%
  filter(between(talla,100,210)) %>%
  group_by(patient_id) %>%
  summarise(
    # Si existen varias tallas válidas para una misma madre,
    # se conserva el primer registro disponible
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

# Integración de ambas fuentes de talla:
# Se prioriza la talla registrada en la cartilla obstétrica.
# Cuando ésta no está disponible se utiliza la procedente de DGP.
# La cartilla obstétrica se considera la fuente principal al tratarse
# del registro específico del embarazo
madre <- madre %>%
  mutate(
    talla = coalesce(talla_cartilla,talla_dgp)
  ) %>%
  # Conservar únicamente las variables finales y renombrarlas
  select(
    patient_id,
    ano_nac,
    pais_nac,
    nacionalidad,
    talla
  ) %>%
  rename(id_madre = patient_id,
         anio_nacimiento = ano_nac,
         pais_nacimiento = pais_nac)

# 5. Ordenamos las variables
madre <- madre %>%
  select(
    id_madre,
    anio_nacimiento,
    pais_nacimiento,
    nacionalidad,
    talla
  )

# 6. Exportar la entidad MADRE
write_csv(
  madre, 
  file.path(PATH_TRANSFORMADOS,"madre.csv")
)
