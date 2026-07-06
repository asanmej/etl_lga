
source("00_config.R")

# Importar los datos principales de la entidad MADRE
madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

madre_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_demograficos.csv", 
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Estandarizar nombres de variables, eliminar registros duplicados y convertir las fechas
madre_cartilla <- madre_cartilla %>%  
  clean_names() %>%  # Convertir a formato estandar: minúsculas, sin tildes ni espacios
  distinct() %>%     # Eliminar duplicados
  mutate(
    fecha_visita = as.Date(fecha_visita, format = "%d/%m/%Y"),
    fur = as.Date(fur, format = "%d/%m/%Y")
  )

# Seleccionar únicamente los atributos permanentes de la entidad MADRE
madre <- madre_demograficos %>%
  select(
    patient_id,
    año_nac,
    pais_nac,
    nacionalidad
  ) %>%
  distinct()

# Recuperar la talla de cada mujer a partir de la primera observación disponible
talla <- madre_cartilla %>%
  group_by(patient_id) %>%
  summarise(
    talla = first(na.omit(talla))
  )

# Incorporar la talla a la entidad MADRE
madre <- madre %>%
  left_join(
    talla,
    by = "patient_id"
  )

# Renombrar el identificador siguiendo la nomenclatura del modelo E/R
madre <- madre %>% 
  rename(id_madre = patient_id)

#View(madre)
