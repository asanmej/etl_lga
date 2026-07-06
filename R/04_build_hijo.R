
source("00_config.R")


# Importar las fuentes de datos necesarias para construir la entidad HIJO
hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/hijo_demograficos.csv", 
                                delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Estandarizar nombres de variables y eliminar posibles registros duplicados
hijo_neosoft <- hijo_neosoft %>%  
  clean_names() %>%  # Convertir a formato estandar: minúsculas, sin tildes ni espacios
  distinct()         # Eliminar duplicados


# Seleccionar los atributos principales que definen la entidad HIJO
hijo <- hijo_neosoft %>%
  select(
    patient_id,
    peso_nacimiento,
    talla_nacimiento,
    perimetro_craneal,
    edad_gestacional,
    malformation_cd,
    muerte_neonatal,
  ) %>%
  distinct()

# Construir la fecha de nacimiento a partir del año y el mes disponibles.
# Al desconocerse el día, se asigna el primer día del mes según la convención
# definida en la documentación del proyecto.
hijo_demograficos <- hijo_demograficos %>%
  mutate(
    fecha_nacimiento = as.Date(
      sprintf("%04d-%02d-01", ano_nac, mes_nac)
    )
  )

# Conservar únicamente las variables necesarias para incorporar la fecha de nacimiento
hijo_demograficos <- hijo_demograficos %>%
  select(patient_id, fecha_nacimiento)

# Incorporar la fecha de nacimiento a la entidad HIJO
hijo <- hijo %>%
  left_join(
    hijo_demograficos,
    by = "patient_id"
  )

# Adaptar los nombres de las variables a la nomenclatura definida en el modelo E/R
hijo <- hijo %>% 
  rename(id_hijo = patient_id, 
         edad_gestacional_nacimiento = edad_gestacional)

#View(hijo)

# El atributo id_embarazo se incorporará durante la construcción de la entidad EMBARAZO,
# una vez se definan las reglas de asociación entre madre, embarazo y recién nacido