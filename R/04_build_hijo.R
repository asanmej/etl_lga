
source("00_config.R")


# Importar las fuentes de datos necesarias para construir la entidad HIJO
hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/hijo_demograficos.csv", 
                                delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Estandarizar nombres de variables y eliminar posibles registros duplicados
hijo_neosoft <- hijo_neosoft %>%  
  clean_names() %>%  
  distinct()        

hijo_demograficos <- hijo_demograficos %>%
  clean_names() %>%
  distinct()

# Seleccionar los atributos principales que definen la entidad HIJO
hijo <- hijo_neosoft %>%
  select(
    patient_id,
    peso_nacimiento,
    talla_nacimiento,
    perimetro_craneal,
    edad_gestacional,
    malformation_cd,
    muerte_neonatal
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

# Tabla auxiliar utilizada únicamente durante el proceso ETL para relacionar cada 
# recién nacido con el embarazo correspondiente. No forma parte del modelo entidad-relación final.
hijo_aux <- hijo_neosoft %>%
  select(
    patient_id,
    mother_patient_id
  ) %>%
  left_join(
    hijo_demograficos,
    by = "patient_id"
  ) %>%
  rename(
    id_hijo = patient_id,
    id_madre = mother_patient_id
  ) %>%
  distinct()




# Comprobaciones a ejecutar mañana

## Para ver que no haya duplicados
hijo_aux %>%
  count(id_hijo) %>%
  filter(n > 1)

## Comprobar cuántos hijos tiene cada madre
hijo_aux %>%
  count(id_madre)

## Madres con más de un hijo registrado
hijo_aux %>%
  count(id_madre) %>%
  filter(n > 1)

## Mirar algún ejemplo concreto
hijo_aux %>%
  filter(id_madre == "XXXX")

# Una vez construida la entidad EMBARAZO, comprobar que la fecha de nacimiento
# del hijo permite identificar correctamente el embarazo correspondiente.
# embarazo %>%
#   filter(id_madre == "XXXX")

## Comprobar que para una misma madre no existan dos hijos nacidos el mismo mes.
## Porque si aparecen dos eso son gemelos (o trillizos) Y entonces ambos deberán 
## compartir el mismo id_embarazo
hijo_aux %>%
  count(id_madre, fecha_nacimiento) %>%
  filter(n > 1)



# join final (primero comprovar todo lo anterior)
hijo_aux <- hijo_aux %>%
  left_join(
    embarazo %>%
      select(id_embarazo,
             id_madre,
             fecha_parto),
    by = c(
      "id_madre",
      "fecha_nacimiento" = "fecha_parto"
    )
  )

#View(hijo)

