# Importar las fuentes de datos necesarias para construir la entidad HIJO
# hijo_neosoft, hijo_demograficos, madre_cartilla y embarazos_aux
# se generan en 03_reconstruccion_embarazos.R

# Limpieza en 03_reconstruccion_embarazos.R

# Creamos una tabla de madres válidas que emplearemos al final para filtrar
madres_validas <- hijo_neosoft %>%
  distinct(mother_patient_id) %>%
  rename(id_madre = mother_patient_id) %>%
  inner_join(
    madre_cartilla %>%
      distinct(patient_id) %>%
      rename(id_madre = patient_id),
    by = "id_madre"
  )

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
# definida en la documentación del proyecto
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
    hijo_demograficos %>%
      select(patient_id, fecha_nacimiento),
    by = "patient_id"
  )

# Adaptar los nombres de las variables a la nomenclatura definida en el modelo E/R
hijo <- hijo %>% 
  rename(id_hijo = patient_id, 
         edad_gestacional_nacimiento = edad_gestacional)

# Construir una tabla auxiliar con los hijos y la madre correspondiente
hijos_aux <- hijo_neosoft %>%
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

hijos_aux <- hijos_aux %>%
  left_join(
    embarazos_aux %>%
      select(
        id_embarazo,
        id_madre,
        fecha_parto
      ),
    by = "id_madre"
  ) %>%
  mutate(diferencia = abs(as.numeric(fecha_nacimiento - fecha_parto))
  ) %>%
  group_by(id_hijo) %>%
  slice_min(diferencia,
            n = 1,
            with_ties =  FALSE
  ) %>%
  ungroup()

hijo <- hijo %>%
  left_join(
    hijos_aux %>%
      select(
        id_hijo,
        id_embarazo
      ),
    by = "id_hijo")

# Formatear las fechas siguiendo el estándar YYYYMMDD  
hijo <- hijo %>%
  mutate(
    fecha_nacimiento = format(fecha_nacimiento, "%Y%m%d")
  )

# Filtramos solo las madres que sean válidas y por lo tanto los hijos válidos
hijo <- hijo %>%
  left_join(
    hijo_neosoft %>%
      select(patient_id, mother_patient_id) %>%
      rename(
        id_hijo = patient_id,
        id_madre = mother_patient_id
      ),
    by = "id_hijo"
  ) %>%
  semi_join(
    madres_validas,
    by = "id_madre"
  ) %>%
  select(-id_madre)

# Reordenar variables 
hijo <- hijo %>%
  select(
    id_hijo,
    id_embarazo,
    peso_nacimiento,
    talla_nacimiento,
    perimetro_craneal,
    edad_gestacional_nacimiento,
    malformation_cd,
    muerte_neonatal,
    fecha_nacimiento
  )

write_csv(hijo,"Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/hijo.csv")
