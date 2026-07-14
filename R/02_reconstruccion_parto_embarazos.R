# Reconstrucción cronológica de los embarazos y su asociación con los partos 
# registrados en la información neonatal
#
# Este script genera los objetos auxiliares utilizados por las entidades EMBARAZO 
# e HIJO, evitando duplicar la misma lógica de reconstrucción en ambos procesos ETL

# Importar las fuentes necesarias para reconstruir los embarazos
madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv",
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv",
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/hijo_demograficos.csv",
                                delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza de las fuentes
hijo_neosoft <- hijo_neosoft %>%
  clean_names() %>%
  distinct()

hijo_demograficos <- hijo_demograficos %>%
  clean_names() %>%
  distinct()

madre_cartilla <- madre_cartilla %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    fur = as.Date(fur, format = "%d/%m/%Y"),
    fecha_visita = as.Date(fecha_visita, format = "%d/%m/%Y")
  ) %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)


# Reconstrucción de los embarazos a partir de cartilla:

# La FUR suele registrarse únicamente en la primera visita por lo que se propaga
# al resto de visitas del mismo embarazo
madre_cartilla <- madre_cartilla %>%
  arrange(patient_id, fecha_visita) %>%
  group_by(patient_id) %>%
  fill(fur, .direction = "down") %>%
  ungroup()

# Para cada combinación madre-FUR se obtiene la primera visita
madre_cartilla <- madre_cartilla %>%
  arrange(patient_id, fecha_visita) %>%
  group_by(patient_id, fur) %>%
  mutate(
    primera_visita = min(fecha_visita, na.rm = TRUE)
  ) %>%
  ungroup()

# Cuando no existe FUR se utiliza la primera visita
madre_cartilla <- madre_cartilla %>%
  mutate(
    fecha_referencia = coalesce(
      fur,
      primera_visita
    )
  )

# Cada combinación madre-fecha_referencia identifica un embarazo
embarazos_aux <- madre_cartilla %>%
  arrange(
    patient_id,
    fecha_referencia,
    fecha_visita
  ) %>%
  group_by(
    patient_id,
    fecha_referencia
  ) %>%
  mutate(
    id_embarazo = cur_group_id()
  ) %>%
  ungroup() %>%
  distinct(
    id_embarazo,
    patient_id,
    fecha_referencia
  ) %>%
  rename(
    id_madre = patient_id
  )

# Reconstrucción cronológica de los partos:

# Se obtiene una única fecha de parto por embarazo.
# Si existen gemelos o trillizos se conserva un único registro
info_parto <- hijo_neosoft %>%
  select(
    patient_id,
    mother_patient_id,
    tipo_parto
  ) %>%
  left_join(
    hijo_demograficos,
    by = "patient_id"
  ) %>%
  mutate(
    fecha_parto = as.Date(
      sprintf("%04d-%02d-01", ano_nac, mes_nac)
    )
  ) %>%
  rename(
    id_madre = mother_patient_id
  ) %>%
  arrange(
    id_madre,
    fecha_parto
  ) %>%
  group_by(
    id_madre,
    fecha_parto
  ) %>%
  summarise(
    tipo_parto = first(tipo_parto),
    .groups = "drop"
  ) %>%
  arrange(
    id_madre,
    fecha_parto
  ) %>%
  group_by(id_madre) %>%
  mutate(
    orden = row_number()
  ) %>%
  ungroup()

# Asociación embarazo-parto:
embarazos_aux <- embarazos_aux %>%
  left_join(
    info_parto %>%
      select(
        id_madre,
        fecha_parto,
        tipo_parto
      ), 
    by = "id_madre"
  ) %>%
  mutate( diferencia = abs(as.numeric(fecha_parto - fecha_referencia)) 
  ) %>% 
  group_by(id_embarazo) %>%
  slice_min(
    diferencia,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

# Crear la fecha de inicio del embarazo.
# Cuando no existe FUR (fecha_referencia), se aproxima restando nueve meses
# a la fecha del parto
embarazos_aux <- embarazos_aux %>%
  mutate(
    fecha_inicio = coalesce(
      fecha_referencia,
      fecha_parto %m-% months(9)
    )
  )
