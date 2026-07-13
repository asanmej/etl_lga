source("00_config.R")

# Importar la fuente de datos necesaria para construir la entidad SITUACION_ADMIN_MADRE
madre_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_demograficos.csv", 
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Para filtrar madres
hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Para obtener el año de cada embarazo
madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza
madre_demograficos <- madre_demograficos %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    altabdu_dt = as.Date(altabdu_dt, format = "%d-%m-%Y"),
    bajabdu_dt = as.Date(bajabdu_dt, format = "%d-%m-%Y")
  ) %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

madre_cartilla <- madre_cartilla %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    fur = as.Date(fur,"%d/%m/%Y"),
    fecha_visita = as.Date(fecha_visita,"%d/%m/%Y")
  ) %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

# Transformar las variables TSI de formato ancho (una columna por año)
# a formato largo (un registro por madre y año)
tsi <- madre_demograficos %>%
  select(patient_id, altabdu_dt, bajabdu_dt, motivo_baja, starts_with("tsi_")) %>%
  pivot_longer(
    cols = starts_with("tsi_"),
    names_to = "anio",
    names_prefix = "tsi_",
    values_to = "tsi"
  )

# Aplicar la misma transformación para la Zona Básica de Salud (ZBS)
zbs <- madre_demograficos %>%
  select(patient_id, starts_with("zbs_")) %>%
  pivot_longer(
    cols = starts_with("zbs_"),
    names_to = "anio",
    names_prefix = "zbs_",
    values_to = "zbs"
  )

# Aplicar la misma transformación para el índice de privación
ind_priv <- madre_demograficos %>%
  select(patient_id, starts_with("ind_priv_")) %>%
  pivot_longer(
    cols = starts_with("ind_priv_"),
    names_to = "anio",
    names_prefix = "ind_priv_",
    values_to = "indice_privacion"
  )

# Integrar la información administrativa anual mediante el identificador
# de la madre y el año correspondiente
situacion_admin_madre <- tsi %>%
  left_join(
    zbs,
    by = c("patient_id", "anio")) %>%
  left_join(
    ind_priv,
    by = c("patient_id", "anio")
  )

# Adaptar los nombres de los atributos a la nomenclatura definida en el modelo E/R
situacion_admin_madre <- situacion_admin_madre %>%
  rename(id_madre = patient_id) %>%
  mutate(
    anio = as.numeric(anio)
  )

# La información administrativa se propaga hacia años posteriores cuando no
# existe un registro para un determinado año, conservando el último valor
# administrativo disponible para cada madre
situacion_admin_madre <- situacion_admin_madre %>%
  arrange(id_madre, anio) %>%
  group_by(id_madre) %>%
  fill(
    tsi,
    zbs,
    indice_privacion,
    .direction = "down"
  ) %>%
  ungroup()

# En algunas historias clínicas la FUR únicamente se registra en la primera visita
# del embarazo. Se propaga este valor al resto de visitas de la misma madre para
# evitar que un mismo embarazo quede dividido en varios registros
madre_cartilla <- madre_cartilla %>%
  arrange(patient_id,fecha_visita) %>%
  group_by(patient_id) %>%
  fill(fur,.direction="down") %>%
  ungroup()

# Para cada embarazo se identifica la fecha de la primera visita registrada.
# Esta fecha se utilizará cuando la FUR no esté disponible
madre_cartilla <- madre_cartilla %>%
  group_by(patient_id,fur) %>%
  mutate(
    primera_visita=min(fecha_visita,na.rm=TRUE)
  ) %>%
  ungroup()

# Cuando la FUR no está disponible se utiliza la fecha de la primera visita
# como referencia para determinar el año del embarazo
madre_cartilla <- madre_cartilla %>%
  mutate(
    fecha_referencia = coalesce(
      fur,
      primera_visita
    )
  )

# Se construye una tabla auxiliar con el año correspondiente a cada embarazo
# para seleccionar únicamente la información administrativa relevante
embarazos_anio <- madre_cartilla %>%
  distinct(patient_id,fecha_referencia) %>%
  mutate(
    anio=year(fecha_referencia),
    # Los embarazos iniciados en 2017 utilizan la información administrativa de
    # 2018, ya que no existen variables administrativas disponibles para 2017.
    anio=if_else(anio==2017,2018,anio) 
  ) %>%
  rename(
    id_madre=patient_id
  ) %>%
  select(
    id_madre,
    anio
  )

# Se conservan únicamente los registros administrativos correspondientes al
# año de inicio de cada embarazo
situacion_admin_madre <- situacion_admin_madre %>%
  semi_join(
    embarazos_anio,
    by = c("id_madre", "anio")
  )

# Generar una clave primaria artificial para identificar de forma única
# cada registro administrativo
situacion_admin_madre <- situacion_admin_madre %>%
  mutate(
    id_admin_madre = row_number()) %>%
  relocate(id_admin_madre)

# Ordenar las variables según la estructura definida para la entidad
situacion_admin_madre <- situacion_admin_madre %>%
  select(
    id_admin_madre,
    id_madre,
    anio,
    tsi,
    zbs,
    indice_privacion,
    altabdu_dt,
    bajabdu_dt,
    motivo_baja
  )

situacion_admin_madre <- situacion_admin_madre %>%
  mutate(
    altabdu_dt = format(altabdu_dt, "%Y%m%d"),
    bajabdu_dt = format(bajabdu_dt, "%Y%m%d")
  )

write_csv(situacion_admin_madre, "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/situacion_admin_madre.csv")
