
source("00_config.R")

# Importar la fuente de datos necesaria para construir la entidad SITUACION_ADMIN_MADRE
madre_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/madre_demograficos.csv", 
                                 delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Transformar las variables TSI de formato ancho (una columna por año)
# a formato largo (un registro por madre y año)
tsi <- madre_demograficos %>%
  select(patient_id, altabdu_dt, bajabdu_dt, motivo_baja, starts_with("tsi_")) %>%
  pivot_longer(
    cols = starts_with("tsi_"),
    names_to = "año",
    names_prefix = "tsi_",
    values_to = "tsi"
  )

# Aplicar la misma transformación para la Zona Básica de Salud (ZBS)
zbs <- madre_demograficos %>%
  select(patient_id, starts_with("zbs_")) %>%
  pivot_longer(
    cols = starts_with("zbs_"),
    names_to = "año",
    names_prefix = "zbs_",
    values_to = "zbs"
  )

# Transformar el índice de privación a formato largo
ind_priv <- madre_demograficos %>%
  select(patient_id, starts_with("ind_priv_")) %>%
  pivot_longer(
    cols = starts_with("ind_priv_"),
    names_to = "año",
    names_prefix = "ind_priv_",
    values_to = "indice_privacion"
  )

# Integrar la información administrativa anual mediante el identificador
# de la madre y el año correspondiente
situacion_admin_madre <- tsi %>%
  left_join(zbs,
            by = c("patient_id", "año")) %>%
  left_join(ind_priv,
            by = c("patient_id", "año"))


# Generar una clave primaria artificial para identificar de forma única
# cada registro administrativo
situacion_admin_madre <- situacion_admin_madre %>%
  mutate(
    id_admin_madre = row_number()) %>%
  relocate(id_admin_madre)

# Adaptar el identificador a la nomenclatura definida en el modelo E/R
situacion_admin_madre <- situacion_admin_madre %>% 
  rename(id_madre = patient_id)

# Ordenar las variables según la estructura definida para la entidad
situacion_admin_madre <- situacion_admin_madre %>%
  select(
    id_admin_madre,
    id_madre,
    año,
    tsi,
    zbs,
    indice_privacion,
    altabdu_dt,
    bajabdu_dt,
    motivo_baja
  )

# NOTA:
# Esta entidad almacena toda la información administrativa anual de cada madre.
# En la construcción de la entidad EMBARAZO se asociará a cada embarazo el
# registro correspondiente al año en el que se inició (según la FUR). Por
# ejemplo, si un embarazo comienza el 25-09-2018, se utilizarán los valores de
# TSI, ZBS e índice de privación registrados para 2018, y no los de años
# posteriores.

#View(situacion_admin_madre)
