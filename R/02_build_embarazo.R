
source("00_config.R")

# Importar los datos principales de la entidad EMBARZO

madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/hijo_demograficos.csv", 
                                delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza
madre_cartilla <- madre_cartilla %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    fur = as.Date(fur, format="%d/%m/%Y")
  )

hijo_neosoft <- hijo_neosoft %>%
  clean_names() %>%
  distinct()

hijo_demograficos <- hijo_demograficos %>%
  clean_names() %>%
  distinct()


# Validar que la propagación de la FUR mediante fill() es consistente con el
# registro clínico de las visitas y confirmar con el tutor el tratamiento de
# embarazos sin FUR registrada.

# En algunas historias clínicas la FUR únicamente se registra en la primera visita
# del embarazo. Se propaga este valor al resto de visitas de la misma madre para
# evitar que un mismo embarazo quede dividido en varios registros.
madre_cartilla <- madre_cartilla %>%
  arrange(patient_id, fecha_visita) %>%
  group_by(patient_id) %>%
  fill(fur, .direction = "down") %>%
  ungroup()

# Ordenar
cartilla <- madre_cartilla %>%
  arrange(
    patient_id,
    fur,
    fecha_visita
  )

# Creamos el identificador del embarazo se genera agrupando las visitas por madre y FUR.
cartilla <- cartilla %>%
  group_by(
    patient_id,
    fur
  ) %>%
  mutate(
    id_embarazo = cur_group_id(),
    )

# Ahora resumir
embarazo <- cartilla %>%
  group_by(
    id_embarazo,
    patient_id
  ) %>%
  summarise(
    fur = first(fur),
    edad = first(na.omit(edad)),
    peso_inicial = first(na.omit(peso)),
    peso_final = last(na.omit(peso)),
    ganancia_peso = peso_final - peso_inicial,
    imc_inicial = first(na.omit(imc)),
    imc_final = last(na.omit(imc)),
    consumo_tabaco = first(na.omit(consumo_tabaco)),
    consumo_alcohol = first(na.omit(consumo_alcohol)),
    embarazos_anteriores = first(emb_anteriores),
    abortos_anteriores = first(abortos_anteriores),
    nacimientos_anteriores = first(nacimientos_anteriores),
    n_visitas_embarazo = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# PENDIENTE
#
# Incorporar los atributos fecha_parto, tipo_parto y numero_hijos.
#
# Actualmente no es posible relacionar directamente cada hijo con un embarazo,
# ya que los ficheros de hijos únicamente contienen el identificador de la madre.
#
# Antes de incorporar esta información es necesario definir junto con el tutor la
# estrategia para asignar cada hijo a su correspondiente id_embarazo, evitando
# duplicidades en mujeres con varios embarazos o embarazos múltiples.
# ------------------------------------------------------------------------------

# Ordenar correctamente las columnas en la entidad final
embarazo <- embarazo %>%
  select(
    id_embarazo,
    id_madre,
    fur,
    fecha_parto,
    edad,
    peso_inicial,
    peso_final,
    ganancia_peso,
    imc_inicial,
    imc_final,
    consumo_tabaco,
    consumo_alcohol,
    embarazos_anteriores,
    abortos_anteriores,
    nacimientos_anteriores,
    n_visitas_embarazo,
    tipo_parto
  )

#View(embarazo)


  
