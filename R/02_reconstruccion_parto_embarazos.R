# -----------------------------------------------------------------------------
# RECONSTRUCCIÓN DE EMBARAZOS
#
# Objetivo:
# Reconstruir los embarazos a partir de las visitas registradas en la cartilla
# obstétrica y asociarlos con los partos registrados en NeoSoft.
#
# Este script genera los objetos auxiliares:
#   - embarazos_aux
#   - partos_aux
#
# Estos objetos serán utilizados posteriormente para construir las entidades
# EMBARAZO e HIJO.
#
# La reconstrucción se realiza:
#   1. Agrupando visitas prenatales pertenecientes al mismo embarazo
#   2. Estimando una fecha de inicio del embarazo
#   3. Emparejando cada embarazo reconstruido con el parto
#      cronológicamente más compatible
#   4. Conservando únicamente los embarazos finalizados en un nacimiento
#
# Parámetros utilizados para la reconstrucción:
#
# DIAS_MIN_EMBARAZO: duración mínima considerada biológicamente compatible.
# DIAS_MAX_EMBARAZO: duración máxima considerada biológicamente compatible.
# DIAS_ESTANDAR: duración utilizada para seleccionar el parto más compatible.
# -----------------------------------------------------------------------------

# 0. Funciones auxiliares para recuperar el primer o el último valor
#    no perdido dentro de cada embarazo
primer_no_na <- function(x){
  
  if(all(is.na(x))){
    NA
    
  }else{
    first(na.omit(x))
    
  }
}

ultimo_no_na <- function(x){
  
  if(all(is.na(x))){
    NA
    
  }else{
    last(na.omit(x))
    
  }
}

# 1. Cargar datos
madre_cartilla <- read_delim(
  "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv",
  delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_neosoft <- read_delim(
  "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv",
  delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_demograficos <- read_delim(
  "Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/hijo_demograficos.csv",
  delim = "|", escape_double = FALSE, trim_ws = TRUE)

# 2. Limpieza y filtrado inicial
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

hijo_demograficos <- hijo_demograficos %>%
  clean_names() %>%
  distinct() %>%
  filter(
    !is.na(ano_nac),
    !is.na(mes_nac)
  ) %>%
  # Conservar únicamente los recién nacidos cuyas madres forman parte
  # de la cohorte definida a partir de la cartilla obstétrica y NeoSoft
  filter(mother_patient_id %in% madre_cartilla$patient_id)

hijo_neosoft <- hijo_neosoft %>%
  clean_names() %>%
  distinct() %>%
  # Eliminamos los recién nacidos que no tienen registo de fecha de nacimiento y
  # están en hijo_neosoft.csv pero no en hijo_demograficos.csv
  semi_join(hijo_demograficos, by = "patient_id") %>%
  filter(mother_patient_id %in% madre_cartilla$patient_id)


# 3. Procesar la edad gestacional y estimar la fecha de inicio del embarazo

# Se estandarizan los distintos formatos utilizados para registrar
# la edad gestacional y se transforma a días para estimar una fecha
# de inicio del embarazo
madre_cartilla <- madre_cartilla %>%
  mutate(
    # Normalizar texto
    edad_gestacional = str_to_lower(edad_gestacional),
    
    edad_gestacional = str_replace_all(edad_gestacional, ",", "."),
    
    edad_gestacional = str_replace_all(edad_gestacional, "día|días|dPP", "dias"),
    
    edad_gestacional = str_replace_all(edad_gestacional, "semana|semanas|s.g", "sem"),
    
    edad_gestacional = str_squish(edad_gestacional),
    
    # Eliminar registros sin ningún número
    edad_gestacional = if_else(
      str_detect(edad_gestacional, "\\d"),
      edad_gestacional,
      NA_character_),
    
    # Extraer TODOS los números
    numeros = str_extract_all(
      edad_gestacional,
      "\\d+"),
    
    # Primer número = semanas
    semanas = purrr::map_dbl(
      numeros,
      ~ if(length(.x) >= 1) as.numeric(.x[1]) else NA_real_),
    
    # Segundo número = días
    dias = purrr::map_dbl(
      numeros,
      ~ if(length(.x) >= 2) as.numeric(.x[2]) else 0),
    
    # Limpiar días imposibles
    dias = if_else(dias > 6, 0, dias),
    
    # Edad gestacional en días
    edad_gestacional_dias = semanas * 7 + dias,
    
    # Fecha estimada de inicio del embarazo
    fecha_inicio_estimada =
      fecha_visita - lubridate::days(edad_gestacional_dias)
  )

# 4. Reconstruir embarazos agrupando las visitas prenatales
madre_cartilla <- madre_cartilla %>%
  # Ordenar las visitas de cada madre según la fecha de inicio del embarazo
  arrange(
    patient_id,
    fecha_inicio_estimada
  ) %>%
  group_by(patient_id) %>%
  mutate(
    diferencia_inicio = as.numeric(
      fecha_inicio_estimada - lag(fecha_inicio_estimada)
    ),
    # Se considera que dos visitas pertenecen a embarazos distintos cuando
    # la diferencia entre las fechas estimadas de inicio supera 28 días.
    # Este margen evita dividir un mismo embarazo debido a pequeñas
    # imprecisiones en la estimación de la edad gestacional
    nuevo_embarazo = row_number() == 1 | diferencia_inicio > 28,
    
    # Identificador temporal utilizado únicamente durante la reconstrucción
    # de los embarazos a partir de las visitas prenatales
    id_embarazo = cumsum(nuevo_embarazo)
  ) %>%
  ungroup() 

# 5. Crear una tabla con un registro por embarazo

# Cada embarazo reconstruido pasa a representarse mediante un único registro
embarazos_aux <- madre_cartilla %>%
  group_by(
    patient_id,
    id_embarazo
  ) %>%
  summarise(
    fecha_inicio_embarazo =
      as.Date(
        # Se utiliza la mediana para reducir el efecto de estimaciones
        # extremas derivadas de errores en la edad gestacional registrada
        median(as.numeric(fecha_inicio_estimada), na.rm = TRUE),
        origin = "1970-01-01"
      ),
    primera_visita_fecha = min(fecha_visita, na.rm = TRUE),
    ultima_visita_fecha = max(fecha_visita, na.rm = TRUE),
    n_visitas_embarazo = n(),
    fur = primer_no_na(fur),
    .groups = "drop"
  )

# 6. Ordenar embarazos

# Se asigna un orden cronológico a los embarazos reconstruidos
# de cada madre
embarazos_aux <- embarazos_aux %>%
  rename(
    id_madre = patient_id
  ) %>%
  arrange(
    id_madre,
    fecha_inicio_embarazo
  ) %>%
  group_by(id_madre) %>%
  mutate(
    orden_embarazo = row_number()
  ) %>%
  ungroup() 

# Incorporar a cada visita prenatal el orden cronológico del embarazo
# al que ha sido asignada. Esta información se utilizará posteriormente
# para propagar el identificador del embarazo a las observaciones de
# madre_cartilla cuando sea necesario
madre_cartilla <- madre_cartilla %>%
  left_join(
    embarazos_aux %>%
      select(
        id_madre,
        id_embarazo,
        orden_embarazo
      ),
    by = c(
      "patient_id" = "id_madre",
      "id_embarazo"
    )
  )

# El identificador auxiliar utilizado durante la reconstrucción de los
# embarazos deja de ser necesario una vez asignado el orden cronológico.
# A partir de este punto los embarazos se identificarán mediante
# orden_embarazo y posteriormente recibirán su identificador definitivo
# durante la construcción de la entidad EMBARAZO
embarazos_aux <- embarazos_aux %>%
  select(-id_embarazo)

# 7. Reconstruir partos

# A partir de la información neonatal se construye una tabla con un
# registro por nacimiento, que posteriormente permitirá emparejar los
# embarazos reconstruidos con sus partos correspondientes
partos_aux <- hijo_neosoft %>%
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
    # La fecha exacta de nacimiento no está disponible. Se asigna el día 
    # 15 del mes únicamente para preservar el orden cronológico de los partos.
    fecha_parto = make_date(
      ano_nac,
      mes_nac,
      15 
    )
  ) %>%
  rename(
    id_hijo = patient_id,
    id_madre = mother_patient_id
  ) %>%
  filter(
    year(fecha_parto) >= 2018,
    year(fecha_parto) <= 2023
  )

# 8. Emparejar embarazos con partos permitiendo embarazos múltiples
# (gemelos, trillizos, etc.). Cada fecha de parto representa un único embarazo,
# aunque pueda tener varios hijos.

# Intervalo considerado biológicamente compatible entre la fecha de
# inicio del embarazo y el parto
DIAS_MIN_EMBARAZO <- 160
DIAS_MAX_EMBARAZO <- 310
DIAS_ESTANDAR <- 280

# El emparejamiento se realiza madre a madre para impedir asignaciones
# entre embarazos pertenecientes a mujeres distintas
lista_madres <- unique(embarazos_aux$id_madre)
asignaciones <- vector("list", length(lista_madres))

# El emparejamiento se realiza de forma independiente para cada madre.
# Cada fecha de parto se asocia a un único embarazo reconstruido. En los
# embarazos múltiples, todos los recién nacidos nacidos en esa fecha quedan
# vinculados al mismo embarazo
for(i in seq_along(lista_madres)){
  
  id_madre_actual <- lista_madres[i]
  
  emb <- embarazos_aux %>%
    filter(id_madre == id_madre_actual) %>%
    arrange(coalesce(fur, fecha_inicio_embarazo))
  
  partos <- partos_aux %>%
    filter(id_madre == id_madre_actual) %>%
    arrange(fecha_parto)
  
  if(nrow(partos)==0){
    
    # Si la madre no tiene ningún parto registrado dentro del periodo de estudio,
    # los embarazos reconstruidos permanecen sin asignación y se procesará la
    # siguiente madre
    asignaciones[[i]] <- emb
    next
  }
  
  # Cada fecha de parto representa un único embarazo.
  # Si existen varios recién nacidos en la misma fecha (embarazo múltiple),
  # todos compartirán posteriormente el mismo embarazo reconstruido
  partos_unicos <- partos %>%
    distinct(fecha_parto) %>%
    arrange(fecha_parto)
  
  partos_unicos$asignado <- FALSE
  
  emb$fecha_parto <- as.Date(NA)
  
  for(j in seq_len(nrow(emb))){
    # Se utiliza como referencia la FUR registrada cuando está disponible.
    # En su ausencia se emplea la fecha estimada de inicio del embarazo
    fecha_ref <- coalesce(
      emb$fur[j],
      emb$fecha_inicio_embarazo[j]
    )
    
    if(is.na(fecha_ref))
      next
    
    # Se identifican únicamente los partos cuya distancia temporal respecto al
    # inicio del embarazo sea compatible con una duración gestacional biológicamente
    # plausible
    candidatos <- which(
      !partos_unicos$asignado &
        between(
          as.numeric(partos_unicos$fecha_parto-fecha_ref),
          DIAS_MIN_EMBARAZO,
          DIAS_MAX_EMBARAZO
        )
    )
    
    if(length(candidatos)==0)
      next
    
    # Si existen varios partos compatibles, se selecciona aquel cuya duración
    # gestacional esté más próxima a los 280 días, considerados la duración
    # estándar de un embarazo a término
    mejor <- candidatos[
      which.min(
        abs(
          as.numeric(
            partos_unicos$fecha_parto[candidatos]-fecha_ref
          )-DIAS_ESTANDAR
        )
      )
    ]
    
    emb$fecha_parto[j] <-
      partos_unicos$fecha_parto[mejor]
    
    # El parto queda marcado como utilizado para impedir que pueda
    # asignarse a otro embarazo de la misma madre
    partos_unicos$asignado[mejor] <- TRUE
    
  }
  
  # Una vez asignada la fecha de parto al embarazo reconstruido, se reincorporan
  # todos los recién nacidos nacidos en esa fecha. De este modo, los embarazos
  # múltiples (gemelos, trillizos, etc.) quedan representados mediante un único
  # embarazo asociado a varios hijos
  emb <- emb %>%
    left_join(
      partos %>%
        select(
          fecha_parto,
          id_hijo,
          tipo_parto
        ),
      by="fecha_parto"
    )
  asignaciones[[i]] <- emb
}

# Unificar los resultados obtenidos para todas las madres
embarazos_aux <- bind_rows(asignaciones)

# Homogeneizar el identificador materno
madre_cartilla <- madre_cartilla %>%
  rename(id_madre = patient_id)

# Se conservan únicamente los embarazos que pudieron asociarse
# a al menos un nacimiento registrado en NeoSoft.
embarazos_aux <- embarazos_aux %>%
  filter(!is.na(id_hijo))

# Conservar únicamente los embarazos cuyo parto ocurrió dentro
# del periodo de estudio (2018–2023)
embarazos_aux <- embarazos_aux %>%
  filter(
    between(
      year(fecha_parto),
      2018,
      2023
    )
  )

# 9. Determinar la FUR definitiva de cada embarazo:

# Se prioriza la FUR registrada en la cartilla materna siempre que sea
# compatible con la fecha del parto (180-310 días de gestación). Cuando la
# FUR no existe o resulta incompatible:
#   1. Se utiliza la fecha de inicio estimada del embarazo obtenida a partir
#      de la edad gestacional registrada en las visitas prenatales, siempre
#      que sea compatible con la fecha del parto.
#   2. En caso contrario, se estima la FUR restando 280 días a la fecha
#      del parto.
#
# Esta estrategia prioriza la información clínica disponible y evita asignar
# fechas pertenecientes a embarazos distintos, ya que la reconstrucción se
# realiza de forma independiente para cada embarazo previamente identificado.

embarazos_aux <- embarazos_aux %>%
  mutate(
    duracion_fur = as.numeric(fecha_parto - fur),
    duracion_inicio = as.numeric(fecha_parto - fecha_inicio_embarazo),
    fur = case_when(
      
      # 1. Utilizar la FUR registrada si es compatible con el parto
      !is.na(fur) &
        between(duracion_fur, DIAS_MIN_EMBARAZO, DIAS_MAX_EMBARAZO) ~ fur,
      
      # 2. Si no existe una FUR válida, utilizar la fecha de inicio estimada
      !is.na(fecha_inicio_embarazo) &
        between(duracion_inicio, DIAS_MIN_EMBARAZO, DIAS_MAX_EMBARAZO) 
      ~ fecha_inicio_embarazo,
      
      # 3. Si tampoco existe una estimación compatible, estimar la FUR a partir 
      #    de la fecha del parto
      !is.na(fecha_parto) ~
        fecha_parto - lubridate::days(DIAS_ESTANDAR),
      
      TRUE ~ as.Date(NA)
    )
  ) %>%
  select(
    -duracion_fur,
    -duracion_inicio
  )
