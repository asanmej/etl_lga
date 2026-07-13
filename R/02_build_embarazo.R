source("00_config.R")

# Importar los datos principales de la entidad EMBARZO
madre_cartilla <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240625/madre_cartilla.csv", 
                             delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_neosoft <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv_20240508/hijo_neosoft.csv", 
                           delim = "|", escape_double = FALSE, trim_ws = TRUE)

hijo_demograficos <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/hijo_demograficos.csv", 
                                delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Para añadir los pesos que falten en madre_cartilla
madre_dgp <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/madre_dgp.csv", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza
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
    fur = as.Date(fur, format="%d/%m/%Y"),
    fecha_visita = as.Date(fecha_visita, format="%d/%m/%Y")
  ) %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

# En algunas historias clínicas la FUR únicamente se registra en la primera visita
# del embarazo. Se propaga este valor al resto de visitas de la misma madre para
# evitar que un mismo embarazo quede dividido en varios registros.
madre_cartilla <- madre_cartilla %>%
  arrange(patient_id, fecha_visita) %>%
  group_by(patient_id) %>%
  fill(fur, .direction = "down") %>% 
  ungroup()

# Para cada combinación madre-FUR se identifica la fecha de la primera visita,
# que servirá como referencia cuando la FUR no esté disponible
madre_cartilla <- madre_cartilla %>%
  arrange(patient_id, fecha_visita) %>%
  group_by(patient_id, fur) %>%
  mutate(
    primera_visita = min(fecha_visita, na.rm = TRUE)
  ) %>%
  ungroup()

# Cuando la FUR no está disponible se utiliza la fecha de la primera visita
# como referencia temporal para identificar cronológicamente el embarazo
madre_cartilla <- madre_cartilla %>%
  mutate(
    fecha_referencia = coalesce(
      fur,
      primera_visita
    )
  )

# Cada embarazo queda definido por la combinación de la madre y una fecha de
# referencia (FUR o, cuando esta no existe, la fecha de la primera visita).
# A partir de esta combinación se genera el identificador del embarazo
visita_embarazo <- madre_cartilla  %>%
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
    id_embarazo = cur_group_id(),
  ) %>%
  ungroup()

# Funciones auxiliares
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

# Se resume toda la información de las visitas en un único registro por embarazo.
# Para cada atributo se conserva el primer o el último valor registrado según
# su significado clínico
embarazo <- visita_embarazo %>%
  group_by(
    id_embarazo,
    patient_id
  ) %>%
  summarise(
    fur = first(fur),
    edad = primer_no_na(edad),
    peso_inicial = primer_no_na(peso),
    peso_final = ultimo_no_na(peso),
    ganancia_peso = peso_final - peso_inicial,
    imc_inicial = primer_no_na(imc),
    imc_final = ultimo_no_na(imc),
    consumo_tabaco = primer_no_na(consumo_tabaco),
    consumo_alcohol = primer_no_na(consumo_alcohol),
    embarazos_anteriores = primer_no_na(emb_anteriores),
    abortos_anteriores = primer_no_na(abortos_anteriores),
    nacimientos_anteriores = primer_no_na(nacimientos_anteriores),
    n_visitas_embarazo = n(),
    primera_visita = first(primera_visita),
    .groups = "drop"
  )

# Los embarazos de una misma madre se ordenan cronológicamente para asociarlos
# posteriormente con los partos registrados en la información neonatal
embarazo <- embarazo %>%
  mutate(
    fecha_referencia = coalesce(
      fur,
      primera_visita
    )
  )

# Crear orden embarazo
embarazo <- embarazo %>%
  arrange(
    patient_id,
    fecha_referencia
  ) %>%
  group_by(patient_id) %>%
  mutate(
    orden = row_number()
  ) %>%
  ungroup()

# Se construye una tabla auxiliar con los partos de cada madre, ordenados
# cronológicamente, para asociar cada embarazo con su parto correspondiente
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
  group_by(id_madre) %>%
  mutate(
    orden = row_number()
  ) %>%
  ungroup()

# La asociación embarazo-parto se realiza utilizando el orden cronológico de
# ambos eventos dentro de cada madre
embarazo <- embarazo %>%
  rename(
    id_madre = patient_id
  ) %>%
  left_join(
    info_parto %>%
      select(
        id_madre,
        orden,
        fecha_parto,
        tipo_parto
      ),
    by = c("id_madre", "orden")
  )

# Detectar posibles outliers mediante el rango intercuartílico (IQR)
q1_inicial <- quantile(embarazo$peso_inicial, 0.25, na.rm = TRUE)

q3_inicial <- quantile(embarazo$peso_inicial, 0.75, na.rm = TRUE)

iqr_inicial <- IQR(embarazo$peso_inicial, na.rm = TRUE)

lim_inf_inicial <- q1_inicial - 1.5 * iqr_inicial
lim_sup_inicial <- q3_inicial + 1.5 * iqr_inicial


q1_final <- quantile(embarazo$peso_final,0.25, na.rm = TRUE)

q3_final <- quantile(embarazo$peso_final, 0.75, na.rm = TRUE)

iqr_final <- IQR(embarazo$peso_final, na.rm = TRUE)

lim_inf_final <- q1_final - 1.5 * iqr_final
lim_sup_final <- q3_final + 1.5 * iqr_final

embarazo <- embarazo %>%
  mutate(
    peso_inicial_outlier =
      peso_inicial < lim_inf_inicial |
      peso_inicial > lim_sup_inicial,
    
    peso_final_outlier = 
      peso_final < lim_inf_final |
      peso_final > lim_sup_final
  )

# Cuando los pesos registrados en la cartilla se consideran atípicos mediante
# el criterio IQR, se sustituyen por el peso más próximo registrado en DGP
peso_dgp <- madre_dgp %>%
  filter(str_detect(str_to_upper(dgp_st), "PESO")) %>%
  mutate(
    peso_dgp = as.numeric(result),
    fecha_dgp = as.Date(dgp_dt, format = "%d-%m-%Y")
  ) %>%
  select(
    patient_id,
    fecha_dgp,
    peso_dgp
  )

q1_dgp <- quantile(peso_dgp$peso_dgp,0.25, na.rm = TRUE)

q3_dgp <- quantile(peso_dgp$peso_dgp, 0.75, na.rm = TRUE)

iqr_dgp <- IQR(peso_dgp$peso_dgp, na.rm = TRUE)

lim_inf_dgp <- q1_dgp - 1.5 * iqr_dgp
lim_sup_dgp <- q3_dgp + 1.5 * iqr_dgp

peso_dgp <- peso_dgp %>%
  filter(
    peso_dgp >= lim_inf_dgp &
      peso_dgp <= lim_sup_dgp
  ) %>%
  rename(
    id_madre = patient_id
  )

embarazo <- embarazo %>%
  mutate(
    fur = as.Date(fur),
    fecha_parto = as.Date(fecha_parto)
  )

# Se recupera el peso DGP registrado más próximo al inicio del embarazo
peso_inicio <- embarazo %>%
  select(
    id_embarazo,
    id_madre,
    fur
  ) %>% 
  left_join(
    peso_dgp,
    by = "id_madre"
  ) %>%
  mutate(
    diferencia = abs(as.numeric(fecha_dgp - fur))
  ) %>%
  group_by(id_embarazo) %>%
  slice_min(
    diferencia,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    id_embarazo,
    peso_dgp_inicio = peso_dgp
  )

# Se recupera el peso DGP registrado más próximo al momento del parto
peso_final <- embarazo %>%
  select(
    id_embarazo,
    id_madre,
    fecha_parto
  ) %>% 
  left_join(
    peso_dgp,
    by = "id_madre"
  ) %>%
  mutate(
    diferencia = abs(as.numeric(fecha_dgp - fecha_parto))
  ) %>%
  group_by(id_embarazo) %>%
  slice_min(
    diferencia,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    id_embarazo,
    peso_dgp_final = peso_dgp
  )

embarazo <- embarazo %>%
  left_join(
    peso_inicio,
    by = "id_embarazo"
  ) %>%
  left_join(
    peso_final,
    by = "id_embarazo"
  )

# Los pesos considerados atípicos se reemplazan por el valor procedente de DGP
embarazo <- embarazo %>%
  mutate(
    peso_inicial = if_else(
      
      peso_inicial < lim_inf_inicial|
        peso_inicial > lim_sup_inicial,
      
      peso_dgp_inicio,
      peso_inicial
    ),
    
    peso_final = if_else(
      
      peso_final < lim_inf_final|
        peso_final > lim_sup_final,
      
      peso_dgp_final,
      peso_final
    )
  )

# Tras la posible sustitución de pesos se recalcula la ganancia de peso
embarazo <- embarazo %>%
  mutate(
    ganancia_peso = peso_final - peso_inicial
  )

# Formatear las fechas siguiendo el estándar YYYYMMDD
embarazo <- embarazo %>%
  mutate(
    fur = format(fur, "%Y%m%d"),
    fecha_parto = format(fecha_parto, "%Y%m%d")
  )

embarazo <- embarazo %>%
  select(
    -fecha_referencia,
    -primera_visita,
    -orden,
    -peso_dgp_inicio,
    -peso_dgp_final,
    -peso_inicial_outlier,
    -peso_final_outlier
  )

write_csv(embarazo,"Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/embarazo.csv")