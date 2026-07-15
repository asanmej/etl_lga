# Importar los datos principales de la entidad EMBARZO
# Los datos madre_cartilla, hijo_neosoft, hijo_demograficos y los objetos
# embarazos_aux se generan en el script 03_reconstruccion_embarazos.R

# Para añadir los pesos que falten en madre_cartilla
madre_dgp <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/madre_dgp.csv", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza en el script 03_reconstruccion_embarazos.R
madre_dgp <- madre_dgp %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    dgp_dt = as.Date(dgp_dt, format = "%d/%m/%Y")
  ) %>%
  filter(patient_id %in% hijo_neosoft$mother_patient_id)

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
embarazo <- madre_cartilla %>%
  left_join(
    embarazos_aux %>%
      select(
        id_embarazo,
        id_madre,
        fecha_referencia
      ),
    by = c(
      "patient_id" = "id_madre",
      "fecha_referencia"
    )
  ) %>%
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
    primera_visita = first(primera_visita),
    .groups = "drop"
  )

embarazo <- embarazo %>%
  rename(
    id_madre = patient_id
  ) %>%
  left_join(
    embarazos_aux %>%
      select(
        id_embarazo,
        fecha_parto,
        tipo_parto
      ),
    by = "id_embarazo"
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

# Filtramos solo las madres que sean válidas y por lo tanto los embarazos válidos
embarazo <- embarazo %>%
  semi_join(madres_validas, by = "id_madre")

# Formatear las fechas siguiendo el estándar YYYYMMDD
embarazo <- embarazo %>%
  mutate(
    fur = format(fur, "%Y%m%d"),
    fecha_parto = format(fecha_parto, "%Y%m%d")
  )

embarazo <- embarazo %>%
  select(
    -primera_visita,
    -peso_dgp_inicio,
    -peso_dgp_final,
    -peso_inicial_outlier,
    -peso_final_outlier
  )

write_csv(embarazo,"Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/embarazo.csv")
