# Importar los datos principales de la entidad EMBARZO
# Los datos madre_cartilla, hijo_neosoft, hijo_demograficos y los objetos
# embarazos_aux se generan en el script 03_reconstruccion_embarazos.R

# Para añadir los pesos que falten en madre_cartilla
madre_dgp <- read_delim("Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos/csv20260616/madre_dgp.csv", 
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

# Limpieza en de los otros scripts 03_reconstruccion_embarazos.R
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

# Recuperar la información de consumo de tabaco y alcohol registrada en DGP.
# Estos datos se utilizarán únicamente cuando dicha información no esté
# disponible en la cartilla materna.
habitos_dgp <- madre_dgp %>%
  filter(!is.na(result)) %>%
  mutate(
    dgp_st = str_to_upper(dgp_st)
  ) %>%
  filter(
    # Cambiar mañana si se llama de otra forma
    dgp_st %in% c("CONSUMO TABACO", "CONSUMO ALCOHOL")
  ) %>%
  rename(
    id_madre = patient_id,
    fecha_dgp = dgp_dt
  )

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
  ) %>%
  mutate(
    fur = as.Date(fur),
    fecha_parto = as.Date(fecha_parto)
  )

# Asociar cada registro de DGP al embarazo cuya FUR se encuentre más próxima
# a la fecha de registro del hábito.
habitos_dgp_embarazo <- embarazo %>%
  select(
    id_embarazo,
    id_madre,
    fur
  ) %>%
  left_join(
    habitos_dgp,
    by = "id_madre"
  ) %>%
  filter(
    !is.na(fecha_dgp),
    !is.na(fur)
  ) %>%
  mutate(
    diferencia = abs(as.numeric(fecha_dgp - fur))
  ) %>%
  group_by(
    id_embarazo,
    dgp_st
  ) %>%
  # En caso de existir varios registros, conservar el correspondiente a la
  # fecha más cercana al inicio del embarazo
  slice_min(
    diferencia,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    id_embarazo,
    dgp_st,
    result
  ) %>%
  pivot_wider(
    names_from = dgp_st,
    values_from = result
  ) %>%
  rename(
    # Cambiar mañana si se llama de otra forma
    consumo_tabaco_dgp = `CONSUMO TABACO`,
    consumo_alcohol_dgp = `CONSUMO ALCOHOL`
  ) 

embarazo <- embarazo %>%
  # Incorporar la información procedente de DGP a la tabla de embarazos
  left_join(
    habitos_dgp_embarazo,
    by = "id_embarazo"
  )

# Priorizar la información registrada en la cartilla materna.
# Cuando el consumo de tabaco o alcohol no esté informado en la cartilla,
# completar el valor utilizando el registro más próximo disponible en DGP
embarazo <- embarazo %>%
  mutate(
    consumo_tabaco = coalesce(consumo_tabaco, consumo_tabaco_dgp),
    consumo_alcohol = coalesce(consumo_alcohol, consumo_alcohol_dgp)
  ) %>%
  select(
    -consumo_tabaco_dgp,
    -consumo_alcohol_dgp
  )

# Homogeneizar la codificación de las variables de consumo para asegurar
# un formato consistente independientemente del origen de los datos
embarazo <- embarazo %>%
  mutate(
    # Verificar mañana que tanto en dgp como en cartilla codifican igual para 
    # que no haya diferentes formatos
    consumo_tabaco = case_when(
      consumo_tabaco %in% c("SI","Sí","S") ~ "Sí",
      consumo_tabaco %in% c("NO","No","N") ~ "No",
      TRUE ~ consumo_tabaco
    ),
    consumo_alcohol = case_when(
      consumo_alcohol %in% c("SI","Sí","S") ~ "Sí",
      consumo_alcohol %in% c("NO","No","N") ~ "No",
      TRUE ~ consumo_alcohol
    )
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
    peso_dgp = as.numeric(result, ",", "."),
    fecha_dgp = dgp_dt
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
      
      (peso_inicial < lim_inf_inicial|
         peso_inicial > lim_sup_inicial) &
        !is.na(peso_dgp_inicio),
      
      peso_dgp_inicio,
      peso_inicial
    ),
    
    peso_final = if_else(
      
      (peso_final < lim_inf_final|
         peso_final > lim_sup_final) &
        !is.na(peso_dgp_final),
      
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

# Los valores perdidos en el número de embarazos, abortos y nacimientos
# anteriores se consideran como ausencia de antecedentes obstétricos y,
# por tanto, se sustituyen por 0
embarazo <- embarazo %>%
  mutate(
    embarazos_anteriores = coalesce(embarazos_anteriores, 0),
    abortos_anteriores = coalesce(abortos_anteriores, 0),
    nacimientos_anteriores = coalesce(nacimientos_anteriores, 0)
  )

# Añadimos las edades que falten usando la fecha del parto y el año de nacimiento
# de la madre
## Seleccionamos el año de nacimiento y la talla de MADRE
embarazo <- embarazo %>%
  left_join(
    madre %>%
      distinct(patient_id, .keep_all = TRUE) %>%
      select(
        patient_id,
        anio_nacimiento,
        talla
      ) %>%
      rename(
        id_madre = patient_id
      ),
    by = "id_madre"
  )

embarazo <- embarazo %>%
  mutate(
    anio_parto = lubridate::year(fecha_parto)
  )

embarazo <- embarazo %>%
  mutate(
    edad = if_else(
      is.na(edad) &
        !is.na(anio_nacimiento) &
        !is.na(anio_parto),
      anio_parto - anio_nacimiento,
      edad
    )
  )

# Recalcular el IMC inicial y final cuando esté ausente o presente
# valores claramente implausibles utilizando la talla materna disponible.
embarazo <- embarazo %>%
  mutate(
    
    # Convertir la talla a metros
    talla_m = talla/100,
    
    # Recalcular IMC inicial 
    imc_inicial = if_else(
      (is.na(imc_inicial) | imc_inicial < 10 | imc_inicial > 80) &
        !is.na(peso_inicial) &
        !is.na(talla_m) &
        talla_m > 0,
      round(peso_inicial/(talla_m^2), 2),
      imc_inicial
    ),
    
    # Recalcular IMC final 
    imc_final = if_else(
      (is.na(imc_final) | imc_final < 10 | imc_final > 80) &
        !is.na(peso_final) &
        !is.na(talla_m) &
        talla_m > 0,
      round(peso_final/(talla_m^2), 2),
      imc_final
    )
  ) %>%
  select(
    -talla_m,
    -anio_parto,
    -anio_nacimiento,
    -peso_inicial_outlier,
    -peso_final_outlier,
    -peso_dgp_inicio,
    -peso_dgp_final
  )

# Formatear las fechas siguiendo el estándar YYYYMMDD
embarazo <- embarazo %>%
  mutate(
    fur = format(fur, "%Y%m%d"),
    fecha_parto = format(fecha_parto, "%Y%m%d")
  )

write_csv(embarazo,"Y:/PROYECTOS/2024 Salud perinatal (Luis-Aída-Sol)/Desarrollo/Datos_transformados/embarazo.csv")
