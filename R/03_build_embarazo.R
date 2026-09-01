# -----------------------------------------------------------------------------
# ENTIDAD EMBARAZO
#
# Objetivo:
# Construir la entidad EMBARAZO a partir de los embarazos reconstruidos y de la
# información clínica registrada en la cartilla obstétrica y en DGP
#
# Se integra la información procedente de la cartilla materna y de DGP,
# priorizando los registros de la cartilla cuando existen y utilizando DGP para
# completar o corregir información ausente o implausible.
#
# Parámetros utilizados:
#
# EDAD_MIN: edad materna mínima considerada biológicamente plausible
# EDAD_MAX: edad materna máxima considerada biológicamente plausible
#
# VENTANA_INICIO: días alrededor de la FUR utilizados para buscar el peso
#                 inicial más próximo registrado en DGP
#
# VENTANA_FINAL: días alrededor de la fecha de parto utilizados para buscar el
#                peso final más próximo registrado en DGP
#
# PERDIDA_MAX_PESO: pérdida máxima de peso considerada plausible durante el
#                   embarazo
#
# GANANCIA_MAX_PESO: ganancia máxima de peso considerada plausible durante el
#                    embarazo
#
# TOLERANCIA_TALLA: diferencia máxima (cm = kg) utilizada para detectar
#                   registros de peso que probablemente corresponden a la talla
#                   por un error de introducción de datos
# -----------------------------------------------------------------------------

# 1. Importar los datos principales
madre_cartilla <- read_delim(
  file.path(PATH_DATOS_INTERMEDIOS, "madre_cartilla_procesada.csv"),
  delim = "|",
  escape_double = FALSE,
  trim_ws = TRUE
)

embarazos_aux <- read_delim(
  file.path(PATH_DATOS_INTERMEDIOS, "embarazos_aux.csv"),
  delim = "|",
  escape_double = FALSE,
  trim_ws = TRUE
)

madre <- read_delim(file.path(PATH_TRANSFORMADOS, "madre.csv"))

# Información clínica complementaria procedente de DGP
madre_dgp <- read_delim(FILE_MADRE_DGP,
                        delim = "|", escape_double = FALSE, trim_ws = TRUE)

# 2. Limpieza y filtrado inicial
madre_cartilla <- madre_cartilla %>%
  mutate(
    fecha_visita = as.Date(fecha_visita),
    fur = as.Date(fur),
    fecha_inicio_estimada = as.Date(fecha_inicio_estimada),
    fecha_inicio_roll_forward = as.Date(fecha_inicio_roll_forward)
  )

embarazos_aux <- embarazos_aux %>%
  mutate(
    fecha_inicio_embarazo = as.Date(fecha_inicio_embarazo),
    primera_visita_fecha = as.Date(primera_visita_fecha),
    ultima_visita_fecha = as.Date(ultima_visita_fecha),
    fecha_parto = as.Date(fecha_parto),
    fur = as.Date(fur)
  )

madre_dgp <- madre_dgp %>%
  clean_names() %>%
  distinct() %>%
  mutate(
    dgp_dt = as.Date(dgp_dt, format = "%d/%m/%Y")
  ) %>%
  filter(patient_id %in% embarazos_aux$id_madre)

# 3. Recuperar la información de consumo de tabaco y alcohol registrada en DGP.
# Estos datos se utilizarán únicamente cuando dicha información no esté
# disponible en la cartilla materna.
habitos_dgp <- madre_dgp %>%
  # Conservar únicamente los registros con información disponible
  filter(!is.na(result)) %>%
  mutate(
    dgp_st = str_to_upper(dgp_st)
  ) %>%
  filter(
    dgp_st %in% c("TABACO (SI/NO)", "ALCOHOL (SI/NO)")
  ) %>%
  rename(
    id_madre = patient_id,
    fecha_dgp = dgp_dt
  )

# 4. Se resume toda la información de las visitas en un único registro por embarazo.
# Para cada atributo se conserva el primer o el último valor registrado según
# su significado clínico
embarazo <- madre_cartilla %>%
  group_by(
    id_madre,
    orden_embarazo
  ) %>%
  summarise(
    edad = primer_no_na(edad),
    peso_inicial = primer_no_na(peso),
    peso_final = ultimo_no_na(peso),
    imc_inicial = primer_no_na(imc),
    imc_final = ultimo_no_na(imc),
    consumo_tabaco = primer_no_na(consumo_tabaco),
    consumo_alcohol = primer_no_na(consumo_alcohol),
    embarazos_anteriores = primer_no_na(emb_anteriores),
    abortos_anteriores = primer_no_na(abortos_anteriores),
    nacimientos_anteriores = primer_no_na(nacimientos_anteriores),
    .groups = "drop"
  ) %>%
  distinct() %>%
  left_join(
    embarazos_aux %>%
      select(
        id_madre,
        orden_embarazo,
        fecha_parto,
        tipo_parto,
        fur,
        primera_visita_fecha
      ),
    by = c(
      "id_madre",
      "orden_embarazo"
    )
  )

# 5. Asignar un identificador único a cada embarazo

# Se genera un identificador consecutivo para cada combinación única de
# madre y orden de embarazo. Para ello se utiliza interaction() para crear
# una combinación única de ambas variables y dense_rank() para asignar un
# identificador entero consecutivo sin dejar huecos
embarazo <- embarazo %>%
  distinct() %>%
  arrange(
    id_madre,
    orden_embarazo
  ) %>%
  mutate(
    id_embarazo = 
      dense_rank(
        interaction(
          id_madre,
          orden_embarazo,
          drop = TRUE
        )
      )
  )

# El identificador se añade también a embarazos_aux para mantener la
# correspondencia entre ambas tablas y disponer de una clave única de
# embarazo en los procesos posteriores
embarazos_aux <- embarazos_aux %>%
  left_join(
    embarazo %>%
      distinct(id_madre, orden_embarazo, id_embarazo) %>%
      select(
        id_madre,
        orden_embarazo,
        id_embarazo
      ),
    by=c(
      "id_madre",
      "orden_embarazo"
    )
  )

write_delim(
  embarazos_aux,
  file.path(PATH_DATOS_INTERMEDIOS, "embarazos_aux.csv"),
  delim = "|",
  na = ""
)

# Eliminamos los embarazos que no tienen fecha de parto
embarazo <- embarazo %>%
  filter(
    !is.na(fecha_parto)
  )

# 6. Para cada embarazo, seleccionar el registro de tabaco o alcohol del DGP
#    cuya fecha se encuentre más próxima a la FUR
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
    !is.na(fur),
    !is.na(fecha_dgp),
    !is.na(result)
  ) %>%
  mutate(
    diferencia = abs(as.numeric(fecha_dgp - fur))
  ) %>%
  group_by(
    id_embarazo,
    dgp_st
  ) %>%
  # En caso de existir varios registros, conservar el más próximo
  # al inicio del embarazo
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
    consumo_tabaco_dgp = `TABACO (SI/NO)`, 
    consumo_alcohol_dgp = `ALCOHOL (SI/NO)`
  ) 

# 7. Incorporar la información procedente de DGP a la tabla de embarazos
embarazo <- embarazo %>%
  left_join(
    habitos_dgp_embarazo,
    by = "id_embarazo"
  )

# 8. Homogeneizar la codificación de las variables de consumo para asegurar
# un formato consistente independientemente del origen de los datos
embarazo <- embarazo %>%
  mutate(
    consumo_tabaco_dgp = case_when(
      consumo_tabaco_dgp == "S" ~ 1,
      consumo_tabaco_dgp == "N" ~ 0,
      TRUE ~ NA_real_
    ),
    consumo_alcohol_dgp = case_when(
      consumo_alcohol_dgp == "S" ~ 1,
      consumo_alcohol_dgp == "N" ~ 0,
      TRUE ~ NA_real_
    )
  )

# 9. Completar la información de consumo de tabaco y alcohol:

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

# 10. Completar la edad materna cuando esté ausente o resulte implausible 
#     utilizando el año de nacimiento y la fecha del parto

## Seleccionamos el año de nacimiento y la talla de MADRE
embarazo <- embarazo %>%
  left_join(
    madre %>%
      distinct(id_madre, .keep_all = TRUE) %>%
      select(
        id_madre,
        anio_nacimiento,
        talla
      ),
    by = "id_madre"
  )

EDAD_MIN <- 12
EDAD_MAX <- 51

embarazo <- embarazo %>%
  mutate(
    edad = if_else(
      is.na(edad) |
        edad < EDAD_MIN |
        edad > EDAD_MAX,
      
      year(fecha_parto) - anio_nacimiento,
      edad
    ),
    edad = if_else(
      edad < EDAD_MIN |
        edad > EDAD_MAX,
      
      NA_real_,
      edad
    )
  )

# 11. Detectar valores atípicos en el peso  mediante el rango intercuartílico (IQR)

## Primero aplicamos un filtro de plausibilidad biológica
embarazo <- embarazo %>%
  mutate(
    
    peso_inicial = if_else(
      peso_inicial < 30 |
        peso_inicial > 200,
      NA_real_,
      peso_inicial
    ),
    
    peso_final = if_else(
      peso_final < 35 |
        peso_final > 220,
      NA_real_,
      peso_final
    )
    
  )

## Función para calcular los límites inferior y superior mediante el criterio IQR
limites_iqr <- function(x){
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  
  iqr <- IQR(x, na.rm = TRUE)
  
  c(
    inferior = q1 - 1.5 * iqr,
    superior = q3 + 1.5 * iqr
  )
}

## Peso inicial
limites_inicial <- limites_iqr(embarazo$peso_inicial)

lim_inf_inicial <- limites_inicial["inferior"]
lim_sup_inicial <- limites_inicial["superior"]

## Peso final
limites_final <- limites_iqr(embarazo$peso_final)

lim_inf_final <- limites_final["inferior"]
lim_sup_final <- limites_final["superior"]

embarazo <- embarazo %>%
  mutate(
    peso_inicial_outlier =
      !is.na(peso_inicial) &
      (
        peso_inicial < lim_inf_inicial |
          peso_inicial > lim_sup_inicial
      ),
    
    peso_final_outlier = 
      !is.na(peso_final) &
      (
        peso_final < lim_inf_final |
          peso_final > lim_sup_final 
      )
  )

## Cuando los pesos registrados en la cartilla se consideran atípicos mediante
## el criterio IQR, se sustituyen por el peso más próximo registrado en DGP
peso_dgp <- madre_dgp %>%
  filter(str_detect(str_to_upper(dgp_st), "PESO")) %>%
  mutate(
    peso_dgp = as.numeric(str_replace(result, ",", ".")),
    fecha_dgp = dgp_dt
  ) %>%
  select(
    patient_id,
    fecha_dgp,
    peso_dgp
  )

peso_dgp <- peso_dgp %>%
  mutate(
    
    peso_dgp = if_else(
      peso_dgp < 30 |
        peso_dgp > 220,
      
      NA_real_,
      peso_dgp
    )
  )

limites_dgp <- limites_iqr(peso_dgp$peso_dgp)

lim_inf_dgp <- limites_dgp["inferior"]
lim_sup_dgp <- limites_dgp["superior"]

peso_dgp <- peso_dgp %>%
  mutate(
    peso_dgp = if_else(
      peso_dgp < lim_inf_dgp |
        peso_dgp > lim_sup_dgp,
      NA_real_,
      peso_dgp
    )) %>%
  rename(
    id_madre = patient_id
  )

# Se recupera el peso DGP registrado más próximo al inicio del embarazo
VENTANA_INICIO <- 30

peso_inicio <- embarazo %>%
  select(
    id_embarazo,
    id_madre,
    fur
  ) %>% 
  left_join(
    peso_dgp,
    by = "id_madre"
  )  %>%
  filter(
    !is.na(fur),
    !is.na(fecha_dgp),
    fecha_dgp >= fur - days(VENTANA_INICIO),
    fecha_dgp <= fur + days(VENTANA_INICIO)
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
VENTANA_FINAL <- 20

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
  filter(
    !is.na(fecha_parto),
    !is.na(fecha_dgp),
    
    fecha_dgp >= fecha_parto - days(VENTANA_FINAL),
    fecha_dgp <= fecha_parto + days(VENTANA_FINAL)
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

# 12. Sustituir los pesos atípicos por el registro más próximo de DGP
embarazo <- embarazo %>%
  mutate(
    peso_inicial = if_else(
      peso_inicial_outlier &
        !is.na(peso_dgp_inicio),
      peso_dgp_inicio,
      peso_inicial
    ),
    
    peso_final = if_else(
      peso_final_outlier &
        !is.na(peso_dgp_final),
      peso_dgp_final,
      peso_final
    )
  )

# 13. Recalcular la ganancia de peso y corregir posibles errores de 
#     introducción de datos

PERDIDA_MAX_PESO <- -10
GANANCIA_MAX_PESO <- 35

# Detectar posibles errores de introducción de datos donde la talla
# haya sido registrada como peso

TOLERANCIA_TALLA <- 1

embarazo <- embarazo %>%
  mutate(
    peso_inicial_original = peso_inicial,
    peso_final_original   = peso_final,
    
    error_peso_inicial =
      !is.na(talla) &
      !is.na(peso_inicial_original) &
      abs(peso_inicial_original - talla) <= TOLERANCIA_TALLA,
    
    error_peso_final =
      !is.na(talla) &
      !is.na(peso_final_original) &
      abs(peso_final_original - talla) <= TOLERANCIA_TALLA
  ) %>%
  mutate(
    
    # Ambos pesos parecen corresponder a la talla
    peso_inicial = case_when(
      error_peso_inicial & error_peso_final ~ NA_real_,
      error_peso_inicial & !error_peso_final ~ peso_final_original,
      TRUE ~ peso_inicial_original
    ),
    
    peso_final = case_when(
      error_peso_inicial & error_peso_final ~ NA_real_,
      !error_peso_inicial & error_peso_final ~ peso_inicial_original,
      TRUE ~ peso_final_original
    ),
    
    ganancia_peso = peso_final - peso_inicial,
    
    ganancia_peso = if_else(
      ganancia_peso < PERDIDA_MAX_PESO |
        ganancia_peso > GANANCIA_MAX_PESO,
      NA_real_,
      ganancia_peso
    )
  ) %>%
  select(
    -peso_inicial_original,
    -peso_final_original
  )

# 14. Completar antecedentes obstétricos:

# Los valores perdidos en el número de embarazos, abortos y nacimientos
# anteriores se consideran como ausencia de antecedentes obstétricos y,
# por tanto, se sustituyen por 0
embarazo <- embarazo %>%
  mutate(
    embarazos_anteriores = coalesce(embarazos_anteriores, 0),
    abortos_anteriores = coalesce(abortos_anteriores, 0),
    nacimientos_anteriores = coalesce(nacimientos_anteriores, 0)
  )

# 15. Recalcular el IMC inicial y final cuando esté ausente o presente
#     valores implausibles, utilizando la talla materna disponible
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
    ),
    
    imc_inicial=
      if_else(
        imc_inicial<10 |
          imc_inicial>80,
        
        NA_real_,
        imc_inicial
      ),
    
    imc_final=
      if_else(
        imc_final<10 |
          imc_final>80,
        
        NA_real_,
        imc_final
      )
  ) %>%
  select(
    -talla_m,
    -anio_nacimiento,
    -peso_inicial_outlier,
    -peso_final_outlier,
    -peso_dgp_inicio,
    -peso_dgp_final,
    -error_peso_inicial,
    -error_peso_final
  )

# 16. Formatear las fechas y seleccionar las variables finales
embarazo <- embarazo %>%
  mutate(
    fur = format(fur, "%Y%m%d"),
    fecha_parto = format(fecha_parto, "%Y%m%d"),
    primera_visita_fecha = format(primera_visita_fecha, "%Y%m%d")
  ) %>%
  select(
    id_embarazo,
    id_madre,
    fur,
    primera_visita_fecha,
    fecha_parto,
    tipo_parto,
    embarazos_anteriores,
    nacimientos_anteriores,
    abortos_anteriores,
    edad,
    peso_inicial,
    peso_final,
    ganancia_peso,
    imc_inicial,
    imc_final,
    consumo_tabaco,
    consumo_alcohol
  )  

# 17. Exportar la entidad EMBARAZO
write_csv(
  embarazo,
  file.path(PATH_TRANSFORMADOS,"embarazo.csv")
)
