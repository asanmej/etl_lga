# -----------------------------------------------------------------------------
# DATASET ANALÍTICO
#
# Objetivo:
# Construir el dataset analítico final integrando todas las entidades del
# modelo E/R y generando las variables derivadas necesarias para el análisis
# descriptivo y los modelos de regresión logística.
#
# Flujo:
# 1. Cargar entidades
# 2. Construir dataset base
# 3. Generar variables derivadas
# 4. Exportar dataset analítico
# -----------------------------------------------------------------------------

# 1. Cargar entidades y limpieza

## 1.1. EMBARAZO
embarazo <- read_csv(file.path(PATH_TRANSFORMADOS, "embarazo.csv"))

embarazo <- embarazo %>%
  mutate(
    fur = ymd(fur),
    fecha_parto = ymd(fecha_parto),
    primera_visita_fecha = ymd(primera_visita_fecha)
  )

## 1.2. MADRE
madre <- read_csv(file.path(PATH_TRANSFORMADOS,"madre.csv"))

## 1.3. HIJO
hijo <- read_csv(file.path(PATH_TRANSFORMADOS,"hijo.csv"))

hijo <- hijo %>%
  mutate(
    fecha_nacimiento = ymd(fecha_nacimiento)
  )

## 1.4. USO_SERVICIO
uso_servicio <- read_csv(file.path(PATH_TRANSFORMADOS,"uso_servicio.csv"))

## 1.5. DIAGNOSTICO
diagnostico <- read_csv(file.path(PATH_TRANSFORMADOS,"diagnostico.csv"))

diagnostico <- diagnostico %>%
  mutate(
    diag_dt = ymd(diag_dt)
  )

## 1.6. SITUACION_ADMIN_MADRE
situacion_admin_madre <- read_csv(file.path(PATH_TRANSFORMADOS,"situacion_admin_madre.csv"))

#----------------------------------------------------

# 2. Construcción dataset analítico base

# El dataset base contiene únicamente variables originales procedentes de las
# entidades del modelo E/R. Las variables derivadas se generan posteriormente

## 2.1. Embarazo
da_base <- embarazo %>%
  select(
    id_embarazo,
    id_madre,
    fur,
    fecha_parto,
    primera_visita_fecha,
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
    tipo_parto
  )

## 2.2. Madre
da_base <- da_base %>%
  left_join(
    madre %>%
      select(
        id_madre,
        nacionalidad,
        talla
      ),
    by="id_madre"
  )

## 2.3. Hijo
hijo_da <- hijo %>%
  mutate(
    # Variable binaria de macrosomía
    # Se considera macrosomía cuando el peso al nacimiento es >= 4500 g
    macrosomia = if_else(
      peso_nacimiento >= 4500,
      1,
      0,
      missing = NA_real_
    )
  ) %>%
  select(
    id_hijo,
    id_embarazo,
    peso_nacimiento,
    edad_gestacional_nacimiento,
    talla_nacimiento,
    perimetro_craneal,
    macrosomia
  ) 

da_base <- da_base %>%
  left_join(
    hijo_da,
    by= "id_embarazo"
  ) 

## 2.4. Uso de servicios
da_base <- da_base %>%
  left_join(
    uso_servicio %>%
      select(
        id_embarazo,
        n_visitas_embarazo,
        n_visitas_hospitalarias,
        n_visitas_atencion_primaria
      ),
    by="id_embarazo"
  ) 

## 2.5. Situación administrativa de la madre

### Preparar la información administrativa anual
situacion_admin <- situacion_admin_madre %>%
  distinct(
    id_madre,
    anio,
    .keep_all = TRUE
  ) %>%
  select(
    id_madre,
    anio,
    tsi,
    zbs,
    indice_privacion
  )

### Añadimos el año al dataset analítico
da_base <- da_base %>%
  mutate(
    anio = year(fur)
  )

### Asociar la información administrativa correspondiente 
### al año de inicio del embarazo
da_base <- da_base %>%
  left_join(
    situacion_admin,
    by = c(
      "id_madre",
      "anio"
    )
  )

## 2.6. Guardar dataset base (opcional)

# write_csv(
#   da_base,
#   file.path(PATH_TRANSFORMADOS, "dataset_analitico_base.csv")
# )

#----------------------------------------------------

# 3. Variables derivadas

DA <- da_base

## 3.0 Peso analítico por embarazo

# Se calcula un peso analítico para que cada embarazo contribuya con el
# mismo peso total al análisis, independientemente del número de recién
# nacidos asociados (embarazos únicos o múltiples)

# De este modo se evita que los embarazos múltiples tengan una influencia
# desproporcionada al existir más de un registro por embarazo
DA <- DA %>%
  group_by(id_embarazo) %>%
  mutate(
    n_hijos_embarazo = n(),
    peso_embarazo = 1 / n_hijos_embarazo
  ) %>%
  ungroup()

## 3.1 Edad gestacional en la primera visita prenatal

# Variables utilizadas:
# - fur
# - primera_visita_fecha

# Número de semanas transcurridas entre la fecha de última regla (FUR)
# y la primera visita prenatal
DA <- DA %>%
  mutate(
    semanas_primera_visita = as.numeric(
      difftime(
        primera_visita_fecha,
        fur,
        units = "days"
      )
    ) / 7
  )

## 3.2 Inicio precoz del control prenatal

# Variable utilizada: semanas_primera_visita

# Se considera inicio precoz del control prenatal cuando la primera
# visita se realiza antes o en la semana 12 de gestación, siguiendo
# las recomendaciones de la OMS 
DA <- DA %>%
  mutate(
    primera_visita_precoz = case_when(
      is.na(semanas_primera_visita) ~ NA_real_,
      semanas_primera_visita <= 12 ~ 1,
      TRUE ~ 0
    )
  )

## 3.3 Número esperado de visitas prenatales (ACOG)

# Variables utilizadas:
# - semanas_primera_visita
# - edad_gestacional_nacimiento

# Basado en el calendario clásico de seguimiento prenatal recomendado
# históricamente por el American College of Obstetricians and
# Gynecologists (ACOG):
#   - hasta semana 28: cada 4 semanas
#   - semanas 28-36: cada 2 semanas
#   - desde la 36: semanal
calcular_visitas_esperadas <- function(sem_inicio, sem_parto){
  
  if(is.na(sem_inicio) | is.na(sem_parto))
    return(NA_real_)
  
  if(sem_inicio >= sem_parto)
    return(0)
  
  visitas <- 0
  
  # Hasta la semana 28: una visita cada 4 semanas
  inicio <- sem_inicio
  fin <- min(sem_parto, 28)
  
  if(inicio < fin)
    visitas <- visitas + (fin - inicio)/4
  
  # Entre 28 y 36: una visita cada 2 semanas
  inicio <- max(sem_inicio, 28)
  fin <- min(sem_parto, 36)
  
  if(inicio < fin)
    visitas <- visitas + (fin - inicio)/2
  
  # Desde la 36 hasta el parto: semanal
  inicio <- max(sem_inicio, 36)
  fin <- sem_parto
  
  if(inicio < fin)
    visitas <- visitas + (fin - inicio)
  
  return(ceiling(visitas)+1)
  
}

DA <- DA %>%
  mutate(
    visitas_esperadas =
      mapply(
        calcular_visitas_esperadas,
        semanas_primera_visita,
        edad_gestacional_nacimiento
      )
  )

## 3.4 Índice de Kessner

# Variables utilizadas:
# - semanas_primera_visita
# - n_visitas_embarazo
# - edad_gestacional_nacimiento

# Basado en el índice de Kessner, que evalúa la adecuación del
# seguimiento prenatal considerando:
# - trimestre de inicio del seguimiento
# - número de visitas prenatales
# - edad gestacional al parto

### Trimestre de la primera visita
DA <- DA %>%
  mutate(
    trimestre_primera_visita = case_when(
      is.na(semanas_primera_visita) ~ NA_character_,
      semanas_primera_visita <= 13 ~ "Primer trimestre",
      semanas_primera_visita <= 27 ~ "Segundo trimestre",
      TRUE ~ "Tercer trimestre"
    )
  )

### Número mínimo de visitas exigidas por Kessner:

### Para gestaciones pretérmino el número mínimo de visitas
### se ajusta proporcionalmente a la duración de la gestación
DA <- DA %>%
  mutate(
    visitas_minimas_kessner = case_when(
      
      is.na(edad_gestacional_nacimiento) ~ NA_real_,
      # Para gestaciones a término (>=36 semanas) se consideran
      # necesarias al menos 9 visitas prenatales
      edad_gestacional_nacimiento >= 36 ~ 9,
      TRUE ~ ceiling(
        9 * edad_gestacional_nacimiento / 40
      )
    )
  )

### Índice
DA <- DA %>%
  mutate(
    indice_kessner = case_when(
      
      is.na(trimestre_primera_visita) |
        is.na(n_visitas_embarazo) |
        is.na(visitas_minimas_kessner) ~ NA_character_,
      
      # Adecuado: Primera visita en el primer trimestre 
      #           y número suficiente de visitas
      trimestre_primera_visita == "Primer trimestre" &
        n_visitas_embarazo >= visitas_minimas_kessner
      ~ "Adecuado",
      
      # Intermedio: Primera visita en segundo trimestre
      #             o inicio precoz pero seguimiento insuficiente
      (trimestre_primera_visita == "Segundo trimestre" |
         (trimestre_primera_visita == "Primer trimestre" &
            between(n_visitas_embarazo, 5, visitas_minimas_kessner - 1)
         )
      )
      ~ "Intermedio",
      
      # Inadecuado: Inicio en tercer trimestre
      #             o seguimiento insuficiente
      TRUE
      ~ "Inadecuado"
    ) 
  ) %>% 
  select(-visitas_minimas_kessner)

### Factor ordenado
DA <- DA %>%
  mutate(
    indice_kessner = factor(
      indice_kessner,
      levels = c(
        "Inadecuado",
        "Intermedio",
        "Adecuado"
      ),
      ordered = TRUE
    )
  )

## 3.5 Índice APNCU (Adequacy of Prenatal Care Utilization)

# Variables utilizadas:
# - semanas_primera_visita
# - visitas_esperadas
# - n_visitas_embarazo

# El índice APNCU (Kotelchuck) evalúa la adecuación del seguimiento prenatal
# combinando:
# - el momento de inicio del seguimiento prenatal
# - el porcentaje de visitas realizadas respecto a las esperadas.

### Porcentaje de visitas realizadas respecto a las esperadas 
DA <- DA %>%
  mutate(
    porcentaje_visitas = case_when(
      is.na(visitas_esperadas) |
        visitas_esperadas == 0 |
        is.na(n_visitas_embarazo) ~ NA_real_,
      
      TRUE ~ 100 * n_visitas_embarazo / visitas_esperadas
    )
  )

### Momento de inicio del seguimiento
DA <- DA %>%
  mutate(
    inicio_apncu = case_when(
      is.na(semanas_primera_visita) ~ NA_character_,
      semanas_primera_visita <= 16 ~ "Precoz",
      TRUE ~ "Tardío"
    )
  )

### Clasificación APNCU
DA <- DA %>%
  mutate(
    indice_apncu = case_when(
      
      is.na(inicio_apncu) |
        is.na(porcentaje_visitas) ~ NA_character_,
      
      # Inicio después del 4º mes o <50% de las visitas esperadas
      inicio_apncu == "Tardío" |
        porcentaje_visitas < 50 ~
        "Inadecuado",
      
      # Inicio precoz y 50–79%
      inicio_apncu == "Precoz" &
        between(porcentaje_visitas, 50, 79.999) ~
        "Intermedio",
      
      # Inicio precoz y 80–109%
      inicio_apncu == "Precoz" &
        between(porcentaje_visitas, 80, 109.999) ~
        "Adecuado",
      
      # Inicio precoz y ≥110%
      inicio_apncu == "Precoz" &
        porcentaje_visitas >= 110 ~
        "Adecuado Plus"
    )
  ) %>%
  select(-inicio_apncu)

### Factor ordenado
DA <- DA %>%
  mutate(
    indice_apncu = factor(
      indice_apncu,
      levels = c(
        "Inadecuado",
        "Intermedio",
        "Adecuado",
        "Adecuado Plus"
      ),
      ordered = TRUE
    )
  )

## 3.6 Diagnósticos

# A partir de los diagnósticos registrados durante cada embarazo se generan
# variables binarias que identifican las principales complicaciones
# obstétricas de interés para el estudio

### Seleccionar diagnósticos maternos registrados durante el embarazo
diagnosticos_resumen <- diagnostico %>%
  filter(tipo_paciente == "Madre") %>%
  mutate(
    diag_st = str_to_upper(diag_st)
  ) %>%
  select(
    id_paciente,
    diag_dt,
    diag_st
  ) %>%
  rename(
    id_madre = id_paciente
  ) %>%
  ### Conservar únicamente los diagnósticos registrados durante el embarazo
  inner_join(
    DA %>%
      select(
        id_embarazo,
        id_madre,
        fur,
        fecha_parto
      ),
    by = "id_madre"
  ) %>%
  filter(
    !is.na(diag_dt),
    !is.na(fur),
    !is.na(fecha_parto),
    diag_dt >= fur,
    diag_dt <= fecha_parto
  ) %>%
  distinct(
    id_embarazo,
    diag_st,
    .keep_all = TRUE
  ) %>%
  group_by(
    id_embarazo
  ) %>%
  ### Resumir la información clínica a nivel de embarazo
  summarise(
    
    diabetes_gestacional =
      any(
        str_detect(
          diag_st,
          "DIABETES.?GESTACIONAL|DIABETES MELLITUS GESTACIONAL"
        ) &
          !str_detect(
            diag_st,
            "ANTEC|AP |RESUELTA|POSTPARTO|PUERPERIO|RECUPERADA"
          )
      ),
    
    hipertension_gestacional =
      any(
        (
          str_detect(diag_st, "HIPERTENS") |
            str_detect(diag_st, "HTA")
        ) &
          (
            str_detect(diag_st, "GESTACIONAL") |
              str_detect(diag_st, "EMBARAZ") |
              str_detect(diag_st, "TRANSITORIA")
          ) &
          !str_detect(
            diag_st,
            "PREEXIST|ESENCIAL|ARTER"
          )
      ),
    
    preeclampsia =
      any(
        str_detect(
          diag_st,
          "PREECLAMP|PREECLAMS|PRE-ECLAMP"
        )
      ),
    
    eclampsia =
      any(
        str_detect(
          diag_st,
          "ECLAMP"
        )
      ),
    
    .groups = "drop"
    
  )

### Incorporar variables clínicas derivadas al dataset analítico
DA <- DA %>%
  left_join(
    diagnosticos_resumen,
    by = "id_embarazo"
  ) %>%
  mutate(
    # Los embarazos sin ningún diagnóstico compatible se consideran negativos
    diabetes_gestacional =
      coalesce(diabetes_gestacional, FALSE),
    
    hipertension_gestacional =
      coalesce(hipertension_gestacional, FALSE),
    
    preeclampsia =
      coalesce(preeclampsia, FALSE),
    
    eclampsia =
      coalesce(eclampsia, FALSE)
  )

## 3.7 Ganancia excesiva de peso

# Variables utilizadas:
# - imc_inicial
# - ganancia_peso

DA <- DA %>%
  mutate(
    
    ganancia_excesiva_peso = case_when(
      
      is.na(imc_inicial) | is.na(ganancia_peso) ~ NA_real_,
      
      imc_inicial < 18.5 & 
        ganancia_peso > 18 ~ 1,
      
      imc_inicial >= 18.5 &
        imc_inicial < 25 &
        ganancia_peso > 16 ~ 1,
      
      imc_inicial >= 25 &
        imc_inicial < 30 &
        ganancia_peso > 11.5 ~ 1,
      
      imc_inicial >= 30 &
        ganancia_peso > 9 ~ 1,
      
      TRUE ~ 0
      
    )
    
  )

## 3.8 Madre extranjera

# Variable utilizada: nacionalidad

# Clasificación binaria de la nacionalidad materna (España = 0; resto = 1).
DA <- DA %>%
  mutate(
    madre_extranjera = case_when(
      is.na(nacionalidad) ~ NA_real_,
      str_to_upper(nacionalidad) %in%
        c("ESPAÑA","ESPANA") ~ 0,
      TRUE ~ 1
    )
  )

## 3.9. Clasificación global basada en Juárez et al. (Scientific Reports, 2025).

# Se consideran Global North:
# - España
# - países de la UE28
# - Canadá
# - Estados Unidos

# El resto de países se clasifican como Global South

DA <- DA %>%
  mutate(
    # Origen materno
    maternal_origin = factor(
      case_when(
        is.na(nacionalidad) ~ NA_character_,
        str_to_upper(nacionalidad) %in% c("ESPAÑA","ESPANA") ~ "Spain",
        TRUE ~ "Foreign"
      ),
      levels=c("Spain","Foreign")
    ),
    
    # Global North / Global South (según Juárez et al.)
    global_region = case_when(
      
      str_to_upper(nacionalidad) %in% c(
        
        # España
        "ESPAÑA",
        
        # UE28
        "RUMANIA",
        "BULGARIA",
        "FRANCIA",
        "POLONIA",
        "PORTUGAL",
        "ITALIA",
        "BELGICA",
        "ALEMANIA",
        "PAISES BAJOS",
        "HUNGRIA",
        "LITUANIA",
        "REPUBLICA CHECA",
        "ESLOVAQUIA",
        "GRECIA",
        "AUSTRIA",
        "FINLANDIA",
        "R.U.GRAN BRETAÑA E IRL N.",
        
        # Norteamérica (alto ingreso)
        "CANADA",
        "ESTADOS UNIDOS DE AMERICA"
        
      ) ~ "Global North",
      
      TRUE ~ "Global South"
      
    ),
    
    global_region = factor(
      global_region,
      levels = c("Global North", "Global South")
    )
  )

# 3.10. Convertir a factor todas las variables binarias/categóricas
DA <- DA %>%
  mutate(
    
    consumo_tabaco = factor(consumo_tabaco,
                            levels = c(0,1),
                            labels = c("No","Yes")),
    
    consumo_alcohol = factor(consumo_alcohol,
                             levels = c(0,1),
                             labels = c("No","Yes")),
    
    macrosomia = factor(macrosomia,
                        levels = c(0,1),
                        labels = c("No","Yes")),
    
    primera_visita_precoz = factor(primera_visita_precoz,
                                   levels = c(0,1),
                                   labels = c("No","Yes")),
    
    madre_extranjera = factor(madre_extranjera,
                              levels = c(0,1),
                              labels = c("Spain","Foreign")),
    
    diabetes_gestacional = factor(diabetes_gestacional,
                                  levels = c(FALSE, TRUE),
                                  labels = c("No","Yes")),
    
    hipertension_gestacional = factor(hipertension_gestacional,
                                      levels = c(FALSE, TRUE),
                                      labels = c("No","Yes")),
    
    preeclampsia = factor(preeclampsia,
                          levels = c(FALSE, TRUE),
                          labels = c("No","Yes")),
    
    eclampsia = factor(eclampsia,
                       levels = c(FALSE, TRUE),
                       labels = c("No","Yes")),
    
    ganancia_excesiva_peso = factor(ganancia_excesiva_peso,
                                    levels = c(0,1),
                                    labels = c("No","Yes")),
    
    tipo_parto = factor(tipo_parto),
    
    tsi = factor(tsi),
    
    zbs = factor(zbs)
  )
#----------------------------------------------------

# 4. Exportar dataset analítico
write_csv(
  DA,
  file.path(PATH_TRANSFORMADOS,"dataset_analitico.csv")
)
