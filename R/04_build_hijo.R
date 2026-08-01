# -----------------------------------------------------------------------------
# ENTIDAD HIJO
#
# Objetivo:
# Construir la entidad HIJO a partir de la información neonatal registrada en
# NeoSoft y de los datos demográficos, vinculando cada recién nacido con el
# embarazo previamente reconstruido.
#
# Se conservan únicamente los hijos pertenecientes a los embarazos incluidos en
# la cohorte de estudio y se aplican controles básicos de calidad sobre las
# variables clínicas
# -----------------------------------------------------------------------------

# 1. Los objetos hijo_neosoft y hijo_demograficos se generan en 
#    02_reconstruccion_embarazos.R y la entidad embarazo en
#    03_build_embarazo.R

# 2. Limpieza en 02_reconstruccion_embarazos.R

# 3. Seleccionar los atributos principales que definen la entidad HIJO
hijo <- hijo_neosoft %>%
  select(
    patient_id,
    peso_nacimiento,
    talla_nacimiento,
    perimetro_craneal,
    edad_gestacional,
    malformation_cd,
    muerte_neonatal
  ) %>%
  distinct()

# 4. Aplicar un filtro de plausibilidad biológica sobre el peso y la talla al
#    nacimiento
hijo <- hijo %>%
  mutate(
    
    peso_nacimiento =
      if_else(
        peso_nacimiento < 500 |
          peso_nacimiento > 5300,
        
        NA_real_,
        peso_nacimiento
      ),
    
    talla_nacimiento =
      if_else(
        talla_nacimiento < 21.4 |
          talla_nacimiento > 65,
        
        NA_real_,
        talla_nacimiento
      )
  )

# 5. Construir la fecha de nacimiento a partir del año y el mes disponibles

# Al desconocerse el día, se asigna el día 15 del mes según la convención
# definida en la documentación del proyecto
fecha_nacimiento <- hijo_demograficos %>%
  mutate(
    fecha_nacimiento = make_date(
      ano_nac,
      mes_nac,
      15
    )
  ) %>%
  filter(
    between(year(fecha_nacimiento), 2018, 2023)
  ) %>%
  select(
    patient_id,
    fecha_nacimiento
  )

# Incorporar la fecha de nacimiento a la entidad HIJO
hijo <- hijo %>%
  left_join(
    fecha_nacimiento,
    by="patient_id"
  ) %>%
  filter(!is.na(fecha_nacimiento))

# 6. Adaptar los nombres de las variables a la nomenclatura definida en el modelo E/R
hijo <- hijo %>% 
  rename(
    id_hijo = patient_id, 
    edad_gestacional_nacimiento = edad_gestacional
  )

# 7. Asociar cada recién nacido con el embarazo previamente reconstruido
hijo <- hijo %>%
  left_join(
    embarazo %>%
      select(
        id_hijo,
        id_embarazo
      ),
    by = "id_hijo"
  )

# 8. Conservar únicamente los hijos cuyo embarazo pertenece al periodo de estudio
hijo <- hijo %>%
  filter(
    !is.na(id_embarazo)
  )

# 9. Formatear las fechas y reordenar variables 
hijo <- hijo %>%
  mutate(
    fecha_nacimiento = format(fecha_nacimiento, "%Y%m%d")
  ) %>%
  select(
    id_hijo,
    id_embarazo,
    peso_nacimiento,
    talla_nacimiento,
    perimetro_craneal,
    edad_gestacional_nacimiento,
    malformation_cd,
    muerte_neonatal,
    fecha_nacimiento
  ) %>%
  distinct() 

# 10. Exportar la entidad HIJO
write_csv(
  hijo,
  file.path(PATH_TRANSFORMADOS,"hijo.csv")
)
