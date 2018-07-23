## mod_data inputs ####

# ifn input
label_ifn <- list(
  cat = 'Versió de les dades',
  esp = 'Versión de los datos',
  eng = 'Data version'
)

# viz_shape input
label_viz_shape <- list(
  cat = 'Tipus de visualització',
  esp = 'Tipo de visualización',
  eng = 'Visualization kind'
)

# admin_div input
label_admin_div <- list(
  cat = 'Divisions administratives',
  esp = 'Divisiones administrativas',
  eng = 'Administrative divisions'
)

# espai_tipus input
label_espai_tipus <- list(
  cat = "Tipus d'espai",
  esp = "Tipo de espacio",
  eng = "Space kind"
)

# admin_div_fil input
label_admin_div_fil <- list(
  cat = list(
    provincia = 'Filtra per provincia',
    vegueria = 'Filtra per vegueria',
    comarca = 'Filtra per comarca',
    municipi = 'Filtra per municipi'
  ),
  esp = list(
    provincia = 'Filtra por provincia',
    vegueria = 'Filtra por vegueria',
    comarca = 'Filtra por comarca',
    municipi = 'Filtra por municipio'
  ),
  eng = list(
    provincia = 'Filter by province',
    vegueria = 'Filter by vegueria',
    comarca = 'Filter by region',
    municipi = 'Filter by municipality'
  )
)

# espai_tipus_fil input
label_espai_tipus_fil <- list(
  cat = list(
    proteccio = 'Filtra per nivell de proteccio',
    nomein = "Filtra per espai d'interès nacional",
    enpes = 'Filtra per espai de protecció especial',
    nomxarxa2000 = 'Filtra per xarxa natura 2000'
  ),
  esp = list(
    proteccio = 'Filtra por nivel de protección',
    nomein = 'Filtra por espacio de interés nacional',
    enpes = 'Filtra por espacio de protección especial',
    nomxarxa2000 = 'Filtra por red natura 2000'
  ),
  eng = list(
    proteccio = 'Filter by protection level',
    nomein = 'Filter by national space of interest',
    enpes = 'Filter by special protection space',
    nomxarxa2000 = 'Filter by natura 2000 net'
  )
)

# apply_filters input
label_apply_filters <- list(
  cat = "Aplicar filtres",
  esp = 'Aplicar filtros',
  eng = 'Apply filters'
)

# agg_level input
label_agg_level <- list(
  cat = "Nivell d'agregació",
  esp = 'Nivel de agragación',
  eng = 'Aggregation level'
)

# diam_class input
label_diam_class <- list(
  cat = '¿Desglossar per classes diametriques?',
  esp = '¿Desglosar por clases diamétricas',
  eng = 'Breakdown by diameter classes?'
)

## mod_viz inputs ####

# color input
# there is no need, as color is equal in the three lenguages

# mida input (size)
label_mida <- list(
  cat = 'Mida',
  esp = 'Tamaño',
  eng = 'Size'
)

# tipo_grup_func input
label_tipo_grup_func <- list(
  cat = 'Tipus grup funcional',
  esp = 'Tipo de grupo funcional',
  eng = 'Functional group type'
)

# grup_func input
label_grup_func <- list(
  cat = list(
    scenario1 = list(
      especie = 'Espècie dominant per densitat',
      especiesimp = 'Espècie simplificat dominant per densitat',
      genere = 'Gènere dominant per densitat',
      cadesccon = 'Caducifoli/Esclerofil/Conifera dominant per densitat',
      planifconif = 'Planifoli/Conifera dominant per densitat'
    ),
    scenario2 = list(
      especie = 'Espècie a visualitzar',
      especiesimp = 'Espècie simplificat a visualitzar',
      genere = 'Génere a visualitzar',
      cadesccon = 'Caducifoli/Esclerofil/Conifera a visualitzar',
      planifconif = 'Planifoli/Conifera a visualitzar'
    )
  ),
  esp = list(
    scenario1 = list(
      especie = 'Especie dominante en densidad',
      especiesimp = 'Especie simplificada dominante en densidad',
      genere = 'Género dominante en densidad',
      cadesccon = 'Caducifolia/Esclerofila/Conífera dominante en densidad',
      planifconif = 'Planifolia/Conífera dominante en densidad'
    ),
    scenario2 = list(
      especie = 'Especie a visualizar',
      especiesimp = 'Especie simplificada a visualizar',
      genere = 'Género a visualizar',
      cadesccon = 'Caducifolia/Esclerofila/Conífera a visualizar',
      planifconif = 'Planifolia/Conífera a visualizar'
    )
  ),
  eng = list(
    scenario1 = list(
      especie = 'Dominant species by density',
      especiesimp = 'Dominant simplified species by density',
      genere = 'Dominant genera by density',
      cadesccon = 'Dominant Deciduous/Sclerophyllous/Conifer by density',
      planifconif = 'Dominant Broadleaf/Conifer by density'
    ),
    scenario2 = list(
      especie = 'Species to visualize',
      especiesimp = 'Simplified species to visualize',
      genere = 'Genera to visualize',
      cadesccon = 'Deciduous/Sclerophyllous/Conifer to visualize',
      planifconif = 'Broadleaf/Conifer to visualize'
    )
  )
)

# statistic input
label_statistic <- list(
  cat = 'Mètrica',
  esp = 'Métrica',
  eng = 'Metric'
)