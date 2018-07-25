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

# advanced filters button input
label_show_adv_fils <- list(
  cat = 'Filtres avançats',
  esp = 'Más Filtros',
  eng = 'More filters'
)

# apply_filters button input
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
  cat = 'Tipus grup funcional dominant',
  esp = 'Tipo de grupo funcional dominante',
  eng = 'Dominant functional group type'
)

# grup_func input
label_grup_func <- list(
  cat = list(
    scenario1 = list(
      especie = "Mostra l'espècie dominant per densitat",
      especiesimp = "Mostra l'espècie simplificat dominant per densitat",
      genere = 'Mostra el gènere dominant per densitat',
      cadesccon = 'Mostra la Caducifoli/Esclerofil/Conifera dominant per densitat',
      planifconif = 'Mostra la Planifoli/Conifera dominant per densitat'
    ),
    scenario2 = list(
      especie = "Selecciona l'espècie a visualitzar",
      especiesimp = "Selecciona l'espècie simplificat a visualitzar",
      genere = 'Selecciona el génere a visualitzar',
      cadesccon = 'Selecciona la Caducifoli/Esclerofil/Conifera a visualitzar',
      planifconif = 'Selecciona la Planifoli/Conifera a visualitzar'
    )
  ),
  esp = list(
    scenario1 = list(
      especie = 'Muestra la especie dominante en densidad',
      especiesimp = 'Muestra la especie simplificada dominante en densidad',
      genere = 'Muestra el género dominante en densidad',
      cadesccon = 'Muestra la Caducifolia/Esclerofila/Conífera dominante en densidad',
      planifconif = 'Muestra la Planifolia/Conífera dominante en densidad'
    ),
    scenario2 = list(
      especie = 'Selecciona la especie a visualizar',
      especiesimp = 'Selecciona la especie simplificada a visualizar',
      genere = 'Selecciona el género a visualizar',
      cadesccon = 'Selecciona la Caducifolia/Esclerofila/Conífera a visualizar',
      planifconif = 'Selecciona la Planifolia/Conífera a visualizar'
    )
  ),
  eng = list(
    scenario1 = list(
      especie = 'Show the dominant species by density',
      especiesimp = 'Show the dominant simplified species by density',
      genere = 'Show the dominant genera by density',
      cadesccon = 'Show the dominant Deciduous/Sclerophyllous/Conifer by density',
      planifconif = 'Show the dominant Broadleaf/Conifer by density'
    ),
    scenario2 = list(
      especie = 'Select the species to visualize',
      especiesimp = 'Select the simplified species to visualize',
      genere = 'Select the genera to visualize',
      cadesccon = 'Select the Deciduous/Sclerophyllous/Conifer to visualize',
      planifconif = 'Select the Broadleaf/Conifer to visualize'
    )
  )
)

label_grup_func$esp$scenario3 <- label_grup_func$esp$scenario1
label_grup_func$cat$scenario3 <- label_grup_func$cat$scenario1
label_grup_func$eng$scenario3 <- label_grup_func$eng$scenario1
label_grup_func$esp$scenario4 <- label_grup_func$esp$scenario2
label_grup_func$cat$scenario4 <- label_grup_func$cat$scenario2
label_grup_func$eng$scenario4 <- label_grup_func$eng$scenario2

# statistic input
label_statistic <- list(
  cat = 'Mètrica',
  esp = 'Métrica',
  eng = 'Metric'
)