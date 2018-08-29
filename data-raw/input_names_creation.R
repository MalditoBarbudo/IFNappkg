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
  esp = list(
    on = 'Desglosado por clases diamétricas',
    off = 'Sin desglosar por clases diamétricas'
  ),
  eng = 'Breakdown by diameter classes?'
)

## mod_viz inputs ####

# color input
# there is no need, as color is equal in the three lenguages

# mida input (size)
label_mida <- list(
  cat = 'Mida',
  esp = 'Selecciona la variable para modificar el tamaño de la parcela',
  eng = 'Size'
)

# tipo_grup_func input
label_tipo_grup_func <- list(
  cat = 'Tipus grup funcional dominant',
  esp = 'Elige el tipo de grupo funcional dominante a visualizar',
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
      especie = 'Elige la especie dominante en densidad a mostrar',
      especiesimp = 'Elige la especie simplificada dominante en densidad a mostrar',
      genere = 'Elige el género dominante en densidad a mostrar',
      cadesccon = 'Elige la Caducifolia/Esclerofila/Conífera dominante en densidad a mostrar',
      planifconif = 'Elige la Planifolia/Conífera dominante en densidad a mostrar'
    ),
    scenario2 = list(
      especie = 'Elige la especie a mostrar',
      especiesimp = 'Elige la especie simplificada a mostrar',
      genere = 'Elige el género a mostrar',
      cadesccon = 'Elige la Caducifolia/Esclerofila/Conífera a mostrar',
      planifconif = 'Elige la Planifolia/Conífera a mostrar'
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
  esp = 'Métrica a visualizar',
  eng = 'Metric'
)

## mod_infopanel plots ####
label_tabpanel_visualization <- list(
  cat = 'Visualització',
  esp = 'Visualización',
  eng = 'Visualization'
)

label_infopanel_plot <- list(
  cat = list(
    cd = list(
      scenario1 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario2 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario3 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario4 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      )
    ),
    nocd = list(
      scenario1 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario2 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario3 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario4 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      )
    )
  ),
  esp = list(
    cd = list(
      scenario1 = list(
        plot = list(
          title = 'Parcela #{click$id}',
          subtitle = 'por clase diamétrica'
        ),
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por clase diamétrica'
        )
      ),
      scenario2 = list(
        plot = list(
          title = 'Parcela #{click$id}',
          subtitle = 'por {label_infopanel_variables[["esp"]][[glue::glue("id{agg_level}")]]} y clase diamétrica'
        ),
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por {label_infopanel_variables[["esp"]][[glue::glue("id{agg_level}")]]} y clase diamétrica'
        )
      ),
      scenario3 = list(
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por clase diamétrica'
        )
      ),
      scenario4 = list(
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por {label_infopanel_variables[["esp"]][[glue::glue("id{agg_level}")]]} y clase diamétrica'
        )
      )
    ),
    nocd = list(
      scenario1 = list(
        plot = list(
          title = 'Parcela #{click$id}',
          subtitle = 'por {tolower(label_infopanel_variables[["esp"]][[glue::glue("{tipo_grup_func}_dom_percdens")]])}'
        ),
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por {tolower(label_infopanel_variables[["esp"]][[glue::glue("{tipo_grup_func}_dom_percdens")]])}'
        )
      ),
      scenario2 = list(
        plot = list(
          title = 'Parcela #{click$id}',
          subtitle = 'por {label_infopanel_variables[["esp"]][[glue::glue("id{agg_level}")]]}'
        ),
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por {label_infopanel_variables[["esp"]][[glue::glue("id{agg_level}")]]}'
        )
      ),
      scenario3 = list(
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por {tolower(label_infopanel_variables[["esp"]][[glue::glue("{tipo_grup_func}_dom_percdens")]])}'
        )
      ),
      scenario4 = list(
        polygon = list(
          title = 'Parcelas en {click$id}',
          subtitle = 'por {label_infopanel_variables[["esp"]][[glue::glue("id{agg_level}")]]}'
        )
      )
    )
  ),
  eng = list(
    cd = list(
      scenario1 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario2 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario3 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario4 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      )
    ),
    nocd = list(
      scenario1 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario2 = list(
        plot = list(
          title = '',
          subtitle = ''
        ),
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario3 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      ),
      scenario4 = list(
        polygon = list(
          title = '',
          subtitle = ''
        )
      )
    )
  )
)




  list(
  cat = list(
    parcela = list(
      title = 'Parcel·la {click$id}',
      subtitle = ''
    ),
    polygon = list(
      title = 'Parcel·les seleccionats a {click$id}',
      subtitle = ''
    )
  ),
  esp = list(
    parcela = list(
      title = 'Parcela {click$id}',
      subtitle = ''
    ),
    polygon = list(
      title = 'Parcelas seleccionadas en {click$id}',
      subtitle = ''
    )
  ),
  eng = list(
    parcela = list(
      title = 'Plot {click$id}',
      subtitle = ''
    ),
    polygon = list(
      title = 'Plots selected in {click$id}',
      subtitle = ''
    )
  )
)

label_infopanel_variables <- list(
  cat = list(
    # y values
    "idparcela" = "ID parcel·la",
    "idclasse" = "ID classe",
    "idcd" = 'Classe diametrica',
    "cadesccon_dom_percdens" = "Caducifoli/Esclerofil/Conifera dominant per densitat",
    "cadesccon_dom_percdens_val" = "Percentatge Densitat Caducifoli/Esclerofil/Conifera dominant",
    "cadesccon_dom_percab" = "Caducifoli/Esclerofil/Conifera dominant per àrea basal",
    "cadesccon_dom_percab_val" = "Percentatge Àrea Basal Caducifoli/Esclerofil/Conifera dominant",
    "planifconif_dom_percdens" = "Planifoli/Conifera dominant per densitat",
    "planifconif_dom_percdens_val" = "Percentatge Densitat Planifoli/Conifera dominant",
    "planifconif_dom_percab" = "Planifoli/Conifera dominant per àrea basal",
    "planifconif_dom_percab_val" = "Percentatge Àrea Basal Planifoli/Conifera dominant",
    "genere_dom_percdens" = "Gènere dominant per densitat",
    "genere_dom_percdens_val" = "Percentatge Densitat Gènere dominant",
    "genere_dom_percab" = "Gènere dominant per àrea basal",
    "genere_dom_percab_val" = "Percentatge Àrea Basal Gènere dominant",
    "especiesimp_dom_percdens" = "Espècie simplificat dominant per densitat",
    "especiesimp_dom_percdens_val" = "Percentatge Densitat Espècie simplificat dominant",
    "especiesimp_dom_percab" = "Espècie simplificat dominant per àrea basal",
    "especiesimp_dom_percab_val" = "Percentatge Àrea Basal Espècie simplificat dominant",
    "especie_dom_percdens" = "Espècie dominant per densitat",
    "especie_dom_percdens_val" = "Percentatge Densitat Espècie dominant",
    "especie_dom_percab" = "Espècie dominant per àrea basal",
    "especie_dom_percab_val" = "Percentatge Àrea Basal Espècie dominant",
    "densitat" = "Densitat total parcel·la",
    "densitatmorts" = "Densitat total peus morts parcel·la",
    "ab" = "Àrea Basal total parcel·la",
    "abmorts" = "Àrea Basal total peus morts parcel·la",
    "dbh" = "Diàmetre a l'altura del pit parcel·la",
    "dbhmorts" = "Diàmetre a l'altura del pit peus morts parcel·la",
    "rc" = "rc",
    "vcc" = "vcc",
    "vccmorts" = "vccmorts",
    "vsc" = "vsc",
    "vscmorts" = "vscmorts",
    "bm" = "bm",
    "bc" = "bc",
    "br" = "br",
    "bh" = "bh",
    "bat" = "bat",
    "iaf" = "iaf",
    "cm" = "cm",
    "cc" = "cc",
    "cr" = "cr",
    "ch" = "ch",
    "cat" = "cat",
    "cca" = "cca",
    "ordredens" = 'Ordre per Densitat',
    "ordreab" = 'Ordre per Àrea Basal',
    "percdens" = 'Percentatge Densitat',
    "percab" = 'Percentatge Àrea Basal',
    # x values
    'idespecie' = 'Espècie',
    'idespeciesimp' = 'Espècie simplificat',
    'idgenere' = 'Gènere',
    'idcadesccon' = 'Conífera/Caducifoli/Esclerofil·le',
    'idplanifconif' = 'Conífera/Planifoli'
  ),
  esp = list(
    "idparcela" = "ID parcela",
    "idclasse" = "ID clase",
    "idcd" = 'Clase diamétrica',
    "cadesccon_dom_percdens" = "Caducifolia/Esclerofila/Conifera dominante por densidad",
    "cadesccon_dom_percdens_val" = "Porcentaje Densidad Caducifolia/Esclerofila/Conifera dominante",
    "cadesccon_dom_percab" = "Caducifolia/Esclerofila/Conifera dominante por área basal",
    "cadesccon_dom_percab_val" = "Porcentaje Área Basal Caducifolia/Esclerofila/Conifera dominante",
    "planifconif_dom_percdens" = "Planifolia/Conifera dominante por densidad",
    "planifconif_dom_percdens_val" = "Porcentaje Densidad Planifolia/Conifera dominante",
    "planifconif_dom_percab" = "Planifolia/Conifera dominante por área basal",
    "planifconif_dom_percab_val" = "Porcentaje Área Basal Planifolia/Conifera dominante",
    "genere_dom_percdens" = "Género dominante por densidad",
    "genere_dom_percdens_val" = "Porcentaje Densidad Género dominante",
    "genere_dom_percab" = "Género dominante por área basal",
    "genere_dom_percab_val" = "Porcentaje Área Basal Género dominante",
    "especiesimp_dom_percdens" = "Especie simplificado dominante por densidad",
    "especiesimp_dom_percdens_val" = "Porcentaje Densidad Especie simplificado dominante",
    "especiesimp_dom_percab" = "Especie simplificado dominante por densidad",
    "especiesimp_dom_percab_val" = "Porcentaje Densidad Especie simplificado dominante",
    "especie_dom_percdens" = "Especie dominante por densidad",
    "especie_dom_percdens_val" = "Porcentaje Densidad Especie dominante",
    "especie_dom_percab" = "Especie dominante por densidad",
    "especie_dom_percab_val" = "Porcentaje Densidad Especie dominante",
    "densitat" = "Densidad total parcela",
    "densitatmorts" = "Densidad total pies muertos parcela",
    "ab" = "Área Basal total parcela",
    "abmorts" = "Área Basal total pies muertos parcela",
    "dbh" = "Diámetro a la altura del pecho parcela",
    "dbhmorts" = "Diámetro a la altura del pecho pies muertos parcela",
    "rc" = "rc",
    "vcc" = "vcc",
    "vccmorts" = "vccmorts",
    "vsc" = "vsc",
    "vscmorts" = "vscmorts",
    "bm" = "bm",
    "bc" = "bc",
    "br" = "br",
    "bh" = "bh",
    "bat" = "bat",
    "iaf" = "iaf",
    "cm" = "cm",
    "cc" = "cc",
    "cr" = "cr",
    "ch" = "ch",
    "cat" = "cat",
    "cca" = "cca",
    "ordredens" = 'Orden por densidad',
    "ordreab" = 'Orden por área basal',
    "percdens" = 'Porcentaje densidad',
    "percab" = 'Porcentaje área basal',
    # x values
    'idespecie' = 'Especie',
    'idespeciesimp' = 'Especie simplificada',
    'idgenere' = 'Género',
    'idcadesccon' = 'Caducifolia/Esclerófila/Conífera',
    'idplanifconif' = 'Planifolia/Conífera'
  ),
  eng = list(
    "idparcela" = "ID plot",
    "idclasse" = "ID class",
    "idcd" = "Diamter Class",
    "cadesccon_dom_percdens" = "Dominant Deciduous/Sclerophyllous/Conifer by density",
    "cadesccon_dom_percdens_val" = "Density Percentage Dominant Deciduous/Sclerophyllous/Conifer",
    "cadesccon_dom_percab" = "Dominant Deciduous/Sclerophyllous/Conifer by basal area",
    "cadesccon_dom_percab_val" = "Basal Area Percentage Dominant Deciduous/Sclerophyllous/Conifer",
    "planifconif_dom_percdens" = "Dominant Broadleaf/Conifer by density",
    "planifconif_dom_percdens_val" = "Density Percentage Dominant Broadleaf/Conifer",
    "planifconif_dom_percab" = "Dominant Broadleaf/Conifer by basal area",
    "planifconif_dom_percab_val" = "Basal Area Percentage Dominant Broadleaf/Conifer",
    "genere_dom_percdens" = "Dominant Genera by density",
    "genere_dom_percdens_val" = "Density Percentage Dominant genera",
    "genere_dom_percab" = "Dominant Genera by basal area",
    "genere_dom_percab_val" = "Basal Area Percentage Dominant Genera",
    "especiesimp_dom_percdens" = "Dominant Simplified Species by density",
    "especiesimp_dom_percdens_val" = "Density Percentage Dominant Simplified Species",
    "especiesimp_dom_percab" = "Dominant Simplified Species by basal area",
    "especiesimp_dom_percab_val" = "Basal Area Percentage Dominant Simplified Species",
    "especie_dom_percdens" = "Dominant Species by density",
    "especie_dom_percdens_val" = "Density Percentage Dominant Species",
    "especie_dom_percab" = "Dominant Species by basal area",
    "especie_dom_percab_val" = "Basal Area Percentage Dominant Species",
    "densitat" = "Total Plot Density",
    "densitatmorts" = "Total Plot Density Dead Trees",
    "ab" = "Total Plot Basal Area",
    "abmorts" = "Total Plot Basal Area Dead Trees",
    "dbh" = "Diameter at Breast Height Plot",
    "dbhmorts" = "Diameter at Breast Height Dead Trees plot",
    "rc" = "rc",
    "vcc" = "vcc",
    "vccmorts" = "vccmorts",
    "vsc" = "vsc",
    "vscmorts" = "vscmorts",
    "bm" = "bm",
    "bc" = "bc",
    "br" = "br",
    "bh" = "bh",
    "bat" = "bat",
    "iaf" = "iaf",
    "cm" = "cm",
    "cc" = "cc",
    "cr" = "cr",
    "ch" = "ch",
    "cat" = "cat",
    "cca" = "cca",
    "ordredens" = 'Rank by Density',
    "ordreab" = 'Rank by Basal Area',
    "percdens" = 'Density Percentage',
    "percab" = 'Basal Area Percentage',
    # x values
    'idespecie' = 'Species',
    'idespeciesimp' = 'Simplified Species',
    'idgenere' = 'Genera',
    'idcadesccon' = 'Deciduous/Sclerophyllous/Conifer',
    'idplanifconif' = 'Broadleaf/Conifer'
  )
)

## mod_infopanel info ####

label_shape_click_info <- list(
  esp = list(
    plot = list(
      header = 'Parcela #{click$id}',
      altitude = 'Altitud: {sig[["altitud"]][1]}',
      slope = 'Pendiente: {sig[["pendentpercentatge"]][1]}',
      muni = 'Municipio: {sig[["municipi"]][1]}',
      comarca = 'Comarca: {sig[["comarca"]][1]}',
      province = 'Provincia: {sig[["provincia"]][1]}',

      an_rad = 'Radiación anual: {clima[["radiacioanual"]][1]}',
      an_ave_temp = 'Temperatura media anual: {clima[["temperaturamitjanaanual"]][1]}',
      an_prec = 'Precipitación anual: {clima[["precipitacioanual"]][1]}'
    ),
    polygon = list(
      header = '{length(unique(sig[["idparcela"]]))} parcelas en {click$id}',
      explanation = ''
    )
  )
)