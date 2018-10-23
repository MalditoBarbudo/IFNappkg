## Polygon objects creation ####

polygons_municipis <- rgdal::readOGR('data-raw/shapefiles', 'bm5mv20sh0tpm1_20180101_0',
                              GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_comarques <- rgdal::readOGR('data-raw/shapefiles', 'bm5mv20sh0tpc1_20180101_0',
                              GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_vegueries <- rgdal::readOGR('data-raw/shapefiles', 'bm5mv20sh0tpv1_20180101_0',
                              GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_provincies <- rgdal::readOGR('data-raw/shapefiles', 'bm5mv20sh0tpp1_20180101_0',
                               GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_delegacions <- rgdal::readOGR('data-raw/shapefiles', 'delegacions2018',
                                       GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_enpe <- rgdal::readOGR('data-raw/shapefiles', 'enpe_2017',
                         GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_pein <- rgdal::readOGR('data-raw/shapefiles', 'pein_2017',
                         GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_xn2000 <- rgdal::readOGR('data-raw/shapefiles', 'xn2000_2017',
                         GDAL1_integer64_policy = FALSE) %>%
  rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

## Polygon divisions names
names_comarcas <- c(
  sort(as.character(polygons_comarques@data$NOMCOMAR))
)

names_municipios <- c(
  sort(as.character(polygons_municipis@data$NOMMUNI))
)

names_veguerias <- c(
  sort(as.character(polygons_vegueries@data$NOMVEGUE))
)

names_provincias <- c(
  sort(as.character(polygons_provincies@data$NOMPROV))
)

names_delegacions <- c(
  sort(as.character(polygons_delegacions@data$comarcas_d))
)

## Polygons dictionary ####
polygons_dictionary <- list(

  ## admin divs

  provincia = list(
    polygon = 'polygons_provincies',
    group = 'provincia',
    label = ~NOMPROV,
    label_new = ~provincia,
    layerId = 'nom_provincies',
    # color_var = ~pal(NOM_PROV),
    label_chr = 'NOMPROV'
  ),

  delegacio = list(
    polygon = 'polygons_delegacions',
    group = 'delegacio',
    label = ~comarcas_d,
    label_new = ~delegacio,
    layerId = 'nom_delegacions',
    label_chr = 'comarcas_d'
  ),

  vegueria = list(
    polygon = 'polygons_vegueries',
    group = 'vegueria',
    label = ~NOMVEGUE,
    label_new = ~vegueria,
    layerId = 'nom_vegueries',
    # color_var = ~pal(NOMVEGUE),
    label_chr = 'NOMVEGUE'
  ),

  comarca = list(
    polygon = 'polygons_comarques',
    group = 'comarca',
    label = ~NOMCOMAR,
    label_new = ~comarca,
    layerId = 'nom_comarques',
    # color_var = ~pal(NOM_COMAR),
    label_chr = 'NOMCOMAR'
  ),

  municipi = list(
    polygon = 'polygons_municipis',
    group = 'municipi',
    label = ~NOMMUNI,
    label_new = ~municipi,
    layerId = 'nom_municipis',
    # color_var = ~pal(NOM_MUNI),
    label_chr = 'NOMMUNI'
  ),

  ## espai tipus

  nomein = list(
    polygon = 'polygons_pein',
    group = 'nomein',
    label = ~nom,
    layerId = 'nom_pein'
  ),

  enpes = list(
    polygon = 'polygons_enpe',
    group = 'enpes',
    label = ~nom,
    layerId = 'nom_enpe'
  ),

  nomxarxa2000 = list(
    polygon = 'polygons_xn2000',
    group = 'nomxarxa2000',
    label = ~nom_n2,
    layerId = 'nom_xn2000'
  )

)