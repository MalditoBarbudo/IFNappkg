## Polygon objects creation ####

polygons_municipis <- rgdal::readOGR('data-raw/shapefiles', 'bm1000mv33sh1fpm1r170',
                              GDAL1_integer64_policy = FALSE) %>%
  rgdal::spTransform(rgdal::CRS("+proj=longlat +datum=WGS84"))

polygons_comarques <- rgdal::readOGR('data-raw/shapefiles', 'bm1000mv33sh1fpc1r170',
                              GDAL1_integer64_policy = FALSE) %>%
  rgdal::spTransform(rgdal::CRS("+proj=longlat +datum=WGS84"))

polygons_vegueries <- rgdal::readOGR('data-raw/shapefiles', 'bm500mv20sh0tpv1_20180101_0',
                              GDAL1_integer64_policy = FALSE) %>%
  rgdal::spTransform(rgdal::CRS("+proj=longlat +datum=WGS84"))

polygons_provincies <- rgdal::readOGR('data-raw/shapefiles', 'bm1000mv33sh1fpp1r170',
                               GDAL1_integer64_policy = FALSE) %>%
  rgdal::spTransform(rgdal::CRS("+proj=longlat +datum=WGS84"))

polygons_enpe <- rgdal::readOGR('data-raw/shapefiles', 'enpe_2017',
                         GDAL1_integer64_policy = FALSE) %>%
  rgdal::spTransform(rgdal::CRS("+proj=longlat +datum=WGS84"))

polygons_pein <- rgdal::readOGR('data-raw/shapefiles', 'pein_2017',
                         GDAL1_integer64_policy = FALSE) %>%
  rgdal::spTransform(rgdal::CRS("+proj=longlat +datum=WGS84"))

polygons_xn2000 <- rgdal::readOGR('data-raw/shapefiles', 'xn2000_2017',
                         GDAL1_integer64_policy = FALSE) %>%
  rgdal::spTransform(rgdal::CRS("+proj=longlat +datum=WGS84"))

## Polygon divisions names
names_comarcas <- c(
  sort(as.character(polygons_comarques@data$NOM_COMAR))
)

names_municipios <- c(
  sort(as.character(polygons_municipis@data$NOM_MUNI))
)

names_veguerias <- c(
  sort(as.character(polygons_vegueries@data$NOMVEGUE))
)

names_provincias <- c(
  sort(as.character(polygons_provincies@data$NOM_PROV))
)