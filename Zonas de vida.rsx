##Análisis=group
##Cuenca=vector
##Zonas_de_vida=output vector

library(geodata)
library(tidyverse)
library(sf)
library(terra)
library(stars)

# Descargando variables
temp <- worldclim_country(country = 'Colombia', var = 'tavg', path = getwd())
precip <- worldclim_country(country = 'Colombia', var = 'prec', path = getwd())
elevacion <- elevation_30s(country = 'Colombia', path = getwd())

if(st_crs(Cuenca)$epsg != 4326){
    Cuenca <- st_transform(Cuenca, crs = 4326)
}

# Cortando los raster para la zona de estudio
temp <- mean(crop(x = temp, y = Cuenca, snap = 'out'))
precip <- sum(crop(x = precip, y = Cuenca, snap = 'out'))
elevacion <- crop(x = elevacion, y = Cuenca, snap = 'out')

# Abreviaturas de las zonas de vida
ab.zonas <- data.frame(Zona_vida = c('Matorral desértico', 'Monte espinoso', 'Bosque seco', 'Bosque muy seco', 'Bosque húmedo',
                                            'Bosque muy húmedo', 'Bosque pluvial', 'Páramo', 'Tundra pluvial', 'Formación nival',
                                            'Desierto polar', 'Tundra seca', 'Tundra húmeda', 'Desierto', 'Tundra muy húmeda', 'Puna', 'Estepa',
                                            'Estepa espinosa'),
                           abreviatura.zona = c('md', 'me', 'bs', 'bms', 'bh', 'bmh', 'bp', 'p', 'tp', 'N', 'dp', 'ts', 'th', 'd', 'tmh', 'pu', 'e', 'ee'))

ab.region <- data.frame(region = c('Tropical', 'Subtropical', 'Templada cálida', 'Templada fría', 'Boreal', 'Subpolar', 'Polar'),
                        abreviatura.region = c('T', 'ST', 'TC', 'TF', 'B', 'SP', 'P'))

ab.piso <- data.frame(piso = c('Basal', 'Premontano', 'Montano bajo', 'Montano', 'Subalpino', 'Alpino', 'Nival'),
                      abreviatura.piso = c('B', 'PM', 'MB', 'M', 'SA', 'A', 'N'))

# Agrupando la información y generando la información de las zonas de vida
var <- c(temp, precip, elevacion)
names(var) <- c('temperatura', 'precipitacion', 'elevacion')

var %>%
    st_as_stars(var)%>%
    st_as_sf()%>%
    mutate(temperatura = case_when(temperatura > 24 ~ 24, temperatura < 0 ~ 0, TRUE ~ temperatura),
         evp = temperatura*58.93/precipitacion,
         Zona_vida = case_when((precipitacion >= 62.5) & (precipitacion < 125) & (evp >= 0.5) & (evp < 1) ~ 'Desierto polar',
                               (precipitacion >= 62.5) & (precipitacion < 125) & (evp >= 1) & (evp < 2) ~ 'Tundra seca',
                               (precipitacion >= 62.5) & (precipitacion < 125) & (evp >= 2) & (evp <= 32) ~ 'Desierto',
                               (precipitacion >= 125) & (precipitacion < 250) & (evp >= 0.25) & (evp < 0.5) ~ 'Desierto polar',
                               (precipitacion >= 125) & (precipitacion < 250) & (evp >= 0.5) & (evp < 1) ~ 'Tundra húmeda',
                               (precipitacion >= 125) & (precipitacion < 250) & (evp >= 1) & (evp < 16) ~ 'Matorral desértico',
                               (precipitacion >= 250) & (precipitacion < 500) & (evp >= 0.125) & (evp < 0.25) ~ 'Desierto polar',
                               (precipitacion >= 250) & (precipitacion < 500) & (evp >= 0.25) & (evp < 0.5) ~ 'Tundra muy húmeda',
                               (precipitacion >= 250) & (precipitacion < 500) & (evp >= 0.5) & (evp < 1) ~ 'Puna',
                               (precipitacion >= 250) & (precipitacion < 500) & (evp >= 1) & (evp < 2) ~ 'Estepa',
                               (precipitacion >= 250) & (precipitacion < 500) & (evp >= 2) & (evp < 4) & (temperatura < 18) ~ 'Estepa espinosa',
                               (precipitacion >= 250) & (precipitacion < 500) & (evp >= 2) & (evp < 4) & (temperatura >= 18) ~ 'Monte espinoso',
                               (precipitacion >= 250) & (precipitacion < 500) & (evp >= 4) & (evp < 8) ~ 'Monte espinoso',
                               (precipitacion >= 500) & (precipitacion < 1000) & (evp >= 0.125) & (evp < 0.25) ~ 'Tundra pluvial',
                               (precipitacion >= 500) & (precipitacion < 1000) & (evp >= 0.25) & (evp < 0.5) ~ 'Páramo',
                               (precipitacion >= 500) & (precipitacion < 1000) & (evp >= 0.5) & (evp < 1) ~ 'Bosque húmedo',
                               (precipitacion >= 500) & (precipitacion < 1000) & (evp >= 1) & (evp < 2) ~ 'Bosque seco',
                               (precipitacion >= 500) & (precipitacion < 1000) & (evp >= 2) & (evp < 4) ~ 'Bosque muy seco',
                               (precipitacion >= 1000) & (precipitacion < 2000) & (evp >= 0.125) & (evp < 0.25) ~ 'Páramo pluvial',
                               (precipitacion >= 1000) & (precipitacion < 2000) & (evp >= 0.25) & (evp < 0.5) ~ 'Bosque muy húmedo',
                               (precipitacion >= 1000) & (precipitacion < 2000) & (evp >= 0.5) & (evp < 1) ~ 'Bosque húmedo',
                               (precipitacion >= 1000) & (precipitacion < 2000) & (evp >= 1) & (evp < 2) ~ 'Bosque seco',
                               (precipitacion >= 2000) & (precipitacion < 4000) & (evp >= 0.125) & (evp < 0.25) ~ 'Bosque pluvial',
                               (precipitacion >= 2000) & (precipitacion < 4000) & (evp >= 0.25) & (evp < 0.5) ~ 'Bosque muy húmedo',
                               (precipitacion >= 2000) & (precipitacion < 4000) & (evp >= 0.5) & (evp < 1) ~ 'Bosque húmedo',
                               (precipitacion >= 4000) & (precipitacion < 8000) & (evp >= 0.125) & (evp < 0.25) ~ 'Bosque pluvial',
                               (precipitacion >= 4000) & (precipitacion < 8000) & (evp >= 0.25) & (evp < 0.5) ~ 'Bosque muy húmedo',
                               (precipitacion >= 8000) & (precipitacion < 16000) & (evp >= 0.125) & (evp < 0.25) ~ 'Bosque pluvial',
                               TRUE ~ 'Información erronea'),
         latitud = st_coordinates(.) %>% as.data.frame() %>% select(Y) %>% slice(1) %>% abs(),
         region = case_when(latitud < 13 ~ 'Tropical',
                            (latitud >= 13) & (latitud < 27.05) ~ 'Subtropical',
                            (latitud >= 27.05) & (latitud < 42) ~ 'Templada cálida',
                            (latitud >= 42) & (latitud < 56.05) ~ 'Templada fría',
                            (latitud >= 56.05) & (latitud < 63.75) ~ 'Boreal',
                            (latitud >= 63.75) & (latitud < 67.36806) ~ 'Subpolar',
                            latitud >= 67.36806 ~ 'Polar'),
         piso = case_when(elevacion < 1000 ~ 'Basal',
                          (elevacion >= 1000) & (elevacion < 2000) ~ 'Premontano',
                          (elevacion >= 2000) & (elevacion < 3000) ~ 'Montano bajo',
                          (elevacion >= 3000) & (elevacion < 4000) ~ 'Montano',
                          (elevacion >= 4000) & (elevacion < 4500) ~ 'Subalpino',
                          (elevacion >= 4500) & (elevacion < 4750) ~ 'Alpino',
                          elevacion >= 450 ~ 'Nival'))%>%
    group_by(Zona_vida, piso, region)%>%
    summarise(n = n()) -> var

var %>% 
  left_join(y = ab.zonas)%>%
  left_join(y = ab.region)%>%
  left_join(y = ab.piso)%>%
  mutate(abreviatura = paste0(abreviatura.zona, ' - ', abreviatura.piso, '/', abreviatura.region))%>%
  select(Zona_vida, piso, region, abreviatura)%>%
  rename(piso_altitudinal = piso, region_latitudinal = region) -> var

Zonas_de_vida <- st_intersection(x = Cuenca, y = var)