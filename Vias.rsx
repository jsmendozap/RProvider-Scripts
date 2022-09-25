##Adquisición de datos=group
##Cuenca=vector
##Vias=output vector

library(pacman)
p_load(sf, osmdata, dplyr, purrr, stringr)

if(st_crs(Cuenca)$epsg != 4326){
    Cuenca <- st_transform(Cuenca, crs = 4326)
}

region <- opq(bbox = st_bbox(Cuenca))
region <- osmdata_sf(region)
region <- region[str_detect(names(region), 'lines')]

Vias <- map_df(.x = region, .f = function(x) select(x, name))
Vias <- st_set_crs(Vias, value = 4326)