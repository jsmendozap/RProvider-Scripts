##Adquisición de datos=group
##Layer=vector
##Fecha_inicial=datetime
##Fecha_final=datetime
##Output=output vector

library(chirps)
library(sf)

inicial <- as.Date(Fecha_inicial)
final <- as.Date(Fecha_final)
if(st_crs(Layer)$epsg != 4326){
    Layer <- st_transform(Layer, crs = 4326)
}
res <- get_chirps(object = Layer, dates = c(inicial, final), server = 'CHC')
Output = st_as_sf(res, coords = c('lon', 'lat'), crs = 4326)