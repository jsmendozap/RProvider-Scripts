##Adquisición de datos=group
##Layer=vector
##variables=selection Precipitación;Temperatura
##Output=output raster

library(geodata)
library(terra)

var <- NULL
if(variables == 0){
    var <- 'prec'
}else{
    var <- 'tavg'
}

variable <- worldclim_country(country = 'Colombia', var = var, path = getwd())

if(st_crs(Layer)$epsg != 4326){
    Layer <- st_transform(Layer, crs = 4326)
}

variable <- terra::crop(x = variable, y = Layer)

if (variables == 0){
    Output <- sum(variable)
}else{
    names(variable) <- month.name
    Output <- variable
}