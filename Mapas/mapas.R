library(raster)
library(rgdal)
library(geosphere)

Deptos <- readOGR("Mapas/mapaProv.shp", encoding = 'UTF-8')
Deptos@data$depto <- Deptos@data$PROVRES2
Deptos@data <-  cbind(Deptos@data,data.frame(centroid(Deptos)))
Arg <- raster::aggregate(Deptos)
save(Deptos,file="Mapas/Mapas.Rdata")
