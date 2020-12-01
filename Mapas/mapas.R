library(raster)
library(rgdal)

download.file("https://www.indec.gob.ar/ftp/cuadros/territorio/codgeo/Codgeo_Pais_x_dpto_con_datos.zip", "Mapas/departamentosArg.zip")
unzip(zipfile = "Mapas/departamentosArg.zip", exdir = "Mapas")
Deptos <- readOGR("Mapas/pxdptodatosok.shp", encoding = 'UTF-8')
save(Deptos,file="Mapas.Rdata")
