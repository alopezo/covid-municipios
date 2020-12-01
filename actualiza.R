library(dplyr)
library(tidyverse)
library(zoo)
library(TTR)

##### FIJAR DIRECTORIO DE TRABAJO ####
#setwd("D:/municipios")

setwd("C:/Users/usuario/Documents/municipios")
#### DESCARGA DATOS  ####
urlMsal <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv'
download.file(urlMsal, "Covid19Casos.csv")

#### IMPORTA DATOS ####
dataMsal<-read.csv("Covid19Casos.csv", fileEncoding = "UTF-8")
dataMsal<-dataMsal %>% filter(clasificacion_resumen=="Confirmado" & residencia_provincia_id==6)

##### NOMBRES DE PARTIDOS PARA APP #####
denom_depto <- dataMsal %>% distinct(residencia_departamento_id, residencia_departamento_nombre) %>%
                            arrange(residencia_departamento_id, residencia_departamento_nombre)

                            
#### CREA DF AGREGADO ####

# df de casos
casos <- dataMsal %>% 
            group_by(
                fecha_diagnostico,
                residencia_departamento_id) %>%
            tally() %>% filter(fecha_diagnostico!="" & fecha_diagnostico>="2020-03-01" & residencia_departamento_id!=0)

# df de muertes
muertes <- dataMsal %>% 
              group_by(
                  fecha_fallecimiento,
                  residencia_departamento_id) %>%
                  tally() %>% filter(fecha_fallecimiento!="" & fecha_fallecimiento>="2020-03-01" & residencia_departamento_id!=0)

# formato fechas
colnames(casos)[1] <- "fecha"
colnames(muertes)[1] <- "fecha"
casos$fecha <- as.Date(casos$fecha)
muertes$fecha <- as.Date(muertes$fecha)

# todas las combinaciones posibles dia/depto/casos/muertes 
# para que en el df aparezcan todos los dias/deptos/casos/muertes
combinaciones <- list(unique(c("casos","muertes")),
                      seq(as.Date(min(casos$fecha)),as.Date(max(casos$fecha)),by=1),
                      unique(casos$residencia_departamento_id))

combinaciones <- data.frame(expand.grid(combinaciones))
colnames(combinaciones) <- c("tipo","fecha","residencia_departamento_id")

casos <- merge(combinaciones %>% filter(tipo=="casos"), casos, all.x=TRUE)
muertes <- merge(combinaciones %>% filter(tipo=="muertes"), muertes, all.x=TRUE)
casos$n[is.na(casos$n)==TRUE] <- 0
muertes$n[is.na(muertes$n)==TRUE] <- 0

#### CREA DF FINAL ####

dataMsal <-  merge(casos,muertes, by=c("fecha","residencia_departamento_id"))
dataMsal$tipo.x <- NULL
dataMsal$tipo.y <- NULL
colnames(dataMsal)[3] <- "casos"
colnames(dataMsal)[4] <- "muertes"


dataMsal <- dataMsal %>% arrange(residencia_departamento_id) %>% 
  group_by(residencia_departamento_id) %>% 
  mutate(casos_acumulados=cumsum(casos), 
         muertes_acumuladas=cumsum(muertes))



#### CALCULA R PROMEDIO ULTIMA SEMANA ####
R_semana <- vector()
for (depto in unique(dataMsal$residencia_departamento_id))
{
  res_parametric_si <-
    estimate_R(dataMsal$casos[dataMsal$residencia_departamento_id == depto],
               method = "parametric_si",
               config = make_config(list(mean_si = 2.6,
                                         std_si = 1.5)))
  R_semana <- c(R_semana, c(rep(0, 7), res_parametric_si$R$`Mean(R)`))
}
dataMsal <- cbind(dataMsal,R_semana=R_semana)


#### CALCULA PROMEDIO CASOS Y MUERTES ULTIMA SEMANA ####

dataMsal <- dataMsal %>% 
  group_by(residencia_departamento_id) %>% 
  mutate(promedio_casos_semana = runMean(casos, 7),
         promedio_muertes_semana = runMean(muertes, 7))

#### AGREGA NOMBRES DEPTOS ####

dataMsal <- merge(dataMsal,denom_depto, all.x=TRUE)

##### GRABA RDATA PARA APP #####
save(dataMsal, file="Data/municipios.RData") 


