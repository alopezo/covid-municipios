library(dplyr)
library(tidyverse)
library(zoo)
library(TTR)
library(EpiEstim)
library(stats)

##### FIJAR DIRECTORIO DE TRABAJO ####
#setwd("D:/municipios")

#### DESCARGA DATOS  ####

urlMsal <- 'https://sisa.msal.gov.ar/datos/descargas/covid-19/files/Covid19Casos.csv'
#download.file(urlMsal, "Covid19Casos.csv")
fileSize <- file.info("Covid19Casos.csv")[1,1]

#### IMPORTA DATOS ####
dataMsal_c <-read.csv("Covid19Casos.csv", fileEncoding = "UTF-8") #dejo una version completa para testeos y positividad

#Reasigno valor 0 de sin especificar departamento a 999

dataMsal_c  <- dataMsal_c %>%
    filter(residencia_provincia_id==6) %>%
    mutate(residencia_departamento_id= case_when(residencia_departamento_id < 7 ~ 999,
                                               TRUE ~ as.numeric(residencia_departamento_id)),
           residencia_departamento_nombre= case_when(residencia_departamento_id == 999 ~ "SIN ESPECIFICAR",
                                                     TRUE ~ as.character(residencia_departamento_nombre)))


#Genero el data de confirmados

dataMsal<-dataMsal_c %>% filter(clasificacion_resumen=="Confirmado")


##### TOTALES CRUDOS #####
totales <- 
  dataMsal_c %>% filter(clasificacion_resumen=="Confirmado") %>% 
  group_by(residencia_departamento_nombre) %>%
  dplyr::summarise(confirmados=sum(clasificacion_resumen=="Confirmado"),
                   fallecidos=sum(fallecido=="SI")) %>% 
  union_all(data.frame(residencia_departamento_nombre="Total Buenos Aires",
                       confirmados=nrow(dataMsal_c[dataMsal_c$clasificacion_resumen=="Confirmado",]),
                       fallecidos=nrow(dataMsal_c[dataMsal_c$fallecido=="SI" & dataMsal_c$clasificacion_resumen=="Confirmado",])))



##### COMPLETA FECHA DIAGNOSTICO CON OTRAS FECHAS #####
dataMsal$fecha <- ""
for (i in 1:nrow(dataMsal))
  {
  if (dataMsal$fecha_diagnostico[i]=="" & dataMsal$fecha_inicio_sintomas[i]=="" & dataMsal$fecha_apertura[i]=="") {dataMsal$fecha[i] <- ""} else
  if (dataMsal$fecha_diagnostico[i]=="" & dataMsal$fecha_inicio_sintomas[i]=="" & dataMsal$fecha_apertura[i]!="") {dataMsal$fecha[i] <- dataMsal$fecha_apertura[i]} else
  if (dataMsal$fecha_diagnostico[i]=="" & dataMsal$fecha_inicio_sintomas[i]!="") {dataMsal$fecha[i] <- dataMsal$fecha_inicio_sintomas[i]}
  if (i==1) {print(paste0("IMPUTANDO FECHA..."))}
  if (i==nrow(dataMsal)) {print(paste0("FINALIZADO"))}
  } 

dataMsal$fecha_diagnostico[dataMsal$fecha_diagnostico==""] <- dataMsal$fecha[dataMsal$fecha_diagnostico==""]
dataMsal$fecha <- NULL


##### NOMBRES DE PARTIDOS PARA APP #####
denom_depto <- rbind(c("Total Buenos Aires",0),cbind(unique(dataMsal$residencia_departamento_nombre),unique(dataMsal$residencia_departamento_id)))
                 

denom_depto <- as.data.frame(denom_depto[order(as.numeric(denom_depto[,2])),])

colnames(denom_depto) <- c("residencia_departamento_nombre","residencia_departamento_id")
                            
#### CREA DF AGREGADO ####

# df de casos
casos <- dataMsal %>% 
            group_by(
                fecha_diagnostico,
                residencia_departamento_id) %>%
            tally() %>% filter(fecha_diagnostico!="" & fecha_diagnostico>="2020-03-01" & residencia_departamento_id!=999) %>%
  ungroup() %>%
  bind_rows(group_by(.,fecha_diagnostico)%>%
            summarise(n= sum(n))%>%
            mutate(residencia_departamento_id = 0)) %>%
  arrange(residencia_departamento_id,fecha_diagnostico)
              


# df de muertes
muertes <- dataMsal %>% 
              group_by(
                  fecha_fallecimiento,
                  residencia_departamento_id) %>%
                  tally() %>% filter(fecha_fallecimiento!="" & fecha_fallecimiento>="2020-03-01" & residencia_departamento_id!=999) %>%
  ungroup() %>%
  bind_rows(group_by(.,fecha_fallecimiento)%>%
              summarise(n= sum(n))%>%
              mutate(residencia_departamento_id = 0)) %>%
  arrange(residencia_departamento_id,fecha_fallecimiento)



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
  data <- data.frame(x=dataMsal$casos[dataMsal$residencia_departamento_id == depto])
  #data <- predict(loess(data$x~seq(1,nrow(data))),span=.5)
  #data[data<0] <- 0
  
  res_parametric_si <-
    estimate_R(data,
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


#### CALCULA TOTAL CASOS Y MUERTES ULTIMOS 14 DIAS ####

dataMsal <- dataMsal %>% 
  group_by(residencia_departamento_id) %>% 
  mutate(total_casos_14d = runMean(casos, 14)*14,
         total_muertes_14d = runMean(muertes, 14)*14)

#### CALCULA INCIDENCIA CASOS Y MUERTES ULTIMOS 14 DIAS ####
load("Data/poblacion.RData")

#Genero poblacion total de provincias


pobdeptos <- pobdeptos %>%
  bind_rows(pobdeptos %>%
            summarise(poblacion= sum(poblacion))%>%
            mutate(coddep= 0, nomdep= "Total Buenos Aires"))%>%
  arrange(coddep)

for (depto in (pobdeptos$coddep))
{dataMsal$poblacion_depto[dataMsal$residencia_departamento_id==depto] <- pobdeptos$poblacion[pobdeptos$coddep==depto]}
dataMsal$incidencia_14d <- dataMsal$total_casos_14d/dataMsal$poblacion_depto*100000
dataMsal$mortalidad_14d <- dataMsal$total_muertes_14d/dataMsal$poblacion_depto*100000



#### CALCULA DIAS DE DUPLICACION ####
source("Modulos/modulos.R", encoding = "UTF-8")
fecha <- vector()
dd <- vector()
depto <- vector()

for (departamento in unique(dataMsal$residencia_departamento_id))
{
  depto <- c(depto, departamento)
  fecha <- c(as.Date(fecha), max(dataMsal$fecha))
  dd <- c(dd, get_dias_dupl(dataMsal, max(dataMsal$fecha), 7, departamento)[1])
}

diasDuplicacion <- data.frame(residencia_depto_if=depto, fecha=fecha, dias_duplicacion=dd)

#### AGREGA NOMBRES DEPTOS ####

dataMsal <- merge(dataMsal,denom_depto, all.x=TRUE)


##### Agrego el indicador de cambio que compara semana actual contra semana anterior####

column_temp <- c("retraso","cum_rolling7","cum_rolling14")

dataMsal <- dataMsal %>%
  group_by(residencia_departamento_id) %>%
  mutate(retraso = lag(casos, n= 7))%>%
  mutate(cum_rolling7= rollapplyr(casos, width = 7, FUN = sum, partial = TRUE))%>%
  mutate(cum_rolling14= rollapplyr(retraso, width = 7, FUN = sum, partial = TRUE)) %>%
  mutate(`% cambio`= round((cum_rolling7-cum_rolling14)*100/cum_rolling14,2)) %>%
  select(-one_of(column_temp)) %>%
  ungroup()

##Genero testeos e indicador de positividad

#Convierto las variables fecha en Date

dataMsal_c$fecha_inicio_sintomas <- as.Date(dataMsal_c$fecha_inicio_sintomas,format = "%Y-%m-%d")
dataMsal_c$fecha_diagnostico <- as.Date(dataMsal_c$fecha_diagnostico,format = "%Y-%m-%d")
dataMsal_c$fecha_apertura <- as.Date(dataMsal_c$fecha_apertura,format = "%Y-%m-%d")

#Genero los indicadores


testeosyposit <- dataMsal_c %>%
  filter(residencia_provincia_id== 6 & residencia_departamento_id != 999) %>%
  mutate(fecha= coalesce(fecha_diagnostico,fecha_inicio_sintomas,fecha_apertura))%>%
  group_by(fecha,clasificacion,residencia_departamento_id)%>%
  summarise(n= n()) %>%
  bind_rows(group_by(.,fecha,clasificacion)%>%
              summarise(n= sum(n))%>%
              mutate(residencia_departamento_id= 0))%>%
  ungroup()%>%
  group_by(residencia_departamento_id,fecha)%>%
  summarise(testeos= sum(n[grepl("criterio clinico-epidemiológico", clasificacion)== "FALSE"]),
            conf_lab = sum(n[grepl("confirmado por laboratorio", clasificacion)== "TRUE"]),
            positividad= round(conf_lab*100/testeos,2)) %>%
  select(- conf_lab)



dataMsal <- dataMsal %>%
       left_join(testeosyposit, by= c("residencia_departamento_id","fecha"))


rm(dataMsal_c)
#Genero el promedio de los últimos 7 días en testeos y positividad

dataMsal <- dataMsal %>% 
       mutate(testeos= case_when(is.na(testeos)== TRUE ~ 0, TRUE ~ as.numeric(testeos)),
              positividad = case_when(is.na(positividad)== TRUE ~ 0, TRUE ~ as.numeric(positividad)))%>%
       mutate(testeos_7= round(runMean(testeos,7),2),
              positividad_7= round(runMean(positividad,7),2)) %>%
  select(-testeos,-positividad) %>%
  as.data.frame()

     


##### GRABA RDATA PARA APP #####
save.image(file="Data/municipios.RData") 







