get_dias_dupl <- function(data, hoy, ventana, depto){
  # data
  df <- data %>% 
    dplyr::select(fecha,casos_acumulados, residencia_departamento_id) %>% 
    mutate(fecha = as.Date(fecha)) %>% 
    filter(fecha %in% seq(as.Date(hoy)-ventana, as.Date(hoy)-1, by="day") & residencia_departamento_id==depto) %>%
    right_join(data.frame(fecha=seq(as.Date(hoy)-ventana, as.Date(hoy)-1, by="day"))) %>% 
    fill(casos_acumulados, .direction = "up") %>%
    fill(casos_acumulados, .direction = "down") %>% 
    arrange(fecha) %>% 
    mutate(dia = 1:ventana,
           acumulados = casos_acumulados, # casos acumulados
           log.acumulados = log(acumulados)) %>% 
    dplyr::select(dia, acumulados, log.acumulados)
  
  # reg lineal de crecim exp
  reglin <- lm(df, formula = log.acumulados ~ dia)
  B1 = as.numeric(reglin$coefficients[2])
  errorB1 <- summary(reglin)$coef[2, 2]
  
  # media e ICs 95%
  diasDup = log(2) / c(B1, B1-1.96*errorB1, B1+1.96*errorB1) 
  
  return(diasDup)
}


