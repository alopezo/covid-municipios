library(shiny)
library(dplyr)
library(dygraphs)
library(xts)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(shinythemes)
library(reshape2)
library(rlist)
library(ggplot2)
library(scales)
library(shinyWidgets)

load("Data/municipios.RData")
load("Mapas/Mapas.Rdata")



# Azul 049
# Gral Belgrano 301
#dataMsal<-dataMsal %>% filter(residencia_departamento_id %in% c(63,294,476,505,547,616,651,700,707,756,784,791,826,301))


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    tags$head(HTML('<link rel="icon", href="ISO-IECS.png", type="image/png" />')),
    titlePanel(windowTitle = "COVID-CIIPS Argentina", title = ""),
    tags$style(".small-box.bg-yellow { background-color: #fff39c !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
    tags$style(".small-box.bg-green { background-color: #a5ff9c !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
    tags$style(".small-box.bg-red { background-color: #ff9c9c !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
    tags$style(".small-box.bg-black { background-color: #ffffff !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
                # Application title
                fluidRow(
                    column(3,
                           fluidRow(
                             column(12,
                                    tags$a(
                                      img(src="iecslogo.png", height = 66, width = 200),
                                      href="https://www.iecs.org.ar",
                                      target="_blank"
                                    )
                             )
                           ),
                           fluidRow(
                             column(12,
                                    tags$a(
                                      img(src="CIPSlogo.png", height = 117, width = 200),
                                      href="https://www.iecs.org.ar/ciips/",
                                      target="_blank"
                                    )
                             )
                           ),
                     ),
                    column(9,
                           fluidRow(
                             tags$h2("Tablero de control dinámico COVID-CIIPS Argentina")
                           ),
                           fluidRow(             
                             p("Datos procesados a partir de información anonimizada del Sistema Nacional de Vigilancia en Salud (SNVS - SISA)"),
                           ),
                           fluidRow(column(9, 
                             p(paste("Datos actualizados al: ",substring(max(dataMsal$fecha),9,10),substring(max(dataMsal$fecha),5,8),substring(max(dataMsal$fecha),1,4),sep="")),
                             align= "center"
                           ))
                    
                )
                ),
                hr(),
                fluidRow(
                    column(12, align="center",
                        selectizeInput("select_depto",
                                   "Jurisdicción:",
                                   choices = unique(dataMsal$residencia_departamento_nombre)[unique(dataMsal$residencia_departamento_nombre)!="SIN ESPECIFICAR"]
                                   )
                    ),
                ),
                fluidRow(
                    column(12, align="center",
                        valueBoxOutput("poblacion", width = 3),
                        valueBoxOutput("positivos", width = 3),
                        valueBoxOutput("defunciones", width = 3),
                        valueBoxOutput("testeos", width = 3)
                       
                        
                   
                    )
                ),
                br(),
                fluidRow(
                    column(12, align="center",
                    valueBoxOutput("tasa", width = 3),
                    valueBoxOutput("r", width = 3),
                    valueBoxOutput("variacion_casos", width = 3),
                    valueBoxOutput("positividad", width = 3)
                ),
                br(),
                ),
                fluidRow(
                    column(12, align="center",
                           selectizeInput("select_var",
                                          "Variable:",
                                          choices = list(
                                              "Casos diarios" = 3,
                                              "Casos diarios (promedio 7 días)"=8,
                                              "Casos acumulados"=5,
                                              "Rt Diario"=7,
                                              "Defunciones diarias"= 4,
                                              "Defunciones diarias (promedio 7 días)"=9,
                                              "Defunciones acumuladas"=6,
                                              "Casos por 100.000 habitantes (últimos 14 días)"=13,
                                              "Muertes por 100.000 habitantes (últimos 14 días)."=14,
                                              "% de cambio casos nuevos ultima semana vs. semana previa"=16,
                                              "Cantidad de testeos (promedio 7 días)"=17,
                                              "Indice de positividad (promedio 7 días)"= 18
                                          ))
                    ),
                ),
                fluidRow(
                    column(2,
                           pickerInput(
                             inputId = "comparar",
                             label = "Seleccionar comparación", 
                             choices = unique(dataMsal$residencia_departamento_nombre),
                             multiple = TRUE,
                             options = list('none-selected-text'= "Jurisdicción")
                           )),
                    column(9,
                       dygraphOutput("grafico1")
                   )
                ),
                br(),
                fluidRow(
                  column(12, align="center",
                         downloadButton("download", label = "Descargar datos"),
                         downloadButton("btn_download", "Descargar gráfico")
                  )
                ),
                br(),
                fluidRow(
                    column(1),
                    column(5,
                       leafletOutput("mapa1")),
                    column(5,
                          leafletOutput("mapa2")
                    )
                ),
                br(),
                hr(),
                fluidRow( class = "text-center",
                  p(style = "margin-bottom: 2px; font-size: 12px;  color: #67c97c;", 
                    HTML(paste0("<i>Esta herramienta fue desarrollada por el <a href=\"https://www.iecs.org.ar/ciips/\" target=\"_blank\">CIIPS</a> &copy;2020.
                        <br>Contacto:</i> <a href=\"mailto:ciips@iecs.org.ar?
                        subject='Modelo COVID-19'\">ciips@iecs.org.ar</a>")
                       )
                  )
                ),
                br()
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
  
  observe({
   
    u_choices <- c(unique(dataMsal$residencia_departamento_nombre[dataMsal$residencia_departamento_nombre != input$select_depto &
                                                                  dataMsal$residencia_departamento_nombre != "SIN ESPECIFICAR" ]))
    updatePickerInput(session,"comparar","Seleccionar comparación",choices=u_choices, selected="Seleccionar comparación")

  })
    
    #input=list(select_var=5,select_depto="Avellaneda")
  
    
    ##### GRAFICOS #####    
    
  output$grafico1 <- renderDygraph({
        var=as.numeric(input$select_var)
        if (var==5) {titulo <- "Casos acumulados"} else
        if (var==6) {titulo <- "Defunciones acumuladas"} else
        if (var==7) {titulo <- "Rt Diario"} else
        if (var==8) {titulo <- "Casos diarios (promedio 7 días)"} else
        if (var==9) {titulo <- "Defunciones diarias (promedio 7 días)"} else
        if (var== 3) {titulo <- "Casos diarios"}
        if (var== 4) {titulo <- "Defunciones diarias"}
        if (var== 13) {titulo <- "Casos por 100.000 habitantes (últimos 14 días)"}
        if (var== 14) {titulo <- "Muertes por 100.000 habitantes (últimos 14 días)"}
        if (var== 16) {titulo <- "% de cambio casos nuevos ultima semana vs. semana previa"}
        if (var== 17) {titulo <- "Cantidad de testeos (promedio 7 días)"}
        if (var== 18) {titulo <- "Indice de positividad (promedio 7 días)"}
        {NULL}
        # browser()
        compara_con <- input$comparar
        
        
        
        data <- as.data.frame(cbind(dataMsal$residencia_departamento_nombre[dataMsal$residencia_departamento_nombre %in% c(input$select_depto,compara_con)], as.character(dataMsal$fecha[dataMsal$residencia_departamento_nombre %in% c(input$select_depto,compara_con)]),dataMsal[dataMsal$residencia_departamento_nombre %in% c(input$select_depto,compara_con),var]))
        
        # data$V2 <- as.Date(data$V2)
        
        data$V3 <- as.numeric(as.character(data$V3))
        colnames(data) <- c('depto','fecha','val')
        
  
        data <- dcast(data = data, formula = fecha ~ depto, fun.aggregate = mean, value.var = "val")
        
        data <- xts(data[,-1], order.by=as.Date(data[,1]))
   
        #dygraph(x, main = paste0(titulo," - ", input$select_depto)) %>% dySeries("V1", label="Valor día")
        if (is.null(colnames(data))==T)
        {colnames(data) <- input$select_depto} 
        
        if (length(colnames(data))>1)
        {
          colnames(data) <- colnames(data)
          seleccion_compara <- colnames(data)[colnames(data)!=input$select_depto]
          series <- vector()
          for (s in 1:length(seleccion_compara))
          {series <- c(series,paste0("dySeries(name=","'",seleccion_compara[as.numeric(s)],"'",",strokePattern = 'dotted')")) 
          }
        }
        dg <- dygraph(data=data, main = paste0(titulo,' - ',input$select_depto)) %>% dySeries(name=input$select_depto, label=input$select_depto, color = 'black', strokeWidth = 1.8) 
        if (exists("series")==T)
        {parte_dg2 <- paste(series, collapse= " %>% ")
        }
        if (exists("series")==T)
        {
          eval(parse(text=paste0("dg %>% ",parte_dg2)))
        }
        else {dg}
    })
    
  
#Armo un gráfico reactivo para poder descargarlo como png
  
grafico <- reactive({
    
    var=as.numeric(input$select_var)
    if (var==5) {titulo <- "Casos acumulados"} else
      if (var==6) {titulo <- "Defunciones acumuladas"} else
        if (var==7) {titulo <- "Rt Diario"} else
          if (var==8) {titulo <- "Casos diarios (promedio 7 días)"} else
            if (var==9) {titulo <- "Defunciones diarias (promedio 7 días)"} else
              if (var== 3) {titulo <- "Casos diarios"}
    if (var== 4) {titulo <- "Defunciones diarias"}
    if (var== 13) {titulo <- "Casos por 100.000 habitantes (últimos 14 días)"}
    if (var== 14) {titulo <- "Muertes por 100.000 habitantes (últimos 14 días)"}
    if (var== 16) {titulo <- "% de cambio casos nuevos ultima semana vs. semana previa"}
    if (var== 17) {titulo <- "Cantidad de testeos (promedio 7 días)"}
    if (var== 18) {titulo <- "Indice de positividad (promedio 7 días)"}
    {NULL}
    
    
    
    data <- descarga()
    
    colnames(data) <- c('fecha','depto','val')  
    
    
    n <- length(unique(data$depto))-1
    
    
    if(n > 0){
      pal <- c(rainbow(n))
    } else{
      pal <- "black" 
    }
    
    theme_tablero <- function () { 
      theme_bw() %+replace% 
        theme(
          panel.border = element_blank(),
          plot.title = element_text(face= "bold",size = 12),
          legend.title = element_text(face= "bold",size = 12)
          
        )
    } 
    
    
    graf <- ggplot()+
      geom_line(data= subset(data,data$depto == input$select_depto),aes_string(x= "fecha", y= "val"),color= "black")+
      geom_line(data= subset(data,data$depto != input$select_depto),aes_string(x= "fecha", y= "val",color= "depto"), linetype= "dotted")+
      labs(title = paste(titulo,input$select_depto,sep="-"),
           x= "Fecha", y= "Valor", color= "Jurisdicción")+
      scale_color_manual(values = pal)+
      theme_tablero()
    
    
    
  })
  

###### Descarga Dygraphs como PNG
  
  output$btn_download <- downloadHandler(
    filename = function() {
      paste("grafico","png",sep = ".")
      
    },
    content = function(file) {
      ggsave(file,plot= grafico(), device= "png",dpi= 120,width = 20,height = 12, units = "cm")
      
    }
  )
  
  
#Armo un data reactive    
    
    data <- reactive({
        
        data <- dataMsal %>% filter(residencia_departamento_nombre == input$select_depto & fecha== max(dataMsal$fecha))
    
    })
    
    dataR <- reactive({
      retrasoR <- 4
      dataR <- dataMsal %>% filter(residencia_departamento_nombre == input$select_depto & fecha== max(dataMsal$fecha)-retrasoR)
      
    })
    
    #Armo el box con positivos
    
    output$positivos <- renderValueBox({
      valor <- totales$confirmados[totales$residencia_departamento_nombre==input$select_depto]
      valueBox(
        value= format(valor, big.mark='.', decimal.mark = ','),
        subtitle = "Total Positivos",
        color = "black"
      )
      
    })
    
    
    #Armo value box defunciones
    
    output$defunciones <- renderValueBox({
      valor <- totales$fallecidos[totales$residencia_departamento_nombre==input$select_depto]
      valueBox(
        value=format(valor, big.mark = '.', decimal.mark = ','),
        subtitle = "Total defunciones",
        color = "black"
      )
    })
    
    #Armo value box con el R
    getRColor <- function(valorR) {
      if (as.double(valorR) > as.double(1.5)) { 
        return("red") 
      } else if (as.double(valorR) >= as.double(1) & as.double(valorR) <= as.double(1.5)) { 
        return("yellow")
      } else {
        return("green") 
      }
    }
    output$r <- renderValueBox({
      valor <-  round(dataR() %>% dplyr::select(R_semana),2)  
      valueBox(
            value= format(valor, big.mark = '.', decimal.mark = ','),
            subtitle = "Número Rt",
            color = getRColor(round(dataR() %>% dplyr::select(R_semana),2))
        )
    })
    #Armo value box con dias dup
    output$dd <- renderValueBox({
        
        valueBox(
            value= 
              if(is.na(round(diasDuplicacion$dias_duplicacion[min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])],2))
                 ==TRUE) {">300"} else
            if(is.na(round(diasDuplicacion$dias_duplicacion[min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])],2))
                  ==FALSE &
               is.na(round(diasDuplicacion$dias_duplicacion[min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])],2))
               > 300
               ) {">300"}  else {round(diasDuplicacion$dias_duplicacion[min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])],2)}
                
              ,
            subtitle = "Días de duplicación"
        )
    })
    
    #armo value box de tasa
    getTasaColor <- function(valor, poblacion) {
      if (as.double(poblacion) > as.double(50000)) {
        if (as.double(valor) > as.double(200)) { 
          return("red") 
        } else if (as.double(valor) >= as.double(50) & as.double(valor) <= as.double(200)) { 
          return("yellow")
        } else {
          return("green") 
        }
      } else {
        if (as.double(valor) > as.double(150)) { 
          return("red") 
        } else if (as.double(valor) >= as.double(30) & as.double(valor) <= as.double(150)) { 
          return("yellow")
        } else {
          return("green") 
        }
      }
    }
    output$tasa <- renderValueBox({
      valor <- round(data() %>% dplyr::select(incidencia_14d),1)  
      valueBox(
        value = format(valor,big.mark = '.', decimal.mark = ','),
        subtitle = "Tasa por 100.000 hab. (últ. 14 días)",
        color = getTasaColor(round(data() %>% dplyr::select(incidencia_14d),2),pobdeptos %>% filter(nomdep== input$select_depto) %>% dplyr::select(poblacion))
      )
    })
    
    #armo value box de positividad
    getPositividadColor <- function(valor) {
      if (as.double(valor) > as.double(30)) { 
        return("red") 
      } else if (as.double(valor) >= as.double(20) & as.double(valor) <= as.double(30)) { 
        return("yellow")
      } else {
        return("green") 
      }
    }
    output$positividad <- renderValueBox({
      valor <- round(data() %>% dplyr::select(positividad_7),1)
      valueBox(
        value= paste(format(valor, big.mark = '.', decimal.mark = ','),"%"),
        subtitle = "Positividad de los tests (últ. 7 días)",
        color = getPositividadColor(round(data() %>% dplyr::select(positividad_7),2))
      )
    })
    
    #armo value box de testeos
    
    output$testeos <- renderValueBox({
      valor <- data() %>% dplyr::select(testeos_7)
      valueBox(
        value= format(valor,big.mark = '.', decimal.mark = ','),
        subtitle = "Cantidad de testeos (promedio 7 días)",
        color = "black"
      )
    })
    
    #armo value box de poblacion
    
    output$poblacion <- renderValueBox({
      valor <- pobdeptos %>% filter(nomdep== input$select_depto) %>% dplyr::select(poblacion)
      valueBox(
        value=format(valor, big.mark = '.', decimal.mark = ','),
        subtitle = "Poblacion estimada",
        color = "black"
      )
    })
    
    #Armo value box de % de cambio de casos ult semana vs semana previa
    getVariacionPorcentualColor <- function(valor) {
      if (as.double(valor) > as.double(10)) { 
        return("red") 
      } else if (as.double(valor) >= as.double(-10) & as.double(valor) <= as.double(10)) { 
        return("yellow")
      } else {
        return("green") 
      }
    }
    output$variacion_casos <- renderValueBox({
      valor <- round(data() %>% dplyr::select(`% cambio`),1)
      valueBox(
        value= paste(format(valor, big.mark = '.', decimal.mark = ','),"%"),
        subtitle = "Variación de casos a 7 días",
        color = getVariacionPorcentualColor(round(data() %>% dplyr::select(`% cambio`),2))
      )
    })
      

    #Mapa
    output$mapa1 <- renderLeaflet({
        
    codigo <- min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])
    
    if (codigo==0)
    {leaflet(Arg,options = leafletOptions(zoomControl = FALSE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(stroke = T, weight=0.3) %>% setView(-63.8, -36.2, zoom = 2)   }
    else
    {
      leaflet(subset(Deptos, depto==codigo),options = leafletOptions(zoomControl = FALSE)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(stroke = T, weight=0.3) %>% 
            addMarkers(~X1, ~X2, popup = ~as.character(""), label = ~as.character(denom_depto$residencia_departamento_nombre[denom_depto$residencia_departamento_id==codigo])) %>% setView(-63.8, -36.2, zoom = 3)  
    }   
    })
    
    
    output$mapa2 <- renderLeaflet({
        
        codigo <- min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])
        if (codigo==0)
        {leaflet(Arg,options = leafletOptions(zoomControl = FALSE)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(stroke = T, weight=0.3) }
        else
        {
        leaflet(subset(Deptos, depto==codigo),options = leafletOptions(zoomControl = FALSE)) %>% 
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(stroke = T, weight=0.3)
        }
    })
    
    # reactive para descarga
    descarga <- reactive({
      
      desc <- dataMsal[dataMsal$residencia_departamento_nombre %in% c(input$select_depto, input$comparar),c(2,15,as.numeric(input$select_var))]
      
    })
    
    output$download <- downloadHandler(
      
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv2(descarga(), file, row.names = F)
      }
    )
     
    
}






# Run the application 
shinyApp(ui = ui, server = server)


                                                                                                                  
