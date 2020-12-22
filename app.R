library(shiny)
library(dplyr)
library(dygraphs)
library(xts)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(shinythemes)




load("Data/municipios.RData")
load("Mapas/Mapas.Rdata")

# Azul 049
dataMsal<-dataMsal %>% filter(residencia_departamento_id %in% c(63,287,294,301,336,466,469,476,505,616,651,700,707,742,756,547,784,791,826))


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    tags$head(HTML('<link rel="icon", href="ISO-IECS.png", type="image/png" />')),
    titlePanel(windowTitle = "COVID Municipios", title = ""),
    tags$style(".small-box.bg-yellow { background-color: #fff39c !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
    tags$style(".small-box.bg-green { background-color: #a5ff9c !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
    tags$style(".small-box.bg-red { background-color: #ff9c9c !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
    tags$style(".small-box.bg-black { background-color: #ffffff !important; color: #000000 !important; border: 2px solid #317eac; border-radius: 25px;}"),
                # Application title
                fluidRow(
                    column(3, align="center",
                           tags$a(
                               img(src="CIPSlogo.png", height = 88, width = 150),
                               href="https://www.iecs.org.ar/ciips/",
                               target="_blank"
                           )
                    ),
                    column(9,
                           fluidRow(
                             tags$h2("Proyecto COVID Municipios Bonaerenses")
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
                                   "Departamento:",
                                   choices = unique(dataMsal$residencia_departamento_nombre))
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
                    column(1),
                    column(10,
                       dygraphOutput("grafico1")
                   )
                ),
                br(),
                fluidRow(
                  column(12, align="center",
                    downloadButton("download", label = "Descargar datos de este gráfico")
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
        
        x <- xts(dataMsal[dataMsal$residencia_departamento_nombre==input$select_depto,var],dataMsal$fecha[dataMsal$residencia_departamento_nombre==input$select_depto])
  
        #dygraph(x, main = paste0(titulo," - ", input$select_depto)) %>% dySeries("V1", label="Valor día")
        dygraph(x, main = paste0(titulo," - ", input$select_depto)) %>% dySeries(colnames(x), label="Valor día")
    })
    
    
    #Armo un data reactive    
    
    data <- reactive({
        
        data <- dataMsal %>% filter(residencia_departamento_nombre == input$select_depto & fecha== max(dataMsal$fecha))
    
    })
    
    
    #Armo el box con positivos
    
    output$positivos <- renderValueBox({
      
      valueBox(
        value= data() %>% dplyr::select(casos_acumulados),
        subtitle = "Total Positivos",
        color = "black"
      )
      
    })
    
    
    #Armo value box defunciones
    
    output$defunciones <- renderValueBox({
      
      valueBox(
        value= data() %>% dplyr::select(muertes_acumuladas),
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
        valueBox(
            value= round(data() %>% dplyr::select(R_semana),2),
            subtitle = "Número Rt",
            color = getRColor(round(data() %>% dplyr::select(R_semana),2))
        )
    })
    #Armo value box con dias dup
    output$dd <- renderValueBox({
        print(input$select_depto)
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
        valueBox(
        value = round(data() %>% dplyr::select(incidencia_14d),2),
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
      valueBox(
        value= paste(round(data() %>% dplyr::select(positividad_7),2),"%"),
        subtitle = "Positividad de los tests (últ. 7 días)",
        color = getPositividadColor(round(data() %>% dplyr::select(positividad_7),2))
      )
    })
    
    #armo value box de testeos
    
    output$testeos <- renderValueBox({
      valueBox(
        value= round(data() %>% dplyr::select(testeos_7),2),
        subtitle = "Cantidad de testeos (promedio 7 días)",
        color = "black"
      )
    })
    
    #armo value box de poblacion
    
    output$poblacion <- renderValueBox({
      valueBox(
        value= pobdeptos %>% filter(nomdep== input$select_depto) %>% dplyr::select(poblacion),
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
      valueBox(
        value= paste(round(data() %>% dplyr::select(`% cambio`),2),"%"),
        subtitle = "Variación de casos a 7 días",
        color = getVariacionPorcentualColor(round(data() %>% dplyr::select(`% cambio`),2))
      )
    })
      

    #Mapa
    output$mapa1 <- renderLeaflet({
        
    codigo <- min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])
    leaflet(subset(Deptos, depto==codigo),options = leafletOptions(zoomControl = FALSE)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(stroke = T, weight=0.3) %>% 
            addMarkers(~X1, ~X2, popup = ~as.character(""), label = ~as.character(departamen)) %>% setView(-60.096495787602436, -36.59018032729006, zoom = 5)  
        
    })
    output$mapa2 <- renderLeaflet({
        
        codigo <- min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])
        leaflet(subset(Deptos, depto==codigo),options = leafletOptions(zoomControl = FALSE)) %>% 
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(stroke = T, weight=0.3)
        
    })
    
    # reactive para descarga
    descarga <- reactive({
      desc <- dataMsal[dataMsal$residencia_departamento_nombre==input$select_depto,c(2,15,as.numeric(input$select_var))]
      
      
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


