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

dataMsal<-dataMsal %>% filter(residencia_departamento_id %in% c(63,287,294,301,466,469,476,505,616,707,742,756,547,784,791,826))


ui <- fluidPage(theme = shinytheme("cerulean"),
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
                           )
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
                        valueBoxOutput("positivos", width = 3),
                        valueBoxOutput("defunciones", width = 3),
                        valueBoxOutput("r", width = 3),
                        valueBoxOutput("dd", width = 3)
                    )
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
                                              "Muertes por 100.000 habitantes (últimos 14 días)."=14
                                          ))
                    ),
                ),
                fluidRow(
                    column(1),
                    column(10,
                       dygraphOutput("grafico1")
                   )
                ),
                fluidRow(
                    column(1),
                    column(10,
                       leafletOutput("mapa")
                    )
                )
)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    #input=list(select_var=5,select_depto="Avellaneda")
    
    ##### GRAFICOS #####    
    output$grafico1 <- renderDygraph({
        var <- as.numeric(input$select_var)
        if (var==5) {titulo <- "Casos acumulados"} else
        if (var==6) {titulo <- "Defunciones acumuladas"} else
        if (var==7) {titulo <- "Rt Diario"} else
        if (var==8) {titulo <- "Casos diarios (promedio 7 días)"} else
        if (var==9) {titulo <- "Defunciones diarias (promedio 7 días)"} else
        if (var== 3) {titulo <- "Casos diarios"}
        if (var== 4) {titulo <- "Defunciones diarias"}
        if (var== 13) {titulo <- "Casos por 100.000 habitantes (últimos 14 días)"}
        if (var== 14) {titulo <- "Muertes por 100.000 habitantes (últimos 14 días)"}
        {NULL}
        
        x <- xts(dataMsal[dataMsal$residencia_departamento_nombre==input$select_depto,var],dataMsal$fecha[dataMsal$residencia_departamento_nombre==input$select_depto])
        dygraph(x, main = paste0(titulo," - ", input$select_depto)) %>%
            dySeries("V1", label="Valor día")
        
    })
    
    
    #Armo un data reactive    
    
    data <- reactive({
        
        data <- dataMsal %>% filter(residencia_departamento_nombre == input$select_depto & fecha== max(dataMsal$fecha))
    })
    
    #Armo el box con positivos
    
    output$positivos <- renderValueBox({
        
        valueBox(
            value= data() %>% dplyr::select(casos_acumulados),
            subtitle = paste("Total Positivos al: ",substring(max(data()$fecha),9,10),substring(max(data()$fecha),5,8),substring(max(data()$fecha),1,4),sep=""),
            color = "green"
        )
    })
    
    #Armo value box defunciones
    
    output$defunciones <- renderValueBox({
        
        valueBox(
            value= data() %>% dplyr::select(muertes_acumuladas),
            subtitle = paste("Total defunciones al: ",substring(max(data()$fecha),9,10),substring(max(data()$fecha),5,8),substring(max(data()$fecha),1,4),sep="")
        )
    })
    
    #Armo value box con el R
    
    output$r <- renderValueBox({
        
        valueBox(
            value= round(data() %>% dplyr::select(R_semana),2),
            subtitle = paste("Indicador R al: ",substring(max(data()$fecha),9,10),substring(max(data()$fecha),5,8),substring(max(data()$fecha),1,4),sep="")
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
            subtitle = paste("Días de duplicación al: ",substring(max(data()$fecha),9,10),substring(max(data()$fecha),5,8),substring(max(data()$fecha),1,4),sep="")
        )
    })

    #Mapa
    output$mapa <- renderLeaflet({
        
    codigo <- min(dataMsal$residencia_departamento_id[dataMsal$residencia_departamento_nombre==input$select_depto])
    leaflet(subset(Deptos, depto==codigo)) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(stroke = T, weight=0.3)
        
    })
}




# Run the application 
shinyApp(ui = ui, server = server)


