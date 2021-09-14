#Library ----

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(magrittr)
library(xtable)
library(plotly)
library(utf8)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(ggbiplot)
library(forecast)
library(shinymanager)
library(lubridate)
library(dashboardthemes)
library(shinyWidgets)
library(htmlwidgets)

#header ----

header <- dashboardHeader(title = "EmpresaX")

#sidebar ----  

sidebar <- dashboardSidebar({
  sidebarMenu(
    menuItem("Principal", tabName = "Principal", icon = shiny::icon("stream")),
    menuItem("Tipo Clientes", tabName = "Cliente0", icon = shiny::icon("stream")),
    menuItem("Clientes", tabName = "Cliente1", icon = shiny::icon("stream"))
  )
})

#body ----


body <- dashboardBody(
  
  tabItems(
    tabItem(tabName = "Principal",
      fluidRow(
        div(infoBoxOutput("Ventas", width = 3),
            style="font-size:60%"),
        div(infoBoxOutput("Costos", width = 3),
            style="font-size:60%"),
        div(infoBoxOutput("Utilidad", width = 3),
            style="font-size:60%"),
        div(infoBoxOutput("Clientes", width = 3),
            style="font-size:60%"),
        box(title = "Ventas",
            status = "info",
            width = 12,
            closable = FALSE,
            collapsible = TRUE,
            fluidRow(
              column(
                width = 8,
                plotlyOutput(outputId = "Grafica1",
                             height = "300px")),
              column(
                width = 4,
                progressBar(id = "pProducto1", 
                            value = 200, 
                            total = 555,  
                            size = "s",
                            display_pct = TRUE, 
                            title = "Producto 1",
                            status = "danger"),
                progressBar(id = "pProducto2", 
                            value = 220, 
                            total = 250, 
                            size = "s",
                            display_pct = TRUE, 
                            title = "Producto 2"),
                progressBar(id = "pProducto3", 
                            value = 300, 
                            total = 756, 
                            display_pct = TRUE,
                            title = "Producto 3"),
                progressBar(id = "pProducto4", 
                            value = 70, 
                            total = 140, 
                            size = "s",
                            display_pct = TRUE, 
                            title = "Producto 4"))),
            footer = fluidRow(
              column(
                width = 4,
                descriptionBlock(
                  number = "17%", 
                  numberColor = "green",
                  header = "$150,123", 
                  text = "Ventas Mensuales", 
                  rightBorder = TRUE,
                  marginBottom = FALSE
                )
              ),
              column(
                width = 4,
                descriptionBlock(
                  number = "3%", 
                  numberColor = "red", 
                  header = "$33,456", 
                  text = "Ventas Semanales", 
                  rightBorder = TRUE,
                  marginBottom = FALSE
                )
              ),
              column(
                width = 4,
                descriptionBlock(
                  number = "4%", 
                  numberColor = "green", 
                  header = "$6,432", 
                  text = "Ventas Diarias", 
                  rightBorder = TRUE,
                  marginBottom = FALSE
                )
              ))),
        box(
          title = "Ventas",
          status = "teal",
          solidHeader = TRUE,
          width = 8,
          closable = FALSE,
          background = "teal", 
          plotlyOutput("Grafica2", 
                       height = "220px"),
          footer = fluidRow(
            column(
              width = 4,
              knobInput(
                inputId = "VentasOnline",
                readOnly = TRUE,
                skin = "tron",
                label = "Ventas Online",
                value = 40,
                min = 0,
                max = 100,
                displayPrevious = TRUE,
                fgColor = "#00ffff",
                inputColor = "#00ffff",
                width = "150px",
                height = "70%")
            ),
            column(
              width = 4,
              knobInput(
                inputId = "VentasTienda",
                readOnly = TRUE, 
                skin = "tron",
                label = "Ventas en Tienda",
                value = 35,
                min = 0,
                max = 100,
                displayPrevious = TRUE,
                fgColor = "#00ffff",
                inputColor = "#00ffff",
                width = "150px",
                height = "70%")
            ),
            column(
              width = 4,
              knobInput(
                inputId = "VentasMayoreo",
                skin = "tron",
                readOnly = TRUE,
                label = "Ventas en Mayoreo",
                value = 25,
                min = 0,
                max = 100,
                displayPrevious = TRUE,
                fgColor = "#00ffff",
                inputColor = "#00ffff",
                width = "150px",
                height = "70%")
            )
          )
        ),
        box(
          title = "Clientes",
          status = "teal",
          width = 4,
          closable = FALSE,
          plotlyOutput("Grafica3", 
                       height = "220px"),
          footer = navPills(
            navPillsItem(
              selected = TRUE,
              left = "Jose Perez ", 
              color = "green",
              right = "12%" ),
            navPillsItem(
              selected = TRUE,
              left = "Javier Martinez ", 
              color = "red",
              right = "4%" ),
            navPillsItem(
              selected = TRUE,
              left = "Martha Hernandez ", 
              color = "green",
              right = "3%"  )
          )
        )
        
      )
    ),
    tabItem(tabName = "Cliente0",
      fluidRow(
        valueBoxOutput("ClientesNum"),
        valueBoxOutput("ClientesNuevos"),
        valueBoxOutput("ClientesPerdidos"),
        box(
          title = "Edad de los Clientes",
          status = "teal",
          solidHeader = TRUE,
          width = 8,
          collapsible = TRUE,
          closable = FALSE,
          background = "teal", 
          plotlyOutput("Grafica4", 
                       height = "220px"),
          footer = fluidRow(
            descriptionBlock(
              header = "Mas Joven", 
              text = textOutput("minimoEdad"), 
              rightBorder = TRUE,
              marginBottom = FALSE
                            ),
            descriptionBlock(
              header = "Promedio", 
              text = textOutput("PromedioEdad"), 
              rightBorder = TRUE,
              marginBottom = FALSE
                             ),
            descriptionBlock(
              header = "Mas Viejo", 
              text = textOutput("maximoEdad"), 
              rightBorder = TRUE,
              marginBottom = FALSE
                             )
                            )        
                ),
        box(
          title = "Sexo Clientes",
          status = "teal",
          solidHeader = FALSE,
          width = 4,
          collapsible = TRUE,
          closable = FALSE, 
          plotlyOutput("Grafica5", 
                       height = "220px"),
          footer = navPills(
            navPillsItem(
              selected = TRUE,
              left = "Hombre", 
              color = "green",
              right = textOutput("Hombre")),
            navPillsItem(
              selected = TRUE,
              left = "Mujer", 
              color = "red", 
              right = textOutput("Mujer"))
                           )
               ),
        box(
          title = "Estado Civil Clientes",
          status = "teal",
          solidHeader = FALSE,
          width = 6,
          closable = FALSE, 
          plotlyOutput("Grafica6")
               ),
        box(
          title = "¿Como nos conocieron?",
          status = "teal",
          solidHeader = FALSE,
          width = 6,
          closable = FALSE, 
          fluidRow(
            column(
              width = 8,
              plotlyOutput(outputId = "Grafica7",
                           height = "300px")),
            column(
              width = 4,
              boxPad(
                color = "teal",
                descriptionBlock(
                  header = "Facebook", 
                  text = textOutput("facebook"), 
                  rightBorder = FALSE,
                  marginBottom = TRUE
                ),
                descriptionBlock(
                  header = "Instagram", 
                  text = textOutput("instragram"), 
                  rightBorder = FALSE,
                  marginBottom = TRUE
                ),
                descriptionBlock(
                  header = "Amigo", 
                  text = textOutput("amigo"), 
                  rightBorder = FALSE,
                  marginBottom = FALSE
                ),
                descriptionBlock(
                  header = "Radio", 
                  text = textOutput("radio"), 
                  rightBorder = FALSE,
                  marginBottom = FALSE
                ),
                descriptionBlock(
                  header = "Volantes", 
                  text = textOutput("volante"), 
                  rightBorder = FALSE,
                  marginBottom = FALSE
                ),
                descriptionBlock(
                  header = "Otros", 
                  text = textOutput("otros"), 
                  rightBorder = FALSE,
                  marginBottom = FALSE
                                )
                   )
                 )
                 )
               )
              )
        )
    
  )
  
)

#ui ----

ui <- dashboardPage(header, sidebar, body, skin = "blue-light")

#Base de Datos ----

Clientes <- data.frame(readxl::read_excel("C:/Users/0303u/Desktop/simulados.xlsx", sheet = "Clientes"))
Compras <- data.frame(readxl::read_excel("C:/Users/0303u/Desktop/simulados.xlsx", sheet = "Compras"))
ComprasProceso <- data.frame(readxl::read_excel("C:/Users/0303u/Desktop/simulados.xlsx", sheet = "ProcesoCompra"))

#datos -----



dias <- 0:30
fecha <-  as.Date("2021/01/01") 
fecha <- fecha + dias
fecha <- as.Date(fecha)

Ventas21 <-  round(rnorm(n=31,
                   mean = 4500,
                   sd = 1500), 0)


Ventas20 <-  round(rnorm(n=31,
                         mean = 5000,
                         sd = 1000), 0)

datos <- cbind(fecha, Ventas21, Ventas20)
datos <- data.frame(datos)

cliente <- c("Jose Perez", "Javier Martinez", "Martha Hernandez", "Julia Lopez", "Adrian Moreno", "Felipe Perez", "Roman Sanchez", "Otros")
Compras <- c(35,30,22,20,20,15,14,10)

datos2 <- cbind(cliente, Compras)

datos2 <- data.frame(datos2)

#server ----

server <- function(input, output) {
  
  output$Ventas <- renderInfoBox( infoBox(title = "Ventas", 
                                          value = paste0("$", "150,123"), 
                                          icon = icon("cart-arrow-down"),
                                          color = "aqua",
                                          fill = TRUE,
                                          subtitle = progressBar(id = "pVentas", 
                                                                 value = 73, 
                                                                 total = 100, 
                                                                 size = "xs")))
  
  output$Costos <- renderInfoBox( infoBox(title = "Costos", 
                                          value = paste0("$", "99,987"), 
                                          icon = icon("dollar-sign"),
                                          color = "teal",
                                          fill = TRUE,
                                          subtitle = progressBar(id = "pCostos", 
                                                                 value = 80, 
                                                                 total = 100, 
                                                                 size = "xxs")))
  
  output$Utilidad <- renderInfoBox( infoBox(title = "Utilidad", 
                                            value = paste0("$", "50,136"), 
                                            icon = icon("hand-holding-usd"),
                                            color = "olive",
                                            fill = TRUE,
                                            subtitle = progressBar(id = "pUtilidad", 
                                                                   value = 55, 
                                                                   total = 100, 
                                                                   size = "xs")))
  
  output$Clientes <- renderInfoBox( infoBox(title = "Clientes", 
                                            value = paste0("521"), 
                                            icon = icon("users"),
                                            color = "green",
                                            fill = TRUE,
                                            subtitle = progressBar(id = "pClientes", 
                                                                   value = 60, 
                                                                   total = 100, 
                                                                   size = "xs")))
  
  output$Grafica1 <- renderPlotly( 
    
    grafica  <- plot_ly(
                        x = ~datos$fecha, 
                        y = ~datos$Ventas20, 
                        type = 'scatter', 
                        mode = 'lines+markers', 
                        name = 'Ventas 2020',
                        fill = 'tozeroy',
                        fillcolor = 'rgba(0, 128, 53, .5)') %>% 
                add_trace(x = ~datos$fecha, 
                          y = ~datos$Ventas21, 
                          name = 'Ventas 2021', 
                          fillcolor = 'rgba(2, 179, 219 .5)') %>% 
                layout(xaxis = list(title = 'Fecha'),
                       yaxis = list(title = 'Ventas'))
  
    
    )
  
  
  output$Grafica2 <- renderPlotly(
    
    grafica  <- plot_ly(
      x = ~datos$fecha, 
      y = ~datos$Ventas20, 
      type = 'scatter', 
      mode = 'lines+markers',
      name = 'Ventas 2020') %>% 
      layout(xaxis = list(title = 'Fecha'),
             yaxis = list(title = 'Ventas'),
             plot_bgcolor = "rgba(0, 0, 0, 0)" , 
             paper_bgcolor = "rgba(0, 0, 0, 0)")
  )

  output$Grafica3 <- renderPlotly(
    
   grafica <-  plot_ly(labels = ~datos2$cliente, values = ~datos2$Compras) %>% 
               add_pie(hole = 0.6) %>% 
               layout(title = "Compras de Clientes",  
                      showlegend = FALSE,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      plot_bgcolor = "rgba(0, 0, 0, 0)" , 
                      paper_bgcolor = "rgba(0, 0, 0, 0)")
    
  )
  
  output$ClientesNum <- renderValueBox(
    
    valueBox(value =length(Clientes$ID),
             subtitle = "Clientes Nuevos",
             icon = icon("users"))
    
    
  )
  
  }

#ShinyApp ----

shinyApp(ui = ui, server = server)






