#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(ECharts2Shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(highcharter) 
options(highcharter.theme = hc_theme_smpl(tooltip = list(valueDecimals = 2)))


#Datos para grafico de productos mas vendidos
dat <- c(VENTAScopia$PRODUCTO)


#UI

ui = dashboardPage(
    dashboardHeader(title="Dashboard Example"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Ventas por sucursal", tabName = "ventasSucursal", icon=icon("money")),
            menuItem("Productos", tabName = "totalProductos", icon=icon("carrot")),
            menuItem("Empleados", tabName = "empleadosMonto", icon=icon("users"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "ventasSucursal",
                titlePanel(title = h1("Resultado de ventas por sucursal", align = "center", style="align:center;margin-bottom:50px")),
                sidebarLayout(
                    sidebarPanel(
                        selectInput('sucursalcategory', h4('Seleccione una sucursal'), choices=VENTAScopia$SUCURSAL)
                    ),
                    mainPanel(
                      DT::dataTableOutput('sucursalData')
                    )
                )
            ),
            
            tabItem(tabName = "productosSucursal",
                    titlePanel(title = h1("Cantidad de productos vendidos por sucursal", align = "center", style="align:center;margin-bottom:50px")),
                    sidebarLayout(
                    sidebarPanel(
                      selectInput('sucursales', h4('Seleccione una sucursal'), choices=VENTAScopia$SUCURSAL)
                    ),
                    mainPanel(
                      plotOutput('barProductos')
                    )
                  )
            ),
            
            tabItem(tabName = "montoProductos",
                  tags$div(id="montoProducto", style="width:20%;height:200px;"),
                  deliverChart(div_id = "montoProducto"),
                  titlePanel(title = h1("Monto total por productos", align = "center", style="align:center;margin-bottom:50px; margin-top:-100px")),
                  plotOutput('bar')
            ),
            
            tabItem(tabName = "totalProductos",
                    hc <- dat %>%
                      hchart(
                        "pie",
                        name = "Fruit consumption"
                      ),
            ),
            
            tabItem(tabName = "empleadosMonto",
                    titlePanel(title = h1("Empleado con mas ventas", align = "center", style="padding-bottom:50px;padding-top:50px")),
                    dateRangeInput(inputId = "rango", label = h3("Seleccione fecha:"), format = "yyyy-mm-dd"),
                    plotOutput("histo")
            )
        )
    ),
    tags$head(
        tags$img(src='descarga.png',height='80',width='230'),
        tags$style(HTML('
          .skin-blue .main-header .logo{
          background-color: #078090;
          color: #201545;
          }
          .skin-blue .main-header .navbar {
          background-color: #201545;
          }
          .skin-blue .main-sidebar{
          background-color: #201545;
          }
          .skin-blue .sidebar-menu>li.active>a{
          background: #201545;
          }
          '))
    )
)

server = function(input, output) {
    #Muestra una tabla de las ventas por sucursal
    output$sucursalData <- DT::renderDataTable({
        sucursalcategoryFilter <- subset(VENTAScopia, VENTAScopia$SUCURSAL == input$sucursalcategory)
        
    })
    
    #Muestra un grafico de los productos mas vendidos
    renderPieChart(div_id = "test",
                   data = dat,
                   font.size.legend= 14
    )
    
    #Muestra un grafico del monto total por producto
    output$bar <- renderPlot({
      pclass_age_data <- VENTAScopia %>%
        select(PRODUCTO, MONTO) %>%
        group_by(PRODUCTO) %>%
        summarise(MONTO = sum(MONTO))
        ggplot(data=pclass_age_data, aes(x=PRODUCTO, y=MONTO)) +
        geom_bar(stat='identity', fill='steelblue') +
        geom_text(aes(label=MONTO), vjust=-0.3, size=5) +
          theme(axis.text.x = element_text(color = "grey20", size = 15),
                axis.text.y = element_text(color = "grey20", size = 15))
    })
    
    #Muestra la cantidad que se vendio por producto en una sucursal
    output$barProductos <- renderPlot({
      pclass_age_data <- VENTAScopia %>%
        filter(SUCURSAL == input$sucursales) %>%
        select(PRODUCTO, CANTIDAD) %>%
        group_by(PRODUCTO) %>%
        summarise(CANTIDAD = sum(CANTIDAD))
        ggplot(data=pclass_age_data, aes(x=PRODUCTO, y=CANTIDAD)) +
        geom_bar(stat='identity', fill='steelblue') +
        geom_text(aes(label=CANTIDAD), vjust=-0.3, size=5) +
        theme(axis.text.x = element_text(color = "grey20", size = 15),
                axis.text.y = element_text(color = "grey20"), legend.box.margin=unit(50, "cm"))
    })
    
    #Muestra un histograma del monto total de ventas por empleado filtrado por la sucursal
    output$histo <- renderPlot({
      hist_data <- VENTAScopia %>%
        filter(FECHA >= input$rango[1], FECHA <= input$rango[2]) %>%
        select(EMPLEADO, MONTO)
        ggplot(hist_data, aes(x=EMPLEADO)) +
        stat_count(width = 0.5, bins=20, color='#62aca8', fill='#1e9a95') +
        ggtitle('Histograma empleados') +
        theme_void() +
        theme(plot.title=element_text(size=20), axis.text.x = element_text(color = "grey20", size = 15)) +
        geom_text(stat='count', aes(x = EMPLEADO, label = hist_data$EMPLEADO), vjust = -1)
    })
    
    
}

shinyApp(ui, server)


