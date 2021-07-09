library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

## Personalización --------------------------------

library(fresh)
# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)

## Fin Personalización ------------------------ --


shinyApp(
  ui = dashboardPage(
    options = list(sidebarExpandOnHover = TRUE), # Sidebar hover action
    header = dashboardHeader(title = "Dashboard"), # Título
    
    ## Barra lateral
    sidebar = dashboardSidebar(
      menuItem("Inicio", tabName = "inicio", icon = icon("home")),
      menuItem("Alerta 1", tabName = "alerta1", icon = icon("alert")),
      minified = TRUE, collapsed = FALSE),
    
    ## Cuerpo de la app
    body = dashboardBody(
      
      # Usar tema personalizado
      use_theme(mytheme),
      
      # Definir contenidos de la app
      tabItems(
        ## ITEM: Inicio
        tabItem("inicio", "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras tempor est in aliquet egestas. Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Cras tristique, nisl vel fringilla efficitur, nulla nibh malesuada risus, eget congue magna orci vel ligula. Aliquam sit amet augue tortor. Integer egestas placerat tortor sit amet aliquet. Quisque luctus finibus orci vitae luctus. In egestas nec turpis nec consequat. Morbi orci augue, pulvinar dignissim sapien a, placerat bibendum est. Curabitur tristique, risus non congue mattis, neque enim pharetra enim, quis sagittis odio turpis non nisi. Morbi finibus purus magna, non consectetur turpis convallis sit amet. Praesent ultricies ipsum vel eros imperdiet finibus. Mauris et volutpat nunc, ut pretium lorem. Nunc venenatis interdum nulla, sit amet suscipit turpis congue sollicitudin.

Integer vitae volutpat quam, id eleifend velit. Nulla lectus leo, congue eget molestie id, tempor et diam. Donec pretium nunc sit amet laoreet facilisis. Etiam a nulla rhoncus, vestibulum massa non, ultricies nunc. Mauris tellus nunc, finibus eget est a, porta hendrerit tortor. Maecenas sapien dui, accumsan a congue accumsan, scelerisque vitae justo. Aliquam imperdiet in felis et egestas.

Ut sodales porttitor lacus, eget cursus magna eleifend ac. Pellentesque quis risus molestie, interdum dolor vitae, blandit sapien. Integer massa libero, accumsan sed ornare quis, elementum ut sem. Aenean congue orci eget volutpat finibus. Nam eget tincidunt mi. In molestie nunc eu egestas interdum. Vestibulum aliquet posuere ligula, nec pellentesque elit euismod non. Aliquam tincidunt ligula ac condimentum tempus. Nulla vestibulum dictum odio, ut fermentum nulla semper eleifend. Sed vel molestie nunc, vel fringilla sapien. In nec suscipit sapien, ut commodo urna. Phasellus scelerisque condimentum quam, condimentum imperdiet purus."),
        
        ## ITEM: Alerta 1
        tabItem("alerta1",
                fluidRow(
                  # selectInput(inputId = "region", label = "Seleccione Región", choices = Regiones),
                  column(6,selectInput(inputId = "cliente", label = "Seleccione Cliente", choices = UTMClientes)),
                  column(6,selectInput(inputId = "producto", label = "Seleccione Producto", choices = NULL))
                ),
                # # fluidRow(
                # #     infoBox(title = "Alerta 1", 
                # #             subtitle = "Indicador de alerta", 
                # #             value = 10, 
                # #             icon = icon("exclamation-circle"),
                # #             color="red"),
                # #     infoBox(title = "Alerta 2", 
                # #             subtitle = "Indicador de alerta", 
                # #             value = 10, 
                # #             icon = icon("exclamation-circle"),
                # #             color="yellow"),
                # #     infoBox(title = "Alerta 3", 
                # #             subtitle = "Indicador de alerta", 
                # #             value = 10, 
                # #             icon = icon("exclamation-circle"),
                # #             color="green")
                # # ),
                fluidRow(
                  # box(width = 2, actionButton("count", "Count")),
                  # infoBoxOutput("ibox"),
                  valueBoxOutput("vbox")
                )
                
        )
      )
      
    ),
    controlbar = dashboardControlbar(),
    title = "DashboardPage"
  ),
  server = function(input, output, session){ 
    
    observeEvent(input$cliente,{
      producto_list = with(DF_AUX,
                           Ad_name[Account==input$cliente]) %>% 
        unique() %>% sort()
      updateSelectInput(session,"producto",
                        choices = producto_list)
    })
    
    # # output$ibox <- renderInfoBox({
    # #     infoBox(
    # #         "Title",
    # #         input$count,
    # #         icon = icon("credit-card")
    # #     )
    # # })
    
    
    output$vbox <- renderValueBox({
      
      error = ifelse(with(TablaAux, Suma[Account == input$cliente] > 0), 1, 0)
      n_error = with(TablaAux, Suma[Account == input$cliente])   
      
      valueBox(
        subtitle = "Indicador error UTM" ,
        value = ifelse(error == 1 ,paste("Existen ",n_error," errores."),"No Error"),
        icon = icon("exclamation-circle"),
        color = ifelse(error == 1 ,"red","green")
      )
    })
  }
  
  
  
)