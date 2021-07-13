library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(DT)


## Funciones dinámicas ----------------------------

Icon_Account = function(i) {
  if(with(TablaAux, Suma[Account == UTMClientes[i]] > 0)){
    tags$i(class = "fa fa-exclamation-triangle", style = "color: red")
  } else {
    tags$i(class = "fa fa-exclamation-triangle", style = "color: green")
  }
}

Vbox_UTM = function(i){
  error = ifelse(with(TablaAux, Suma[Account == UTMClientes[i]] > 0), 1, 0)
  n_error = with(TablaAux, Suma[Account == UTMClientes[i]])   
  
  valueBox(
    subtitle = "Indicador error UTM" ,
    value = ifelse(error == 1 ,paste("Existen ",n_error," errores."),"No Error"),
    icon = icon("exclamation-circle"),
    color = ifelse(error == 1 ,"red","green")
  )
}


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
      sidebarMenu(id = "tabs", minified = T, collapsed = F,
        menuItem("Inicio", tabName = "inicio", icon = icon("home")),
        menuItem(UTMClientes[1], tabName = "usuario1", icon = icon("alert")),
        menuItem(UTMClientes[2], tabName = "usuario2", icon = icon("alert")),
        menuItem(UTMClientes[3], tabName = "usuario3", icon = icon("alert"))
        )
      ),
    
    ## Cuerpo de la app
    body = dashboardBody(
      
      # Usar tema personalizado
      use_theme(mytheme),
      
      # Definir contenidos de la app
      tabItems(
        ## ITEM: Inicio
        tabItem("inicio",
                fluidRow(
                  box(title = span(Icon_Account(1), UTMClientes[1]),
                      width=12, collapsible = T,
                      actionButton("id",paste("Enlace a página de ",UTMClientes[1])),
                      br(),
                      valueBoxOutput("vbox1"))),
                fluidRow(
                  box(title = span(Icon_Account(2), UTMClientes[2]),
                      width=12, collapsible = T, 
                      actionButton("id2",paste("Enlace a página de ",UTMClientes[2])),
                      br(),
                      valueBoxOutput("vbox2"))),
                fluidRow(
                  box(title = span(Icon_Account(3), UTMClientes[3]),
                      width=12, collapsible = T,
                      actionButton("id3",paste("Enlace a página de ",UTMClientes[3])),
                      br(),
                      valueBoxOutput("vbox3")))
                ),
        ## ITEM: Usuario 1 
        tabItem("usuario1", 
                fluidRow(
                  actionButton("home1",paste("Volver al inicio")),
                  box(width = 12, DTOutput('tbl1'))
                  )
        ),
        ## ITEM: Usuario 2 
        tabItem("usuario2", 
                fluidRow(
                  actionButton("home2",paste("Volver al inicio")),
                  box(width = 12, DTOutput('tbl2'))
                )
        ),
        ## ITEM: Usuario 3 
        tabItem("usuario3", 
                fluidRow(
                  actionButton("home3",paste("Volver al inicio")),
                  box(width = 12, DTOutput('tbl3'))
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
    
    

# Botones entre páginas ---------------------------------------------------
    
    ## Ir a cada usuario 
    observeEvent(input$id, {
      updateTabsetPanel(session, "tabs","usuario1")
    })
    observeEvent(input$id2, {
      updateTabsetPanel(session, "tabs","usuario2")
    })
    observeEvent(input$id3, {
      updateTabsetPanel(session, "tabs","usuario3")
    })
    
    ## Volver al home
    
    observeEvent( (input$home1 | input$home2 | input$home3) , {
      updateTabsetPanel(session, "tabs","inicio")
    })
    

    ## Creación VBOX - Inicio UTM -----------------
    output$vbox1 <- renderValueBox({ Vbox_UTM(1) })
    output$vbox2 <- renderValueBox({ Vbox_UTM(2) })
    output$vbox3 <- renderValueBox({ Vbox_UTM(3) })
    
    ## Creación DT - UTM dado Cliente
    output$tbl1 = renderDT(UTM_Account(1))
    output$tbl2 = renderDT( UTM_Account(2))
    output$tbl3 = renderDT(UTM_Account(3))
  }
  
  
  
)