library(shiny)
library(shinyjs)
library(shinydashboard)
source('modules/login.R')

header <- dashboardHeader(title = 'Testing login!')



ui <- dashboardPage(
  title="Building Blocks",
  header,
  dashboardSidebar(uiOutput('sidebar')),
  dashboardBody(uiOutput('body'))
)

# ui <- shinyUI(fluidPage(
#   shinyjs::useShinyjs(),
#   logoutUI(id = 'logout'),
#   loginUI(id = 'login'),
#   dataTableOutput('mtcars')
# ))

server <- shinyServer(function(input, output, session) {
  logout_init <- callModule(
    logout,
    id = 'logout',
    active = reactive(credentials()$user_auth)
  )
  
  credentials <- callModule(
    login,
    id = 'login',
    user_data = users,
    log_out = reactive(logout_init())
  )
  
  output$sidebar <- renderUI({
    if (credentials()$user_auth) {
      sidebarMenu(
        id = 'menu',
        menuItem(
          'Sección 1',
          tabName = 'seccion-1'
        )
      )
    } else {
      NULL
    }
  })
    
  
  output$body <- renderUI({
    if (credentials()$user_auth) {
      tabItems(
        tabItem(
          tabName = 'seccion-1',
          logoutUI(id = 'logout'),
          h4('Hola Mundo!')
        )
      )
    } else {
      tagList(
        shinyjs::useShinyjs(),
        loginUI(id = 'login')
      )
    }
  })
  
  observe({
    if (credentials()$user_auth) {
      updateTabItems(session, 'menu', 'seccion-1')
    }
  })
  
  output$mtcars <- renderDataTable({
    req(credentials()$user_auth)
    mtcars
  })
})

shinyApp(ui, server)
