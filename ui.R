
## Header
header <- uiOutput('header')

## Sidebar
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  uiOutput('sidebar')
)

## Body
body <- dashboardBody(
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'custom.css'),
    tags$link(rel = 'stylesheet', href = 'https://fonts.googleapis.com/css?family=Bree+Serif|Coiny')
  ),
  uiOutput('body')
)

dashboardPage(
  title = lang$app_name,
  header,
  sidebar,
  body
)
