
downloadsServer <- function(input, output, session) {
  files <- reactive({
    input$refresh
    sort(list.files('data/detail/', full.names = TRUE, recursive = TRUE, include.dirs = FALSE, pattern = '*detalle.csv'), decreasing = TRUE)
  })
  
  output$dropdown <- renderUI({
    selectInput(
      inputId = session$ns('selected_file'),
      label = 'Seleccionar archivo',
      choices = set_names(files(), basename(files())),
      multiple = FALSE,
      width = '400px'
    )
  })
  
  output$download_file <- downloadHandler(
    filename = function(){
      basename(input$selected_file)
    },
    content = function(file){
      file.copy(input$selected_file, file)
    },
    contentType = 'text/csv'
  )
  
}

downloadsUI <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    title = "Descargas",
    actionButton(ns('refresh'), label = 'Actualizar', icon = icon('refresh')),
    uiOutput(ns('dropdown')),
    downloadButton(
      outputId = ns('download_file'),
      label = lang$download_data
    )
  )
}
