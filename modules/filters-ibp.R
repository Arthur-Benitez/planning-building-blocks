unfiltered_data <- process_info()
unique_choices <- read_rds(choices_file)

filters_ibp_Server <- function(input, output, session){
  metric_reac <- reactive({input$metric})
  
  datos_filt <- eventReactive({
    input$apply_filters
  }, {
    for (v in ibp_filters) {
      req(input[[v]])
    }
    # Aplicar los filtros a la información
    ibp_filters %>% 
      map(~{
        (unfiltered_data[[.x]] %in% input[[.x]] & 
          !is.na(unfiltered_data[[.x]])) |
          nombre_todos %in% input[[.x]]
      }) %>% 
      reduce(`&`) %>% 
      unfiltered_data[., ]
  })
  
  eval(parse(text = paste(str_replace_all("
  output[[paste0('input_', '%s')]] <- renderUI({
     choices <- c(nombre_todos, unique_choices[['%s']])
     pickerInput(
      inputId = session$ns('%s'),
      label = lang[['%s']],
      choices = choices,
      selected = nombre_todos,
      options = list(
        `live-search` = TRUE,
        `selected-text-format` = 'count > 3'
      ),
      multiple = TRUE
    )
   });", '%s', ibp_filters), collapse = '\n')))
  
  reactive({
    filtered_data <- list(
      metric = metric_reac(),
      data = datos_filt()
    )
    filtered_data
  })
}

filters_ibp_UI <- function(id){
  ns <- NS(id)
  tagList(
    radioGroupButtons(
      inputId = ns('metric'),
      label = 'Unidad',
      choices = c('Costo ($)' = 'cost', 'Piezas' = 'eaches'),
      status = 'primary',
      justified = TRUE,
      checkIcon = list(
        yes = icon('ok', lib = 'glyphicon'),
        no = icon('remove', lib = 'glyphicon')
      )
    ),
    actionBttn(
      inputId = ns('apply_filters'),
      label = lang$button,
      style = 'jelly', 
      color = 'primary'
    ),
    tags$hr(),
    do.call(
      tagList,
      map(ibp_filters, ~uiOutput(ns(paste0('input_', .x))))
    )
  )
}
