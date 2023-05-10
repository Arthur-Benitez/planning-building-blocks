

# Helper functions --------------------------------------------------------

default_choice <- function(choices, selected) {
  if (is.null(choices)) {
    return(NULL)
  }
  if (!is.null(selected) && selected %in% choices) {
    selected
  } else {
    choices[1]
  }
}

pretty_filters <- function(values, variable, mapping) {
  variable_label <- paste0(variable, '_label')
  if (variable %in% names(mapping) && variable_label %in% names(mapping)) {
    maybe_extract(values, deframe(mapping[c(variable, variable_label)]))
  } else {
    values
  }
}

update_filters <- function(session, variable, selected, values, mapping, nombre_todos = nombre_todos, .desc = FALSE) {
  choices <- c(nombre_todos, sort(unique(values[[variable]]), decreasing = .desc))
  if (!is.null(mapping)) {
    names(choices) <- pretty_filters(choices, variable, mapping)
  }
  updateSelectInput(session, variable, choices = choices, selected = selected)
}


# Server ------------------------------------------------------------------

filtersServer <- function(input, output, session, credentials, data, info_category, waterfall_simulator_filters) {
  
  ## Generar UI inicial
  ## >> Por alguna razón no funciona con un for() y no quise escribir uno por uno
  eval(parse(text = paste(str_replace_all("
  output[[paste0('input_', '%s')]] <- renderUI({
     choices <- c(nombre_todos, sort(unique(data[['%s']])))
     names(choices) <- pretty_filters(choices, '%s', info_category)
     selectInput(
       inputId = session$ns('%s'),
       label = h5(lang[['%s']]),
       choices = choices
     )
   });", '%s', business_unit_filters), collapse = '\n')))

  eval(parse(text = paste(str_replace_all("
  output[[paste0('input_', '%s')]] <- renderUI({
     choices <- sort(unique(data[['%s']]), decreasing = TRUE)
     selectInput(
       inputId = session$ns('%s'),
       label = h5(lang[['%s']]),
       choices = choices
     )
   });", '%s', time_filters), collapse = '\n')))
  
  ## Actualizar valores de inputs para que tengan sentido
  trigger <- reactiveValues(
    filter_data = 0,
    reset_timer = 0,
    reset = 0,
    return = 0,
    init_output = 0
  )
  
  ### Actualizar fechas de cálculo cuando cambia el YM
  observeEvent(input$ym, {
    
    choices <- as.character(sort(unique(data$fecha_calculo[data$ym == input$ym]), decreasing = TRUE))
    selected <- default_choice(choices, input$fecha_calculo)
    updateSelectInput(session, 'fecha_calculo', choices = choices, selected = selected)
  })
  
  ### Actualizar filtros de unidad de negocio
  filter_combinations <- data[all_filters] %>% 
    distinct() %>% 
    mutate_all(as.character)
  observeEvent(input$apply_filters, {
    fc_filtered <- all_filters %>% 
      map(~{
        filter_combinations[[.x]] == input[[.x]] |
          input[[.x]] == nombre_todos
      }) %>% 
      reduce(`&`) %>% 
      filter_combinations[., ]
    for (v in business_unit_filters) {
      update_filters(
        session = session,
        variable = v,
        selected = input[[v]],
        values = fc_filtered,
        mapping = info_category,
        nombre_todos = nombre_todos
      )
    }
    trigger$filter_data <- trigger$filter_data + 1
  })
  
  ## Cargar simulación
  observeEvent(waterfall_simulator_filters(), {
    fc_filtered <- all_filters %>% 
      map(~{
        filter_combinations[[.x]] == waterfall_simulator_filters()[[.x]] |
          waterfall_simulator_filters()[[.x]] == nombre_todos
      }) %>% 
      reduce(`&`) %>% 
      filter_combinations[., ]
    
    for (v in business_unit_filters) {
      update_filters(
        session = session,
        variable = v,
        selected = waterfall_simulator_filters()[[v]],
        values = fc_filtered,
        mapping = info_category,
        nombre_todos = nombre_todos
      )
    }
    update_filters(
      session = session,
      variable = 'ym',
      selected = waterfall_simulator_filters()[['ym']],
      values = data, # ojo que aquí es data
      mapping = NULL,
      nombre_todos = NULL,
      .desc = TRUE
    )
    update_filters(
      session = session,
      variable = 'fecha_calculo',
      selected = waterfall_simulator_filters()[['fecha_calculo']],
      values = fc_filtered,
      mapping = NULL,
      nombre_todos = NULL,
      .desc = TRUE
    )
    trigger$filter_data <- trigger$filter_data + 1
  })
  

  ## Resetear filtros manualmente (el simulador se resetea en waterfall)
  ## Nota: El tema del timer es porque si no, por alguna razón no se actualizan
  ## a tiempo los inputs y entonces datos_filt se recalcula con los filtros que
  ## ya estaban. Con este truco obligamos a que el proceso espere 500ms antes de
  ## recalcular.
  observeEvent({
    input$reset
    trigger$init_output
  }, {
    fc_filtered <- time_filters %>% 
      map(~{
        filter_combinations[[.x]] == input[[.x]] |
          input[[.x]] == nombre_todos
      }) %>% 
      reduce(`&`) %>% 
      filter_combinations[., ]
    for (v in business_unit_filters) {
      update_filters(
        session = session,
        variable = v,
        selected = nombre_todos,
        values = fc_filtered,
        mapping = info_category,
        nombre_todos = nombre_todos
      )
    }
    trigger$reset_timer <- trigger$reset_timer + 1
  })
  reset_flag <- reactiveVal(FALSE)
  observe({
    trigger$reset_timer
    if (!isolate(reset_flag())) {
      reset_flag(TRUE)
      invalidateLater(500)
    } else {
      reset_flag(FALSE)
      isolate(trigger$reset <- trigger$reset + 1)
    }
  })
  
  ## Datos con todos los filtros
  observeEvent(input$ym, {
    ## Esto obliga a datos_filt a que corra una vez al principio (i.e. se
    ## inicialice) sin necesidad de dar click en "Aplicar"
    trigger$init_output <- trigger$init_output + 1
  }, ignoreInit = TRUE, ignoreNULL = TRUE, once = TRUE)

  datos_filt <- eventReactive({
    trigger$filter_data
    trigger$reset
  }, {
    for (v in all_filters) {
      req(input[[v]])
    }
    all_filters %>% 
      map(~{
        data[[.x]] == input[[.x]] |
          input[[.x]] == nombre_todos
      }) %>% 
      reduce(`&`) %>% 
      data[., ]
  })
  
  ## Objeto de salida para uso en otros módulos
  reactive({
    res <- list(
      reset = input$reset,
      datos_filt = datos_filt()
    )
    for (v in all_filters) {
      isolate(res[[v]] <- input[[v]])
    }
    res
  })
  
}


# UI ----------------------------------------------------------------------


filtersUI <- function(id) {
  ns <- shiny::NS(id)
  
  tagList(
    actionButton(ns('apply_filters'), lang$apply_filters, icon = icon('filter')),
    actionButton(ns('reset'), label = lang$reset, icon = icon('redo-alt')),
    do.call(
      tagList,
      map(all_filters, ~uiOutput(ns(paste0('input_', .x))))
    )
  )
}
