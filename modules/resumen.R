
resumenServer <- function(input, output, session, credentials, filters, reset) {
  
  
  ## Reactivos más lentos para evitar que parpadee la aplicación y que pasen cosas raras
  deb_filters <- filters
  deb_check_btn <- reactive(input$check_btn) %>% debounce(500)
  ## Administrar checkbox
  proceed <- reactiveValues(
    check_btn = TRUE
  )
  ### Reset
  observeEvent(reset(),{
    ns <- session$ns
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = "RESETTING SUMMARY TABLE INPUTS",
      details = list()
    )))
    if (!setequal(deb_check_btn(), character(0))) {
      proceed$check_btn <- FALSE
      updateCheckboxGroupInput(
        session = session,
        inputId = 'check_btn',
        selected = character(0)
      )
    }
  })
  
  ### Asegurar que se seleccionan las variables correspondientes a los filtros seleccionados
  observeEvent({
    deb_filters()
    deb_check_btn()
  }, {
    for (v in business_unit_filters) {
      req(deb_filters()[[v]])
    }
    if (proceed$check_btn) {
      selected <- business_unit_filters %>% 
        map(~deb_filters()[[.x]]) %>% 
        set_names(business_unit_filters) %>% 
        keep(~ !nombre_todos %in% .x) %>% 
        names() %>% 
        c(deb_check_btn()) %>% 
        unique()
      if (!setequal(deb_check_btn(), selected)) {
        proceed$check_btn <- FALSE
        updateCheckboxGroupInput(
          session = session,
          inputId = 'check_btn',
          selected = selected
        )
      }
    } else {
      proceed$check_btn <- TRUE
    }
  }, ignoreNULL = FALSE) # Esto es para que la condición entre aún cuando input$check_btn sea nulo
  
  ## Tabla de resumen
  resumen_out <- reactive({
    deb_filters()$datos_filt %>%
      sum_filters_data(., deb_check_btn())
  })
  
  ## Tabla de resumen para mostrar en UI
  output$resumen <- DT::renderDataTable({
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = "LOADING TABLE",
      details = deb_filters()[all_filters]
    )))
    ##loginfo('LOADING resumen')
    resumen_out() %>% 
      select(-ym, -fecha_calculo) %>% 
      mutate_if(is.numeric, scales::comma) %>% 
      set_names(get_pretty_names(names(.))) %>% 
      datatable(
        options = list(
          filter = 'top',
          scrollX = TRUE,
          scrollY = '400px'
        )
      )
  })
  
  ## Botón de descarga de resumen
  output$download_resumen <- downloadHandler(
    filename = function() {
      paste0(deb_filters()$formato, Sys.Date(), "_resumen.csv")
    },
    content = function(file) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = "DOWNLOADING DATA SUMMARY",
        details = deb_filters()[all_filters]
      )))
      resumen_out() %>% 
        set_names(get_pretty_names(names(.))) %>% 
        write_excel_csv(file, na = '')
    }
  )
}

resumenUI <- function(id) {
  ns <- shiny::NS(id)
  
  tabPanel(
    title = "Resumen",
    tags$div(
      class = 'inline-inputs small-margin',
      checkboxGroupInput(
        inputId = ns("check_btn"),
        label = "",
        choices = business_unit_filters %>% 
          set_names(unlist(lang[.])),
        inline = TRUE
      ),
      downloadButton(ns("download_resumen"), label = lang$download_data)
    ),
    DT::dataTableOutput(ns("resumen")) %>% withSpinner(type = 8),
    tags$p(tags$strong(lang$money_units))
  )
}