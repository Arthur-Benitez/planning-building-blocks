
# options(shiny.reactlog = TRUE)

# Server ------------------------------------------------------------------
shinyServer(function(input, output, session) {
  
  ## Logger (va aquí y no en global para que sea en el archivo con fecha de la sesión, no de cuando prende el deployment)
  flog.logger(
    name = 'ROOT',
    threshold = INFO,
    appender = appender.tee(paste0(app_deployment_environment, '/log/', as.character(Sys.Date()), '.log'))
  )
  
  # Datos -------------------------------------------------------------------
  
  ## Info categorías y departamentos
  info_category <- read_tsv('data/business-units.txt') %>% 
    set_names(tolower(names(.))) %>% 
    mutate(
      departamento = pad_id(departamento),
      categoria = get_deptcat(departamento, pad_id(categoria)),
      departamento_label = paste0(departamento, ' - ', departamento_name),
      categoria_label = paste0(categoria, ' - ', categoria_name),
      tribu_label = tribu,
      squad_label = squad
    ) %>% 
    arrange(departamento, categoria)
  
  
  ## Datos building blocks
  datos <- list.files('data/summary', full.names = TRUE, recursive = TRUE) %>% 
    str_subset('[0-9]{8}_building_blocks_[0-9]{6}.rds') %>% 
    map_df(function(x){
      y <- tryCatch({
        readRDS(x) %>%
          init_col(req_cols, init_value = NA)
      }, error = function(e) {
        flog.info(toJSON(list(
          session_info = list(),
          message = "ERROR READING DATA",
          details = list(
            file = x
          )
        )))
        NULL
      })
      y
    })
  
  flog.info(toJSON(list(
    session_info = list(),
    message = "DONE READING DATA",
    details = list()
  )))
  
  ## UI - Header
  output$header <- renderUI({
    if (credentials()$user_auth) {
      logout_button <- logoutUI('logout')
    } else {
      logout_button <- NULL
    }
    header <- dashboardHeader(
      titleWidth = 300,
      tags$li(class = 'dropdown', logout_button)
    )
    header$children[[2]]$children <- tags$div(
      tags$div(
        id = 'app-logo-container',
        tags$img(src = 'bb4-logo.png', id = 'app-logo')
        # HTML('<img src="bb4-logo.png" id="app-logo">')
      ),
      tags$div(
        id = 'app-name-container',
        tags$p(id = 'app-name', lang$app_name),
        tags$p(id = 'app-description', lang$app_description)
      ),
      class = 'dropdown'
    )
    header
  })
  
  ## UI - Sidebar
  output$sidebar <- renderUI({
    if (credentials()$user_auth) {
      if (any(c('admin', 'owner') %in% credentials()$role)) {
        items <- tagList(
          menuItem(lang$ibp, tabName = 'ibp', icon = icon('bar-chart-o')),
          menuItem(lang$planning, tabName = 'planning', icon = icon('calendar')),
          menuItem(lang$usage_stats, tabName = 'usage_stats', icon = icon('tachometer-alt')),
          menuItem(lang$user_management, tabName = 'user_management', icon = icon('users')),
          menuItem(lang$password_update, tabName = 'password_update', icon = icon('lock')),
          menuItem(lang$faq, tabName = 'faq', icon = icon('question-circle'))
        )
      } else {
        items <- tagList(
          menuItem(lang$ibp, tabName = 'ibp', icon = icon('bar-chart-o')),
          menuItem(lang$planning, tabName = 'planning', icon = icon('calendar')),
          menuItem(lang$password_update, tabName = 'password_update', icon = icon('lock')),
          menuItem(lang$faq, tabName = 'faq', icon = icon('question-circle'))
        )
      }
      
    } else {
      items <- tagList(
        menuItem(lang$login, tabName = 'login', icon = icon('sign-in')),
        menuItem(lang$faq, tabName = 'faq', icon = icon('question-circle'))
      )
    }
    sidebarMenu(
      id = 'menu',
      items,
      column(
        icon('empire'),
        lang$app_version_text,
        align = 'right',
        width = 12,
        style = 'position: absolute; bottom: 0;'
      )
    )
  })
  
  ## UI - Body
  output$body <- renderUI({
    if (credentials()$user_auth) {
      inner_sidebar <- box(
        filtersUI('filters'),
        width = 2
      )
      inner_sidebar_ibp <- box(
        filters_ibp_UI('filters_ibp'),
        width = 2
      )
      tabItems(
        tabItem(
          tabName = 'ibp',
          fluidRow(
            inner_sidebar_ibp,
            tabBox(
              title = lang$ibp,
              id = 'tabs_ibp',
              width = 10,
              overviewUI('overview'),
              demandUI('demand'),
              colaborationUI('colaboration')
            )
          )
        ),
        tabItem(
          tabName = 'planning',
          fluidRow(
            inner_sidebar,
            tabBox(
              title = lang$planning_tabset,
              id = 'tabs',
              width = 10,
              waterfallUI('waterfall'),
              resumenUI('resumen'),
              downloadsUI('downloads')
            )
          )
        ),
        tabItem(
          tabName = 'usage_stats',
          usageStatsUI('usage_stats')
        ),
        tabItem(
          tabName = 'user_management',
          userManagementUI('user_management')
        ),
        tabItem(
          tabName = 'password_update',
          passwordUpdateUI('password_update')
        ),
        tabItem(
          tabName = 'faq',
          faqUI('faq')
        )
      )
    } else {
      tabItems(
        tabItem(
          tabName = 'login',
          shinyjs::useShinyjs(),
          loginUI('login')
        ),
        tabItem(
          tabName = 'faq',
          faqUI('faq')
        )
      )
    }
  })
  
  observe({
    if (credentials()$user_auth) {
      updateTabItems(session, 'menu', 'ibp')
      updateTabItems(session, 'menu', 'planning')
    } else {
      updateTabItems(session, 'menu', 'login')
    }
  })
  
  ## Login
  credentials <- callModule(
    login,
    id = 'login',
    log_out = reactive(logout_init())
  )
  
  logout_init <- callModule(
    logout,
    id = 'logout',
    active = reactive(credentials()$user_auth)
  )
  
  ## Administración de usuarios
  callModule(
    userManagementServer,
    id = 'user_management',
    credentials = reactive(credentials())
  )
  
  ## Actualización de contraseña
  callModule(
    passwordUpdateServer,
    id = 'password_update',
    credentials = reactive(credentials())
  )
  
  ## Filtros de negocio
  filters <- callModule(
    filtersServer,
    id = 'filters',
    credentials = reactive(credentials()),
    data = datos,
    info_category = info_category,
    waterfall_simulator_filters = reactive(waterfall())
  )
  
  ## Waterfall
  waterfall <- callModule(
    waterfallServer,
    id = 'waterfall',
    credentials = reactive(credentials()),
    filters = filters,
    reset = reactive(filters()$reset)
  )
  
  ## Tabla de resumen
  resumen <- callModule(
    resumenServer,
    id = 'resumen',
    credentials = reactive(credentials()),
    filters = filters,
    reset = reactive(filters()$reset)
  )
  
  ## Descargas
  callModule(
    downloadsServer,
    id = 'downloads'
  )
  
  #filtros ibp
  filtered_data <- callModule(
    filters_ibp_Server,
    id = "filters_ibp"
  )
  
  #Overview
  callModule(
    overviewServer,
    id = 'overview',
    filtered_data = filtered_data
  )
  
  #Plan de Demanda
  callModule(
    demandServer,
    id = 'demand',
    filtered_data = filtered_data
  )
  
  #Vista de colaboracion
  callModule(
    colaborationServer,
    id = 'colaboration',
    filtered_data = filtered_data
  )
  
  ## Usage stats
  callModule(
    usageStatsServer,
    id = 'usage_stats',
    credentials = reactive(credentials())
  )
  
  ##FAQ
  callModule(
    faqServer,
    id = 'faq'
  )
})



