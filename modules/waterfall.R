
waterfallServer <- function(input, output, session, credentials, filters, reset) {
  
  ## Reset
  observeEvent(reset(),{
    ns <- session$ns
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = "RESETTING SIMULATOR INPUTS",
      details = list()
    )))
    updateNumericInput(session, 'sim_forecast_promocional', value = 0)
    updateNumericInput(session, 'sim_no_resurtible',value = 0)
    updateNumericInput(session, 'sim_fulfillment',value = 0)
  })
  
  is_na_bp <- reactiveVal()
  waterfall <-  reactive({
    req(nrow(filters()$datos_filt) > 0)
    
    ## Quitar NAs de inputs de simulador
    sim_forecast_promocional <- input$sim_forecast_promocional %>% as.numeric() %>% ifelse(is.na(.), 0, .)
    sim_no_resurtible <- input$sim_no_resurtible %>% as.numeric() %>% ifelse(is.na(.), 0, .)
    sim_fulfillment <- input$sim_fulfillment %>% as.numeric() %>% ifelse(is.na(.), 0, .)
    
    ## Sumarizar
    df_temp_1 <- sum_filters_data(filters()$datos_filt, NULL)
    is_na_bp(any(is.na(df_temp_1$bp)))

    ## Poner un BP y un Gap cuando no hay BP
    dd <- df_temp_1 %>%
      mutate(
        proyeccion_total = ventas + forecast_base + forecast_promocional + sim_forecast_promocional + no_resurtible + sim_no_resurtible + sim_fulfillment,
        bp = ifelse(any(is.na(bp)), proyeccion_total, bp),
        gap = bp - proyeccion_total,
        fulfillment = fulfillment_ref
      )
    
    ## Definir barras
    b <- list()
    b$ventas <- list(
      x = 'ventas',
      base = 0,
      y = dd$ventas,
      type = 'proyeccion'
    )
    b$forecast_base <- list(
      x = 'forecast_base',
      base = with(b$ventas, base + y),
      y = dd$forecast_base,
      type = 'proyeccion'
    )
    b$forecast_promocional <- list(
      x = 'forecast_promocional',
      base = with(b$forecast_base, base + y),
      y = dd$forecast_promocional,
      type = 'proyeccion'
    )
    b$sim_forecast_promocional <- list(
      x = 'forecast_promocional',
      base = with(b$forecast_promocional, base + y),
      y = sim_forecast_promocional,
      type = 'apuestas'
    )
    b$no_resurtible <- list(
      x = 'no_resurtible',
      base = with(b$sim_forecast_promocional, base + y),
      y = dd$no_resurtible,
      type = 'proyeccion'
    )
    b$sim_no_resurtible <- list(
      x = 'no_resurtible',
      base = with(b$no_resurtible, base + y),
      y = sim_no_resurtible,
      type = 'apuestas'
    )
    b$sim_fulfillment <- list(
      x = 'fulfillment',
      base = with(b$sim_no_resurtible, base + y),
      y = sim_fulfillment,
      type = 'apuestas'
    )
    b$fulfillment_ref <- list(
      x = 'fulfillment',
      base = with(b$sim_no_resurtible, base + y),
      y = dd$fulfillment_ref,
      type = 'potencial'
    )
    b$gap <- list(
      x = 'gap',
      base = with(b$sim_fulfillment, base + y),
      y = dd$gap,
      type = 'proyeccion'
    )
    b$bp <- list(
      x = 'bp',
      base = 0,
      y = dd$bp,
      type = 'proyeccion'
    )
    b$ventas_ly <- list(
      x = 'ventas_ly',
      base = 0,
      y = dd$ventas_ly,
      type = 'proyeccion'
    )
    b <- map(b, as_tibble)
    b
  })
  
  ## Waterfall (separada en reactive + renderPlotly porque renderPlotly no permite debuggear bien)
  plot_flag <- reactiveVal(TRUE)
  grafica <- reactive({
    flog.info(toJSON(list(
      session_info = msg_cred(credentials()),
      message = "LOADING WATERFALL PLOT",
      details = filters()[all_filters]
    )))
    if (plot_flag()) {
      invalidateLater(500)
      plot_flag(FALSE)
    } ## Check para que la gráfica se inicialice correctamente con el debounce
    
    if (nrow(filters()$datos_filt) == 0) {
      ## Cuando los filtros no aplican (i.e. no hay info para esos filtros), mostramos una gráfica vacía
      g <- plot_ly() %>% 
        add_text(x = 0, y = 0, text = ':(', textfont = list(size = 80)) %>% 
        layout(
          title = list(
            text = lang$title_error,
            font = list(size = 30)
          ),
          xaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            showticklabels = FALSE
          ),
          margin = list(t = 60)
        )
    } else {
      ## Fill
      gap <- waterfall()$gap$y
      fills <- list(
        proyeccion = '#595959CC',
        apuestas = '#00b8e6CC',
        potencial = '#00000000',
        gap = ifelse(gap > 0, rgb(221, 75, 57, 204, maxColorValue = 255), rgb(0, 176, 80, 204, maxColorValue = 255)),
        bp = ifelse(is_na_bp(), '#59595911', '#595959CC')
      )
      
      ## Line
      lines <- list(
        color = list(
          proyeccion = NA,
          potencial = rgb(204, 0, 153, maxColorValue = 255),
          apuestas = NA
        ),
        width = list(
          proyeccion = 0,
          potencial = 3,
          apuestas = 0
        ),
        dash = list(
          proyeccion = 'solid',
          potencial = 'dot',
          apuestas = 'solid'
        )
      )
      ## Bar width
      bwd <- 0.6
      
      ## Bar order
      bar_order <- c('ventas', 'forecast_base', 'forecast_promocional', 'no_resurtible', 'fulfillment', 'gap', 'bp', 'ventas_ly')
      
      ## Show legend
      show_legend <- c('ventas', 'sim_forecast_promocional', 'fulfillment_ref')
      
      ## Plot df list
      plot_df_list <- waterfall() %>% 
        map2(., names(.), function(df, name){
          df %>% 
            mutate(
              x = factor(x, levels = bar_order),
              ymax = base + y,
              marker = map2(type, x, function(y, xx){
                xx <- as.character(xx)
                list(
                  color = ifelse(xx %in% names(fills), fills[[xx]], fills[[y]]),
                  line = list(
                    color = lines$color[[y]],
                    width = lines$width[[y]],
                    dash = lines$dash[[y]]
                  )
                )
              }),
              showlegend = name %in% show_legend,
              p = y / waterfall()$bp$y,
              p_cum = ymax / waterfall()$bp$y,
              text = case_when(
                x %in% c('gap', 'bp') & is_na_bp() ~ 'N/A',
                x %in% c('bp', 'gap', 'ventas_ly') ~ sprintf('%s:\n%s\n$%s', get_pretty_names(x), fg_percent(p, 1), scales::comma(y, 1)),
                type == 'potencial' ~ sprintf('%s:\n%s\n$%s', get_pretty_names(type), fg_percent(p, 1), scales::comma(y, 1)),
                TRUE ~ sprintf('%s:\n%s\n$%s\nAcumulado:\n%s\n$%s', get_pretty_names(type), fg_percent(p, 1), scales::comma(y, 1), fg_percent(p_cum, 1), scales::comma(ymax, 1))
              )
            )
        })
      
      ## Segmentos que unen barras
      segments <- plot_df_list %>% 
        bind_rows() %>% 
        filter(type != 'potencial' & !x %in% tail(bar_order, 2)) %>% 
        group_by(x) %>% 
        summarise(
          ymax = max(ymax)
        ) %>% 
        ungroup() %>% 
        mutate(
          x = as.numeric(x) + bwd / 2,
          xend = as.numeric(x) + 1 - bwd,
          y = ymax,
          yend = ymax
        ) %>% 
        filter_all(all_vars(!is.na(.)))
      ln <- list(type = 'line', line = list(color = 'gray', width = 1, dash = 'dot'), xref = 'x', yref = 'y')
      lns <- map(1:nrow(segments), function(i){
        ll <- ln
        ll$x0 <- segments$x[i] - 1
        ll$x1 <- segments$xend[i] - 1
        ll$y0 <- segments$y[i]
        ll$y1 <- segments$yend[i]
        ll
      })
      
      ## Etiquetas
      labels <- plot_df_list %>% 
        bind_rows() %>% 
        group_by(x) %>% 
        summarise(
          ymax = max(pmax(ymax, base)),
          p = sum(y[type != 'potencial'], na.rm = TRUE) / plot_df_list$bp$y,
          p_potencial = sum(y[type == 'potencial'], na.rm = TRUE) / plot_df_list$bp$y
        ) %>% 
        ungroup() %>% 
        mutate(
          text = case_when(
            x == 'fulfillment' ~ sprintf('%s (%s)', fg_percent(p, 1), fg_percent(p_potencial, 1)),
            x %in% c('gap', 'bp') & is_na_bp() ~ 'N/A',
            TRUE ~ fg_percent(p, 1)
          ),
          text_y = ymax + 0.05 * max(ymax)
        )
      
      g <- plot_ly(x = ~get_pretty_names(x)) %>% 
        layout(
          shapes = lns,
          title = list(
            text = title_summary(list(filters = filters())),
            font = list(size = 30)
          ),
          font = list(size = 18),
          xaxis = list(
            title = '',
            tickangle = 0,
            tickfont = list(size = 18)
          ),
          yaxis = list(
            title = '',
            range = c(0, max(map_dbl(plot_df_list, 'ymax')) * 1.1)
          ),
          barmode = 'stack',
          showlegend = TRUE,
          legend = list(
            orientation = "h",
            x = 0,
            y = -0.1
          ),
          margin = list(t = 60),
          hovermode = 'compare'
        )
      for (df in plot_df_list) {
        g <- add_bars(
          p = g,
          y = ~y,
          base = ~base,
          text = ~text,
          hoverinfo = 'text',
          width = bwd,
          marker = df$marker[[1]],
          data = df,
          name = get_pretty_names(df$type),
          showlegend = df$showlegend
        )
      }
      g <- g %>% 
        add_text(
          data = labels,
          y = ~ymax,
          text = ~text,
          textposition = 'top',
          textfont = list(color = '#000000'),
          hoverinfo = 'none',
          showlegend = FALSE
        )
    }
    g
  }) %>% debounce(1000)
  
  ## Exportar waterfall
  output$grafica <- renderPlotly({
    grafica()
  })
  
  
  waterfall_out <- reactive({
    req(filters()$fecha_calculo, filters()$ym)
    waterfall() %>% 
      transmute(
        fecha_calculo = filters()$fecha_calculo,
        ym = filters()$ym,
        Rubro = x,
        fill,
        y
      ) %>% 
      spread(fill, y, fill = 0) %>% 
      group_by(fecha_calculo, ym, Rubro) %>% 
      summarise(
        proyeccion = sum(proyeccion),
        apuestas = sum(apuestas),
        Total = proyeccion + apuestas
      ) %>% 
      set_names(get_pretty_names(names(.)))
  })
  
  output$download_waterfall <- downloadHandler(
    filename = function() {
      paste0(Sys.Date(), "_waterfall.csv")
    },
    content = function(file) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = "DOWNLOADING WATERFALL DATA",
        details = filters()[all_filters]
      )))
      write_excel_csv(waterfall_out(), file)
    }
  )
  
  ## Simulador ----------------------------------------------------------
  
  ### Inicializar
  simulator_base_path <- paste0(app_deployment_environment, '/sim/')
  simulator_user_path <- reactive(paste0(simulator_base_path, credentials()$user))
  simulator_current_path <- reactive(paste0(simulator_user_path(), '/', paste(selected_sim_path(), collapse = '/')))
  observe({
    req(simulator_user_path(), credentials(), credentials()$user)
    if (!dir.exists(simulator_user_path()) && nchar(credentials()$user) > 0) {
      dir.create(simulator_user_path(), recursive = TRUE)
    }
  })
  
  ### Manipular estado
  when_finished <- reactiveValues(
    save_sim = 0,
    confirm_delete_sim = 0,
    confirm_add_blob = 0,
    modify_tree_sharing = 0
  )
  
  ### Simulaciones aplicables a filtros actuales
  sim_list <- reactive({
    req(credentials()$user)
    req(filters())
    req(simulator_current_path())
    ## Generar dependencia con los botones, pero esperar a que se acaben esos procesos
    when_finished$confirm_delete_sim
    when_finished$save_sim
    when_finished$confirm_add_blob
    when_finished$modify_tree_sharing
    current_filters_sim <- simulation(
      user = 'x',
      filters = filters()[all_filters],
      dir = simulator_current_path(),
      write = FALSE
    )
    get_sim_list(path = simulator_current_path(), credentials = credentials()) %>%
      keep(function(z) {
        input$show_all_sims ||
          all(map_lgl(time_pred_filters, ~ isTRUE(z$filters[[.x]] == current_filters_sim$filters[[.x]])))
      })
  })
  
  ### Simulaciones de usuario a compartir
  sim_list_share <- reactive({
    req(input$selected_user)
    req(simulator_base_path)
    ## Generar dependencia con los botones, pero esperar a que se acaben esos procesos
    ## Es necesario aquí en caso de que el usuario a compartir sea el mismo usuario que comparte
    when_finished$confirm_delete_sim
    when_finished$save_sim
    when_finished$confirm_add_blob
    when_finished$modify_tree_sharing
    get_sim_list(path = paste0(simulator_base_path, '/', input$selected_user), credentials = credentials()) %>%
      keep(function(.x){
        (
          is.tree(.x) &&
            (
              .x$user == input$selected_user ||
                .x$sharing$visible == TRUE
            )
        ) &&
          selected_sim() %<=% .x &&
          !identical(selected_sim(), .x)
      })
  })
  
  ### UI para seleccionar simulaciones
  selected_sim_path <- reactiveVal(NULL)
  output$selected_sim_ui <- renderUI({
    # req(input$selected_sim != '-' || is.null(input$selected_sim))
    req(sim_list())
    choices <- get_sim_list_choices(sim_list())
    if (isTRUE(isolate(selected_sim())$path %in% choices) && !identical(isolate(input$selected_sim), '-')) {
      selected <- isolate(selected_sim())$path
    } else {
      selected <- '-'
    }
    ns <- session$ns
    selectInput(
      inputId = ns('selected_sim'),
      label = h4(lang$selected_sim),
      choices = c('-', choices),
      selected = selected
    )
  })
  
  
  selected_sim <- reactive({
    when_finished$modify_tree_sharing
    tryCatch({
      if (input$selected_sim == '-') {
        NULL
      } else {
        read_simulation(input$selected_sim, simulator_current_path())
      }
    }, error = function(e){
      NULL
    })
  })
  observeEvent(input$get_children, {
    req(input$selected_sim)
    if (input$selected_sim != '-') {
      new_sim <- read_simulation(input$selected_sim, simulator_current_path())
      if (!new_sim$path %in% selected_sim_path()) {
        selected_sim_path(c(selected_sim_path(), new_sim$path))
      }
    }
  })
  observeEvent(input$get_parent, {
    req(input$selected_sim)
    if (length(selected_sim_path()) > 0) {
      selected_sim_path(head(selected_sim_path(), -1))
    }
  })
  output$selected_sim_path <- renderUI({
    tryCatch({
      base <- 'home'
      if (length(selected_sim_path()) == 0) {
        road <- NULL
        last <- NULL
      } else {
        if (length(selected_sim_path()) == 1) {
          road <- NULL
        } else {
          road <- head(selected_sim_path(), -1) %>% 
            strsplit('_') %>% 
            map_chr(2) %>% 
            paste0('(', ., ')')
        }
        last_sim_dir <- strsplit(simulator_current_path(), '/') %>%
          .[[1]] %>% 
          head(-1) %>%
          paste0(collapse = '/')
        last_sim_name <- strsplit(simulator_current_path(), '/')[[1]] %>% tail(1)
        last_sim <- read_simulation(last_sim_name, last_sim_dir)
        last <- get_sim_list_choices(list(last_sim)) %>% names()
      }
      res <- c(base, road, last)
      res <- paste0(map_chr(seq_along(res), ~ strrep('>', .x - 1)), ' ', res)
      tags$div(
        h4('Ruta'),
        reduce(res, ~ tagList(.x, tags$br(), .y))
      )
    }, error = function(e){
      NULL
    })
  })
  output$selected_sim_details <- renderUI({
    if (is.null(selected_sim())) {
      info <- tags$p('-')
    } else {
      dt <- title_dates(selected_sim())
      usr <- title_user(selected_sim())
      x <- selected_sim()
      info <- tags$p(
        tags$strong(paste0(lang$ym, ':')), x$filters$ym, tags$br(),
        tags$strong(paste0(lang$fecha_calculo, ':')), x$filters$fecha_calculo, tags$br(),
        tags$strong(paste0(lang$created_by, ':')), x$user, tags$br(),
        tags$strong(paste0(lang$created_on, ':')), x$timestamp, tags$br()
      )
    }
    tags$div(
      h4('Detalles'),
      info
    )
  })
  
  
  
  ### Guardar blob
  observeEvent(input$save_sim, {
    tryCatch({
      new_sim <- blob(
        user = credentials()$user,
        filters = filters() %>% keep(is.atomic),
        dir = simulator_user_path(), # siempre se crean en la carpeta root
        simulation = list(
          sim_forecast_promocional = input$sim_forecast_promocional,
          sim_no_resurtible = input$sim_no_resurtible,
          sim_fulfillment = input$sim_fulfillment
        )
      )
      
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'SAVED SIMULATION DATA',
        details = list(
          file = new_sim$path
        )
      )))
    }, error = function(e){
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ERROR SAVING SIMULATION DATA'
      )))
    })
    
    when_finished$save_sim <- when_finished$save_sim + 1
  })
  
  ### Guardar tree
  observeEvent(input$save_tree, {
    tryCatch({
      new_sim <- tree(
        user = credentials()$user,
        filters = filters() %>% keep(is.atomic),
        dir = simulator_user_path(),
        sharing = sharing_permissions()
      )
      
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'SAVED TREE',
        details = list(
          file = new_sim$path
        )
      )))
    }, error = function(e){
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ERROR SAVING TREE'
      )))
    })
    
    when_finished$save_sim <- when_finished$save_sim + 1
  })
  
  ### Cambiar permisos de tree
  observeEvent(input$share_tree, {
    ns <- session$ns
    if (is.tree(selected_sim())) {
      showModal(modalDialog(
        easyClose = FALSE,
        title = 'Compartir carpeta',
        checkboxInput(ns('sharing_visible'), lang$visible, selected_sim()$sharing$visible),
        checkboxInput(ns('sharing_locked'), lang$locked, selected_sim()$sharing$locked),
        dateInput(ns('sharing_unlock_deadline'), lang$unlock_deadline, selected_sim()$sharing$unlock_deadline),
        footer = tagList(
          modalButton(lang$cancel),
          actionButton(ns('confirm_share_tree'), lang$ok, class = 'btn-primary')
        )
      ))
    } else {
      showModal(modalDialog(
        easyClose = TRUE,
        title = 'Error',
        'Sólo se puede editar los permisos de carpetas.',
        footer = modalButton(lang$ok)
      ))
    }
  })
  
  observeEvent(input$confirm_share_tree, {
    tryCatch({
      removeModal()
      ## Modificar tree
      selected_sim() %>% 
        modify_sharing(
          dir = simulator_current_path(),
          visible = input$sharing_visible,
          locked = input$sharing_locked,
          unlock_deadline = input$sharing_unlock_deadline,
          write = TRUE
        )
      ## Refrescar lista de simulaciones
      when_finished$modify_tree_sharing <- when_finished$modify_tree_sharing + 1
      
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'MODIFIED TREE SHARING',
        details = list(
          file = selected_sim()$path
        )
      )))
    }, error = function(e) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ERROR MODIFIYING TREE SHARING',
        details = list(
          file = selected_sim()$path
        )
      )))
    })
  })
  
  
      
  
  ### Agregar blob a tree
  #### Mostrar usuarios disponibles
  observeEvent(input$add_blob, {
    if (is.null(selected_sim())) {
      showModal(modalDialog(
        easyClose = TRUE,
        title = 'Error',
        'Favor de seleccionar un escenario.',
        footer = tagList(
          modalButton(lang$ok)
        )
      ))
    } else {
      ns <- session$ns
      users <- load_users(user_data_path)
      choices <- users %>% map_chr('user')
      names(choices) <- ifelse(choices == credentials()$user, paste0('*', choices), choices)
      selected <- credentials()$user
      showModal(modalDialog(
        easyClose = FALSE,
        title = 'Seleccionar recipiente',
        selectInput(ns('selected_user'), h4(lang$user), choices = choices, selected = selected),
        footer = tagList(
          modalButton(lang$cancel),
          actionButton(ns('add_blob_user'), lang$ok, class = 'btn-primary')
        )
      ))
    }
  })
  #### Mostrar simulaciones de un usuario
  observeEvent(input$add_blob_user, {
    ns <- session$ns
    if (length(sim_list_share()) > 0) {
      choices <- get_sim_list_choices(sim_list_share())
      if (input$selected_user == credentials()$user) {
        choices <- c('home', choices)
      }
      showModal(modalDialog(
        easyClose = FALSE,
        title = 'Seleccionar carpeta',
        selectInput(ns('selected_share_tree'), h4(sprintf('Carpetas de %s', input$selected_user)), choices = choices),
        footer = tagList(
          modalButton(lang$cancel),
          actionButton(ns('check_add_blob'), lang$share, class = 'btn-primary')
        )
      ))
    } else {
      showModal(modalDialog(
        easyClose = TRUE,
        title = lang$title_error,
        tags$p(sprintf('El usuario %s no tiene carpetas disponibles', input$selected_user)),
        footer = tagList(
          modalButton(lang$ok)
        )
      ))
    }
  })
  #### Validar clashes con blobs existentes
  clashes <- reactiveVal()
  observeEvent(input$check_add_blob, {
    share_path <- paste0(simulator_base_path, '/', input$selected_user)
    if (input$selected_share_tree == 'home') {
      clashes(clashes_with_directory(share_path, selected_sim()))
    } else {
      selected_share_tree <- read_simulation(input$selected_share_tree, share_path)
      clashes(clashes_with_children(selected_share_tree, selected_sim(), share_path))
    }
    ns <- session$ns
    if (identical(clashes(), FALSE)) {
      showModal(modalDialog(
        easyClose = FALSE,
        title = '¿Enviar?',
        'Ningún escenario se sobreescribirá',
        footer = tagList(
          modalButton(lang$cancel),
          actionButton(ns('confirm_add_blob'), lang$confirm, class = 'btn-primary')
        )
      ))
    } else {
      clashes_to_overwrite <- get_sim_list_choices(clashes()) %>% 
        names()
      showModal(modalDialog(
        easyClose = FALSE,
        title = '¿Enviar y sobreescribir escenarios?',
        tags$p('Los siguientes escenarios se sobreescribirán:'),
        tags$ul(map(clashes_to_overwrite, tags$li)),
        footer = tagList(
          modalButton(lang$cancel),
          actionButton(ns('confirm_add_blob'), lang$confirm, class = 'btn-primary')
        )
      ))
    }
  }, ignoreInit = TRUE)
  #### Copiar y compartir la simulación
  observeEvent(input$confirm_add_blob, {
    removeModal()
    share_path <- paste0(simulator_base_path, '/', input$selected_user)
    if (input$selected_share_tree != 'home') {
      selected_share_tree <- read_simulation(input$selected_share_tree, share_path)
    }
    if (input$selected_share_tree == 'home' || isTRUE(is_unlocked(selected_share_tree))) {
      ## Agregar blob a al tree
      if (input$selected_share_tree == 'home') {
        ## Eliminar clashes manualmente
        if (!identical(clashes(), FALSE)) {
          walk(clashes(), ~unlink(simulation_path(.x, share_path), recursive = TRUE))
        }
        ## Copiar simulaciones manualmente
        file.copy(
          from = simulation_path(selected_sim(), simulator_current_path()),
          to = share_path,
          overwrite = FALSE,
          recursive = TRUE
        )
      } else {
        add_children(
          x = selected_share_tree,
          dir = share_path,
          children = list(selected_sim()),
          children_dir = simulator_current_path(),
          overwrite_children = TRUE
        )
      }
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ADDED BLOB TO TREE',
        details = list(
          from = list(user = credentials()$user, simulation = selected_sim()$path),
          to = list(user = input$selected_user, tree = input$selected_share_tree)
        )
      )))
      ## Volver a cargar simulaciones
      if (input$selected_user == credentials()$user) {
        when_finished$confirm_add_blob <- when_finished$confirm_add_blob + 1
      }
    } else {
      showModal(modalDialog(
        easyClose = TRUE,
        title = 'Error al enviar escenario',
        ifelse(is_unlocked(selected_share_tree) == 'locked', 'La carpeta está bloqueada.', 'Se ha excedido la fecha límite para agregar escenarios a esa carpeta.'),
        footer = tagList(
          modalButton(lang$ok)
        )
      ))
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ERROR ADDING BLOB TO TREE',
        details = list(
          from = list(user = credentials()$user, blob = selected_sim()$path),
          to = list(user = input$selected_user, tree = input$selected_share_tree)
        )
      )))
    }
  }, ignoreInit = TRUE)
  
  
  ### Eliminar
  #### Checar que no haya referencias a la simulación
  observeEvent(input$delete_sim, {
    ns <- session$ns
    req(is.simulation(selected_sim()))
    if (!simulation_exists(selected_sim(), simulator_current_path())) {
      dial <- modalDialog(
        title = 'Error eliminando escenario',
        tags$p('No existe el escenario.'),
        footer = tagList(
          modalButton(lang$ok)
        ),
        easyClose = TRUE
      )
    } else {
      dial <- modalDialog(
        title = '¿Eliminar escenario?',
        tags$p('Esta acción no se puede deshacer.'),
        footer = tagList(
          actionButton(ns('confirm_delete_sim'), label = lang$delete, class = 'btn-danger'),
          modalButton(lang$cancel)
        ),
        easyClose = TRUE
      )
    }
    showModal(dial)
  })
  #### Eliminar simulación
  observeEvent(input$confirm_delete_sim, {
    removeModal()
    tryCatch({
      delete_simulation(selected_sim(), simulator_current_path())
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DELETED SIMULATION DATA',
        details = list(
          status = 'SUCCESS',
          file = simulation_path(selected_sim(), simulator_current_path())
        )
      )))
    }, error = function(e){
      futile.logger::flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ERROR DELETING SIMULATION DATA',
        details = list(
          status = 'ERROR',
          file = simulation_path(selected_sim(), simulator_current_path())
        )
      )))
    })
    when_finished$confirm_delete_sim <- when_finished$confirm_delete_sim + 1
  })
  
  
  ### Cargar
  loaded_sim <- eventReactive(input$load_sim, {
    if (!is.simulation(selected_sim())) {
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'SIMULATION DATA NOT AVAILABLE',
        details = list(
          status = 'ERROR'
        )
      )))
    }
    selected_sim()
  })
  observeEvent(loaded_sim(), { # si el trigger es NULL, se ignora
    tryCatch({
      if (is.tree(loaded_sim())) {
        sim <- as.blob(loaded_sim(), simulator_current_path(), write = FALSE)$simulation
      } else {
        sim <- loaded_sim()$simulation
      }
      # ns <- session$ns
      for (s in all_simulators) {
        updateNumericInput(session, s, value = ifelse(is.null(sim[[s]]) | is.na(sim[[s]]), 0, sim[[s]]))
      }
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'DONE LOADING SIMULATION DATA',
        details = list(
          status = 'SUCCESS',
          file = simulation_path(loaded_sim(), simulator_current_path())
        )
      )))
    }, error = function(e){
      flog.info(toJSON(list(
        session_info = msg_cred(credentials()),
        message = 'ERROR LOADING SIMULATION DATA',
        details = list(
          status = 'ERROR',
          file = simulation_path(loaded_sim(), simulator_current_path())
        )
      )))
    })
  })
  
  reactive({
    loaded_sim()$filters
  })
}

waterfallUI <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    title = "Gráfica",
    fluidPage(
      fluidRow(
        plotlyOutput(ns("grafica")) %>% withSpinner(type = 8),
        tags$div(
          class = 'inline-inputs small-margin',
          style = 'margin-left: 15px',
          downloadButton(ns("download_waterfall"), label = lang$download_data),
          tags$strong(p(lang$money_units, class = 'footnote'))
        )
      ),
      fluidRow(
        box(
          width = 12,
          status = "primary",
          class = 'hovering-controls',
          h2('Simulador'),
          div(
            class = 'simulator-grid',
            numericInput(
              inputId = ns("sim_forecast_promocional"),
              label = h4(lang$forecast_promocional),
              min = 0,
              step = 1e6 / precision_numeros,
              value = 0
            ),
            numericInput(
              inputId = ns("sim_no_resurtible"),
              label = h4(lang$no_resurtible),
              min = 0,
              step = 1e6 / precision_numeros,
              value = 0
            ),
            numericInput(
              inputId = ns("sim_fulfillment"),
              label = h4(lang$fulfillment),
              min = 0,
              step = 1e6 / precision_numeros,
              value = 0
            ),
            tags$div(
              uiOutput(ns('selected_sim_ui')),
              checkboxInput(ns('show_all_sims'), 'Mostrar todo', value = FALSE)
            ),
            htmlOutput(ns('selected_sim_details')),
            htmlOutput(ns('selected_sim_path'))
          ),
          tags$div(
            class = 'inline-button-groups',
            tags$div(
              class = 'inline-buttons',
              actionButton(ns('save_tree'), lang$save_tree, icon = icon('folder-plus')),
              actionButton(ns('save_sim'), lang$save_sim, icon = icon('save')),
              actionButton(ns('load_sim'), lang$load_sim, icon = icon('redo-alt')),
              actionButton(ns('delete_sim'), lang$delete_sim, icon = icon('minus-circle'))
            ),
            tags$div(
              class = 'inline-buttons',
              actionButton(ns('get_children'), lang$get_children, icon = icon('arrow-down')),
              actionButton(ns('get_parent'), lang$get_parent, icon = icon('arrow-up')),
              actionButton(ns('share_tree'), lang$share_tree, icon = icon('lock'))
            ),
            tags$div(
              class = 'inline-buttons',
              actionButton(ns('add_blob'), lang$add_blob, icon = icon('share-alt'))
            )
          )
        )
      )
    )
  )
  
}
