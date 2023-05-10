overviewServer <- function(input, output, session, filtered_data) {
  
  output$filtered_data <- renderText({sprintf('Filas: %d', nrow(filtered_data()$data))})
  
  base_overview <- reactive(
    filtered_data()$data %>% 
      group_by(mes) %>% 
      summarise(
        across(
          c(sales_ly_cost, sales_ly_eaches, bp_cost, bp_eaches, ytd_ytg_cost, ytd_ytg_eaches, ibp_last_iteration_cost, ibp_last_iteration_eaches),
          sum,
          na.rm = TRUE
        ),
        .groups = 'drop'
      ) %>% 
      left_join(meses, by = 'mes') %>% 
      select(-mes) %>% 
      pivot_longer(cols = -mes_name, names_to = 'value', values_to = 'costo') %>%
      pivot_wider(names_from = mes_name, values_from = costo) %>% 
      pivot_longer(!value, names_to = "mes",values_to = "values") %>%
      pivot_wider(names_from = value, values_from = values) %>%
      mutate(mes = factor(mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"))) %>%
      arrange(mes) %>%
      mutate(
        num_month = 1:12,
        month_current = ifelse(num_month == month(today()),1,0),
        days_month = days_in_month(num_month)
      )
  )
  
  base_reactivo <- reactive(
    #Datos Grafica
    base_overview() %>% 
      select(mes, num_month, month_current, days_month, contains(filtered_data()$metric)) %>% 
      set_names(str_replace(names(.), '_eaches|_cost', '')) %>%
      mutate(
        sales_day = sales_ly/days_month, 
        bp_day = bp/days_month, 
        ytd_ytg_day = ytd_ytg/days_month, 
        ytd_ytg_vs_sales_ly_abs = ifelse(ytd_ytg == 0,0,ytd_ytg-sales_ly), 
        ytd_ytg_vs_sales_ly_p = ytd_ytg_vs_sales_ly_abs/sales_ly,
        ytd_ytg_vs_bp_abs = ifelse(ytd_ytg == 0, 0,ytd_ytg-bp),
        ytd_ytg_vs_bp_p = ytd_ytg_vs_bp_abs/bp
      ) %>%
      select(-num_month, -month_current,-days_month)
  )
  
  second <- reactive({
    #Construccion de la segunda tabla
    a <- base_reactivo() %>%
      pivot_longer(!mes,names_to = "variable", values_to = "valor") %>%
      pivot_wider(names_from = mes, values_from = valor)
    
    b <- base_overview() %>%
      mutate(price_sales_ly = sales_ly_cost/sales_ly_eaches,
             price_bp = bp_cost/sales_ly_eaches,
             price_ibp = ytd_ytg_cost/sales_ly_eaches) %>%
      select(mes,contains("price")) %>% 
      pivot_longer(!mes,names_to = "variable", values_to = "valor") %>%
      pivot_wider(names_from = mes, values_from = valor)
    
    second_table <- rbind(a,b) %>% 
      mutate(pretty_name = get_pretty_names(variable)) %>% 
      select(pretty_name, everything(), -variable) %>%
      mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
      sapply(print.numeric)
    
    second_table
  })
  
  first <- reactive({
    #Construccion first table
    tmp_1 <- base_overview() %>% 
      mutate(month_current_ibp_sales_cost = ifelse(month_current == 1,ytd_ytg_cost-sales_ly_cost,0), #ok
             month_current_ibp_sales_cost_percent = month_current_ibp_sales_cost/sales_ly_cost,
             month_current_ibp_sales_eaches = month_current_ibp_sales_cost/sales_ly_eaches,
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_eaches/(sales_ly_cost/sales_ly_eaches)) %>%
      select(starts_with("month_current")) %>%
      filter(month_current == 1) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "Mes actual - IBP vs AA") %>%
      select(-month_current)
    
    tmp_2 <- base_overview() %>% 
      mutate(month_current_ibp_sales_cost = ifelse(month_current == 1, ytd_ytg_cost-bp_cost,0),
             month_current_ibp_sales_cost_p = month_current_ibp_sales_cost/bp_cost,
             month_current_ibp_sales_cost_eaches = month_current_ibp_sales_cost/bp_eaches,
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_cost_eaches/(bp_cost/bp_eaches)) %>%
      select(starts_with("month_current")) %>%
      filter(month_current == 1) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "Mes actual - IBP vs BP") %>% 
      select(-month_current, -variable)
    
    tmp_3 <- base_overview()[0:dates$this_month,] %>%
      summarize_if(is.numeric, sum, na.rm=TRUE) %>%
      mutate(month_current_ibp_sales_cost = ytd_ytg_cost-sales_ly_cost,
             month_current_ibp_sales_cost_percent = month_current_ibp_sales_cost/sales_ly_cost,
             month_current_ibp_sales_cost_eaches = ytd_ytg_eaches-sales_ly_eaches,
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_cost_eaches/sales_ly_eaches) %>%
      select(starts_with("month_current")) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "YTD vs AA") %>% 
      select(-month_current, -variable)
    
    tmp_4 <- base_overview()[1+dates$this_month:11,] %>%
      summarize_if(is.numeric, sum, na.rm=TRUE) %>%
      mutate(month_current_ibp_sales_cost = ibp_last_iteration_cost - sales_ly_cost,
             month_current_ibp_sales_cost_percent = month_current_ibp_sales_cost/sales_ly_cost,
             month_current_ibp_sales_cost_eaches = ibp_last_iteration_eaches-sales_ly_eaches,
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_cost_eaches/sales_ly_eaches) %>% 
      select(starts_with("month_current")) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "YTG vs AA") %>% 
      select(-month_current, -variable)
    
    tmp_5 <- base_overview()[0:dates$this_month,] %>%
      summarize_if(is.numeric, sum, na.rm=TRUE) %>%
      mutate(month_current_ibp_sales_cost = ytd_ytg_cost - bp_cost,
             month_current_ibp_sales_cost_percent = month_current_ibp_sales_cost/bp_cost,
             month_current_ibp_sales_cost_eaches = ytd_ytg_eaches - bp_eaches,
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_cost_eaches/bp_eaches) %>% 
      select(starts_with("month_current")) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "YTD vs BP") %>% 
      select(-month_current, -variable)
    
    tmp_6 <- base_overview()[1+dates$this_month:11,] %>%
      summarize_if(is.numeric, sum, na.rm=TRUE) %>%
      mutate(month_current_ibp_sales_cost = ibp_last_iteration_cost - bp_cost,
             month_current_ibp_sales_cost_percent = month_current_ibp_sales_cost/bp_cost,
             month_current_ibp_sales_cost_eaches = ibp_last_iteration_eaches - bp_eaches,
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_cost_eaches/bp_eaches) %>% 
      select(starts_with("month_current")) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "YTG vs BP") %>% 
      select(-month_current, -variable)
    
    tmp_7 <- data.frame(
      month_current = 1,
      month_current_ibp_sales_cost = sum(base_overview()$ytd_ytg_cost) - sum(base_overview()$sales_ly_cost),
      month_current_ibp_sales_cost_eaches = sum(base_overview()$ytd_ytg_eaches) - sum(base_overview()$sales_ly_eaches)) %>%
      mutate(month_current_ibp_sales_cost_percent = month_current_ibp_sales_cost/sum(base_overview()$sales_ly_cost),
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_cost_eaches/sum(base_overview()$sales_ly_eaches)) %>%
      select(month_current,month_current_ibp_sales_cost,month_current_ibp_sales_cost_percent,month_current_ibp_sales_cost_eaches,month_current_ibp_sales_eaches_p) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "FY vs AA") %>% 
      select(-month_current,-variable)
    
    tmp_8 <- data.frame(
      month_current = 1,
      month_current_ibp_sales_cost = sum(base_overview()$ytd_ytg_cost) - sum(base_overview()$bp_cost),
      month_current_ibp_sales_cost_eaches = sum(base_overview()$ytd_ytg_eaches) - sum(base_overview()$bp_eaches)) %>%
      mutate(month_current_ibp_sales_cost_percent = month_current_ibp_sales_cost/sum(base_overview()$bp_cost),
             month_current_ibp_sales_eaches_p = month_current_ibp_sales_cost_eaches/sum(base_overview()$bp_eaches)) %>%
      select(month_current,month_current_ibp_sales_cost,month_current_ibp_sales_cost_percent,month_current_ibp_sales_cost_eaches,month_current_ibp_sales_eaches_p) %>%
      pivot_longer(!month_current, names_to = "variable",values_to = "FY vs BP") %>% 
      select(-month_current,-variable)
    
    cbind(tmp_1,tmp_2,tmp_3,tmp_4,tmp_7,tmp_5,tmp_6,tmp_8) %>%
      mutate(pretty_name = get_pretty_names(variable)) %>% 
      select(pretty_name, everything(), -variable) %>%
      sapply(print.numeric)
  })
  
  #Outputs
  output$first_table <- renderDataTable(
    datatable(
      {
        first()
      }, 
      rownames = FALSE, 
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 100
      )
    )
  ) 
  
  output$second_table <- renderDataTable(
    datatable(
      {
        second()
      }, 
      rownames = FALSE, 
      options = list(
        scrollX = TRUE, 
        scrollY = TRUE,
        pageLength = 100
      )
    )
  )
                                    
  output$graph <- renderPlotly({
    flog.info(toJSON(list(session_info = list(),message = "LOADING PLOT OVERVIEW",details = list() )))
    
    plot_ly(data = base_reactivo(), 
                   x = ~mes,
                   hoverinfo = 'text') %>%
      add_trace(y = ~sales_ly,
                name = 'Venta AA',
                marker = list(color = '#78b9e7'),
                text = ~sprintf("%s", scales::comma(sales_ly))) %>%
      add_trace(y = ~bp,
                name = 'Bussines Plan (BP)',
                marker = list(color = '#0f1c2c'),
                text = ~sprintf("%s", scales::comma(bp))) %>%
      add_lines(y = ~ytd_ytg, 
                name = 'YTD + YTG', 
                marker = list(color = "#f47521"),
                line = list(color = '#f47521', width = 3),
                text = ~sprintf("%s", scales::comma(ytd_ytg)) ) %>%
      add_lines(y = ~ibp_last_iteration, 
                name = 'IBP Last Iteration', 
                marker = list(color = "#ffc220"),
                line = list(color = '##ffc220', width = 3),
                text = ~sprintf("%s", scales::comma(ibp_last_iteration)) ) %>% 
      layout(
        title = '',
        xaxis = list(title = '', tickangle = 0),
        yaxis = list(
          title = '',
          range = c(
            min(base_reactivo()$sales_ly, base_reactivo()$bp, base_reactivo()$ytd_ytg) / 1.5,
            max(base_reactivo()$sales_ly, base_reactivo()$bp, base_reactivo()$ytd_ytg) / 0.90
          ),
          tickformat = ',2.f'
        ),
        legend = list(x = 0.35, y = -0.15, orientation = 'h'),
        margin = list(r = -2),
        hovermode = 'compare',
        shapes = lines
      )
    })
}

overviewUI <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    title = "Overview",
    textOutput(ns("filtered_data")),
    plotlyOutput(ns('graph')),
    br(),br(),
    dataTableOutput(ns('first_table')),
    br(),br(),
    dataTableOutput(ns('second_table')))}
