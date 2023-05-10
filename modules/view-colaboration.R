colaborationServer <- function(input, output, session, filtered_data) {
  
  base_colaboration <- reactive(
    filtered_data()$data %>%
      group_by(mes, squad) %>% 
      summarise(
        across(
          c(sales_ly_cost, sales_ly_eaches, bp_cost, bp_eaches, ytd_ytg_cost, ytd_ytg_eaches, ci_forecast_base_cost, ci_forecast_base_eaches, ci_forecast_promo_cost, ci_forecast_promo_eaches),
          sum,
          na.rm = TRUE
        ),
        .groups = 'drop'
      ) %>% 
      left_join(meses, by = 'mes') %>% 
      select(squad, mes_name, contains(filtered_data()$metric)) %>% 
      rename_with(~str_remove_all(.x, 'ci_|_eaches|_cost')) %>% 
      mutate(
        ytd_ytg_vs_sales_ly_p = round((ytd_ytg - sales_ly) / sales_ly, 2),
        ytd_ytg_vs_bp_p = (ytd_ytg - bp) / bp,
        # ytd_ytg_vs_sales_ly_p = ifelse(ytd_ytg_vs_sales_ly_p == -100, 0, ytd_ytg_vs_sales_ly_p),
        # ytd_ytg_vs_bp_p = ifelse(ytd_ytg_vs_bp_p == -100, 0, ytd_ytg_vs_bp_p)
      )
  )
  
  tabla_reactivo <- reactive(
    base_colaboration() %>% 
      pivot_longer(cols = -c(mes_name, squad), names_to = 'bb', values_to = 'costo') %>%
      pivot_wider(names_from = mes_name, values_from = costo) %>% 
      mutate(pretty_bb = get_pretty_names(bb)) %>% 
      select(squad, pretty_bb, everything(), -bb) %>% 
      set_names(get_pretty_names(names(.))) %>% 
      sapply(print.numeric)
  )
  
  base_grafica <- reactive(
    base_colaboration() %>% 
      group_by(mes_name) %>% 
      summarise(across(-c(squad, ends_with('_p')), sum), .groups = 'drop') %>% 
      mutate(
        mes_name = factor(mes_name, levels = mes_name_vector),
        ytd_ytg_vs_sales_ly_p = (ytd_ytg - sales_ly) / sales_ly
      )
  )
  
  output$base <- renderDataTable(
    datatable(
      {
        tabla_reactivo()
      },
      rownames = FALSE, 
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 20
      )
    ) %>% 
      formatStyle(
        columns = names(tabla_reactivo()), 
        color = styleInterval(cuts = 0, values = c("red", "black"))
      )
  )
  
  output$grafica <- renderPlotly({
    flog.info(toJSON(list(session_info = list(), message = "LOADING COLLAB VIEW PLOT", details = list())))
    
    plot_ly(
      data = base_grafica(),
      x = ~mes_name,
      hoverinfo = 'text'
    ) %>%
      add_lines(
        y = ~sales_ly,
        name = 'Venta AA',
        marker = list(color = "#0071ce"),
        line = list(color = '#0071ce', width = 2),
        fill = 'tozeroy',
        text = ~sprintf("$%s", scales::comma(sales_ly))
      ) %>%
      add_lines(
        y = ~ytd_ytg,
        name = 'YTD + YTG',
        marker = list(color = "#ffc220"),
        line = list(color = '#ffc220', width = 4),
        text = ~sprintf("$%s", scales::comma(ytd_ytg))
      ) %>%
      add_annotations(
        x = base_grafica()$mes_name,
        y = base_grafica()$ytd_ytg,
        text = ~print_percentage(ytd_ytg_vs_sales_ly_p),
        yanchor = 'bottom',
        showarrow = FALSE
      ) %>%
      layout(
        title = '',
        xaxis = list(title = "", tickangle = 0),
        yaxis = list(
          title = '',
          range = c(min(base_grafica()$sales_ly, base_grafica()$ytd_ytg)/1.01, max(base_grafica()$sales_ly, base_grafica()$ytd_ytg)/.95),
          tickformat = '$,2.f'
        ),
        legend = list(x = .40, y = -.15, orientation = 'h'),
        margin = list(r = -2),
        hovermode = 'compare',
        shapes = lines
      )
  })
}

colaborationUI <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    title = "Vista de Colaboración",
    plotlyOutput(ns('grafica')),
    br(),br(),
    dataTableOutput(ns('base')))
}
