demandServer <- function(input, output, session, filtered_data) {
  
  flog.info(toJSON(list(session_info = list(), message = "DONE READING DATA DEMAND PLAN", details = list() )))
  
  base_demand <- reactive(
    filtered_data()$data %>% 
      group_by(squad, cat_name) %>% 
      summarise(
        across(
          c(sales_ly_eaches, sales_ly_cost, sales_ytd_eaches, sales_ytd_cost, ibp_ytg_eaches, ibp_ytg_cost, bp_cost, bp_eaches),
          sum,
          na.rm = TRUE
        ),
        .groups = 'drop'
      ) %>% 
      mutate(
        ytd_ibp_ytg_cost = sales_ytd_cost + ibp_ytg_cost,
        ytd_ibp_ytg_eaches = sales_ytd_eaches + ibp_ytg_eaches,
        price_sales_ly = sales_ly_cost / sales_ly_eaches,
        price_bp = bp_cost / bp_eaches,
        price_ibp = ytd_ibp_ytg_cost / ytd_ibp_ytg_eaches
      ) %>% 
      select(squad, cat_name, contains(filtered_data()$metric), contains('price')) %>% 
      rename_with(~str_remove(.x, '_eaches|_cost')) %>% 
      mutate(
        fy_vs_sales_ly = ytd_ibp_ytg - sales_ly, 
        fy_vs_sales_ly_p = fy_vs_sales_ly / sales_ly,
        fy_vs_bp = ytd_ibp_ytg - bp,
        fy_vs_bp_p = fy_vs_bp / bp
      ) %>% 
      select(squad, cat_name, sales_ly, sales_ytd, ibp_ytg, ytd_ibp_ytg, bp, fy_vs_sales_ly:fy_vs_bp_p, contains('price')) %>% 
      set_names(get_pretty_names(names(.))) %>% 
      sapply(print.numeric)
  )
  
  output$plan_demanda <- renderDataTable(
    datatable(
      {
        base_demand()
      }, 
      rownames = FALSE, 
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 20,
        keys = TRUE
      )
    ) %>% 
      formatStyle(
        columns = names(base_demand()), 
        color = styleInterval(cuts = 0, values = c("red", "black"))
      )
  )
}

demandUI <- function(id) {
  ns <- shiny::NS(id)
  tabPanel(
    title = "Plan de Demanda",
    dataTableOutput(ns('plan_demanda'))
  )
}
