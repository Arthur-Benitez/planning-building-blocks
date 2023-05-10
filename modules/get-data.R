get_frozen_files <- function(forecast_files, latest_refresh_dates, exceptions, forecast_frozen_months) {
  # Obtener la actualización correcta de los meses congelados
  valid_forecast_files <- forecast_files %>% 
    keep(str_detect(., paste(unlist(latest_refresh_dates), collapse = '|'))) %>% 
    discard(str_detect(., paste(exceptions, collapse = '|')))
  
  valid_refresh_dates <- as.numeric(str_extract(valid_forecast_files, '\\d{6}'))
  valid_ym <- as.numeric(str_extract(valid_forecast_files, '(?<=\\d{8}\\/)\\d{6}'))
  
  frozen_forecast <- valid_forecast_files %>% 
    keep(valid_ym - valid_refresh_dates <= forecast_frozen_months)
  
  frozen_forecast
}

# Forecast files selection ------------------------------------------------

# Flaws:
# Más de una actualización por dia
all_forecast_files <- list.files(
  path = fcst_folder, 
  full.names = TRUE, 
  recursive = TRUE
) %>% 
  sort(decreasing = TRUE)

all_refresh_dates <- all_forecast_files %>% 
  split(str_extract(., '\\d{6}')) %>% 
  map(~unique(str_extract(.x, '\\d{8}')))

latest_refresh_dates <- map(all_refresh_dates, ~.x[1])
this_iteration <- unlist(latest_refresh_dates[as.character(dates$this_ym)])
last_iteration <- unlist(latest_refresh_dates[as.character(dates$last_ym)])

this_forecast <- all_forecast_files %>% 
  keep(str_detect(., this_iteration))

current_iteration_forecast_files <- c(
  this_forecast, 
  get_frozen_files(
    forecast_files = all_forecast_files, 
    latest_refresh_dates = latest_refresh_dates, 
    exceptions = this_iteration, 
    forecast_frozen_months = forecast_frozen_months
  )
)

if (!is.null(last_iteration)) {
  last_forecast <- all_forecast_files %>% 
    keep(str_detect(., last_iteration))
  
  last_iteration_forecast_files <- c(
    last_forecast, 
    get_frozen_files(
      forecast_files = all_forecast_files, 
      latest_refresh_dates = latest_refresh_dates, 
      exceptions = list(this_iteration, last_iteration), 
      forecast_frozen_months = forecast_frozen_months
    )
  )
} else {
  last_iteration_forecast_files <- current_iteration_forecast_files
}

# Sales files selection ---------------------------------------------------

all_sales_files <- list.files(
  path = sales_folder, 
  full.names = TRUE, 
  recursive = TRUE
) %>% 
  sort()

sales_ly_files <- all_sales_files %>% 
  keep(str_detect(., paste(dates$last_year_yms, collapse = '|')))

sales_ytd_files <- all_sales_files %>% 
  keep(str_detect(., paste(dates$this_year_past_yms, collapse = '|')))


#  Extended Assortment sales files selection ------------------------------

all_sales_ea_files <- list.files(
  path = sales_ea_folder, 
  full.names = TRUE, 
  recursive = TRUE
) %>% 
  sort()

sales_ea_ly_files <- all_sales_ea_files %>% 
  keep(str_detect(., paste(dates$last_year_yms, collapse = '|')))

sales_ea_ytd_files <- all_sales_ea_files %>% 
  keep(str_detect(., paste(dates$this_year_past_yms, collapse = '|')))

# BP files selection ------------------------------------------------------

all_bp_files <- list.files(
  path = bp_folder, 
  full.names = TRUE, 
  recursive = TRUE
) %>% 
  sort()

bp_files <- all_bp_files %>% 
  keep(str_detect(., paste(dates$this_year_yms, collapse = '|')))

current_files <- sort(c(current_iteration_forecast_files, last_iteration_forecast_files, sales_ly_files, sales_ytd_files, sales_ea_ly_files, sales_ea_ytd_files, bp_files))

valid_existing_file <- FALSE
if (file.exists(result_metadata_file)) {
  past_files <- sort(read_rds(result_metadata_file))
  if (all(past_files == current_files)){
    valid_existing_file <- TRUE
  }
}

process_info <- function(){
  # Leer el que ya está si es válido
  if (
    file.exists(result_data_file) & 
    valid_existing_file & 
    month(file.info(result_data_file)$mtime) == dates$this_month & 
    !override_data_file
  ) {
    complete_data <- read_rds(result_data_file)
  } else {
    # Forecast files reading --------------------------------------------------

    ytg_ci_data <- current_iteration_forecast_files %>% 
      map(readRDS) %>% 
      bind_rows() %>% 
      rename_with(~paste('ci', .x, sep = '_'), starts_with('forecast'))
    
    ytg_li_data <- last_iteration_forecast_files %>%
      map(readRDS) %>% 
      bind_rows() %>% 
      rename_with(~paste('li', .x, sep = '_'), starts_with('forecast'))
    
    # Sales files reading -----------------------------------------------------
    
    sales_ly_data <- sales_ly_files %>% 
      map(readRDS) %>% 
      bind_rows() %>% 
      rename_with(~str_replace(.x, '_', '_ly_'), starts_with('sales'))
    
    sales_ytd_data <- sales_ytd_files %>% 
      map(readRDS) %>% 
      bind_rows() %>% 
      rename_with(~str_replace(.x, '_', '_ytd_'), starts_with('sales'))
    
    # Extended Assortment sales files reading ---------------------------------
    
    sales_ea_ly_data <- sales_ea_ly_files %>% 
      map(readRDS) %>% 
      bind_rows() %>% 
      rename_with(~str_replace(.x, '_', '_ea_ly_'), starts_with('sales')) 
    
    sales_ea_ytd_data <- sales_ea_ytd_files %>% 
      map(readRDS) %>% 
      bind_rows() %>% 
      rename_with(~str_replace(.x, '_', '_ea_ytd_'), starts_with('sales')) 
    
    # BP files reading --------------------------------------------------------

    bp_data <- bp_files %>%
      map(readRDS) %>%
      bind_rows()
    
    # Reading & processing ----------------------------------------------------
    
    complete_data <- list(ytg_ci_data, ytg_li_data, sales_ly_data, sales_ytd_data, sales_ea_ly_data, sales_ea_ytd_data, bp_data) %>% 
      bind_rows() %>% 
      mutate(
        across(
          contains(c('sales', 'forecast', 'bp')),
          replace_na, 0
        ),
        ibp_ytg_cost = ci_forecast_base_cost + ci_forecast_promo_cost,
        ibp_ytg_eaches = ci_forecast_base_eaches + ci_forecast_promo_eaches,
        ibp_last_iteration_cost = li_forecast_base_cost + li_forecast_promo_cost,
        ibp_last_iteration_eaches = li_forecast_base_eaches + li_forecast_promo_eaches,
        ytd_ytg_cost = sales_ytd_cost + ibp_ytg_cost + sales_ea_ytd_cost,
        ytd_ytg_eaches = sales_ytd_eaches + ibp_ytg_eaches + sales_ea_ytd_eaches
      )

    # Quitar las cols innecesarias para ahorrar espacio
    complete_data <- complete_data %>% 
      select(-ends_with(c('vnpk', 'whpk')), -starts_with('li'))
    
    write_rds(complete_data, result_data_file)
    write_rds(sort(current_files), result_metadata_file)
  }
  
  # Actualizar o crear el archivo de las opciones si es necesario
  if (!file.exists(choices_file)){
    # Las opciones para la interfaz de los filtros
    unique_choices <- list()
    
    for (i in 1:length(ibp_filters)) {
      unique_choices[[i]] <- complete_data %>% 
        select(ibp_filters) %>% 
        magrittr::extract2(i) %>% 
        unique() %>% 
        sort()
    }
    
    unique_choices %>% 
      set_names(ibp_filters) %>% 
      write_rds(choices_file)
  }
  
  return(complete_data)
}
