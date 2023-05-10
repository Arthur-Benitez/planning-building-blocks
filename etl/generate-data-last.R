#' Obtener los datos de forecast y ventas del mes anterior
#' requeridos para alimentar la aplicación
#' 
#' Input:
#'   * Consultas de SQL:
#'      * business_units.SQL
#'      * ventas.SQL
#'   * Archivo BP.xlsx (Business Plan)
#'   
#' Output:
#'   * Archivo csv con el resumen obtenido a nivel vicepresidencia, departamento, categoría, 
#'   formato, fecha de cálculo y ym (year-month)
#'   * Archivo csv con el detalle de los datos a nivel artículo formato
#' 
#' Autor: Eduardo Castro <eduardo.castro0@walmart.com>
#' Colaboración: Robert Paniagua <mrpania@walmart.com>
#'               Felipe Gerard <felipe.gerard@walmart.com>
#' 

if (lubridate::month(lubridate::today() - 7) != lubridate::month(lubridate::today())) {
  
  params <- list()
  
  if (!'mlutils' %in% installed.packages()[ , 1]) {
    params$teradata_connector <- NULL
    params$teradata_UID <- rstudioapi::askForPassword('Teradata User')
    params$teradata_PWD <- rstudioapi::askForPassword('Teradata Password')
    params$n_cores <- parallel::detectCores() - 2
  } else {
    if (!interactive()) {
      Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc", LANG = "en_US.UTF-8")
      setwd('/home/rstudio/building-blocks-app')
    }
    params$teradata_connector <- 'teradata-production-connector'
    params$teradata_UID <- NULL
    params$teradata_PWD <- NULL
    params$n_cores <- 1
  }
  
  eval(parse('global.R', encoding = 'UTF-8'))
  options(stringsAsFactors = FALSE)
  
  ## Leer los queries desde archivos SQL
  raw_ventas <- readLines('etl/sql/ventas-pasado.sql')
  raw_business_units <- readLines('etl/sql/business-units.sql')
  
  ## Parámetros
  wm_month <- -1
  query_params_lm <- c('\\?NUM_PAST_MONTHS' = as.character(abs(wm_month))) # El query ya lo hace negativo
  query_params_ly <- c('\\?NUM_PAST_MONTHS' = as.character(abs(12 - wm_month))) # El query ya lo hace negativo
  
  ## Procesamiento de los queries
  
  ### Preparar los queries
  query_ventas <- raw_ventas %>% 
    paste(collapse = '\n') %>% 
    str_replace_all(query_params_lm)
  query_ventas_ly <- raw_ventas %>% 
    paste(collapse = '\n') %>% 
    str_replace_all(query_params_ly)
  query_business_units <- raw_business_units %>%
    paste(collapse = '\n')
  
  ## Archivos temporales
  temp_ventas <- paste0('etl/vts_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')
  temp_ventas_ly <- paste0('etl/ventas_ly_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')
  temp_business_units <- paste0('etl/business_units_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')
  
  ### Correr queries
  flog.info(substr(query_ventas, 1, 100))
  result_ventas <- run_query(
    db = 'teradata',
    UID = params$teradata_UID,
    PWD = params$teradata_PWD,
    connector = params$teradata_connector,
    query = query_ventas
  )
  write_csv(result_ventas, temp_ventas)
  
  flog.info(substr(query_ventas_ly, 1, 100))
  result_ventas_ly <- run_query(
    db = 'teradata',
    UID = params$teradata_UID,
    PWD = params$teradata_PWD,
    connector = params$teradata_connector,
    query = query_ventas_ly
  )
  write_csv(result_ventas_ly, temp_ventas_ly)
  
  flog.info(substr(query_business_units, 1, 100))
  result_business_units <- run_query(
    db = 'teradata',
    UID = params$teradata_UID,
    PWD = params$teradata_PWD,
    connector = params$teradata_connector,
    query = query_business_units
  )
  write_csv(result_business_units, temp_business_units)
  
  ### Estructurar el resultado de los queries
  ventas <- as_tibble(result_ventas) %>% 
    set_names(tolower(names(.))) %>%
    mutate(cat_nbr = as.numeric(cat_nbr))
  
  ventas_ly <- as_tibble(result_ventas_ly) %>% 
    set_names(tolower(names(.))) %>%
    group_by(departamento = dept_nbr, categoria = cat_nbr, formato) %>% 
    summarise(
      ventas_ly = sum(pos_sales, na.rm = TRUE),
      ventas_ly_piezas = sum(pos_qty, na.rm = TRUE)
    ) %>% 
    ungroup() %>%
    mutate(categoria = as.numeric(categoria))
  
  if (is.data.frame(result_business_units)) {
    business_units <- as_tibble(result_business_units) %>%
      set_names(tolower(names(.))) %>%
      mutate(categoria = as.numeric(categoria))
    
    # Si la ultima version fue modificada hace mas de 2 dias, lo vuelve a guardar
    if (file.info('data/business-units.txt')$mtime <= today()-2) {
      write_tsv(business_units, 'data/business-units.txt')
      write_tsv(business_units, paste0('data/business-units_', format(today(), '%Y%m%d'), '.txt'))
    }
  } else {
    business_units <- read_tsv('data/business-units.txt') %>%
      set_names(tolower(names(.))) %>%
      mutate(categoria = as.numeric(categoria))
  }
  
  squads <- business_units %>%
    filter(!is.na(tribu)) %>%
    select(departamento, categoria, tribu, squad) %>%
    rename(
      dept_nbr = departamento,
      cat_nbr = categoria
    )
  
  ## Leer el Business Plan
  bp_mes <- year(today() %m+% months(wm_month)) %>% 
    add(0:1) %>% 
    read_bp() %>% 
    filter(mes == as.numeric(format(today() %m+% months(wm_month), "%Y%m")))
  
  ## Unir datos con los diferentes inputs
  result <- ventas %>%
    bind_rows(bp_mes) %>%
    left_join(squads, by = c('dept_nbr', 'cat_nbr')) %>%
    mutate(
      item1_desc = str_replace_all(item1_desc, '\\W', ''),
      fecha_calculo = today(),
      ym = as.numeric(format(today() %m+% months(wm_month), "%Y%m"))
    ) %>%
    rename(
      departamento = dept_nbr,
      categoria = cat_nbr,
      ventas_piezas = pos_qty,
      ventas = pos_sales
    )
  
  
  ## Sumarizar para obtener el archivo de resumen
  result_summary <- result %>%
    group_by(., departamento, categoria, formato, tribu, squad, fecha_calculo, ym) %>%
    summarise_at(
      vars(
        bp,
        ventas,
        ventas_piezas
      ),
      ~ sum(as.numeric(.x), na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    left_join(ventas_ly, by = c('departamento', 'categoria', 'formato')) %>%
    replace_na(list(ventas_ly = 0, ventas_ly_piezas = 0))
  
  ## Exportar resultados
  ### Crear directorios si no existen
  det_dir <- get_directory(group = 'detail', date = today() %m+% months(wm_month))
  if(!dir.exists(det_dir)) dir.create(det_dir)
  sum_dir <- get_directory(group = 'summary', date = today() %m+% months(wm_month))
  if(!dir.exists(sum_dir)) dir.create(sum_dir)
  
  ## Escribir archivos de salida
  write_csv(result_summary, get_filename(dir = sum_dir, group = 'summary', date = today() %m+% months(wm_month)))
  write_csv(result, get_filename(dir = det_dir, group = 'detail', date = today() %m+% months(wm_month)))
  
  
  ## Eliminar archivos temporales
  file.remove(temp_ventas)
  file.remove(temp_ventas_ly)
  file.remove(temp_business_units)
  
}

