#' Obtener los datos de forecast y ventas del mes actual requeridos
#' requeridos para alimentar la aplicación
#' 
#' Input:
#'   * Consultas de SQL:
#'      * forecast-current.SQL
#'      * business_units.SQL
#'      * precio_futuro.SQL 
#'      * ventas.SQL
#'   * Archivo BP.xlsx (Business Plan)
#'   
#' Output:
#'   * Archivo csv con el resumen obtenido a nivel vicepresidencia, departamento, categoría, 
#'    formato, fecha de cálculo y ym (year-month)
#'   * Archivo csv con el detalle de los datos a nivel artículo formato
#' 
#' Autor: Eduardo Castro <eduardo.castro0@walmart.com>
#' Colaboración: Robert Paniagua <mrpania@walmart.com>
#'               Felipe Gerard <felipe.gerard@walmart.com>
#'               Arturo Benitez <arturo.benitez@walmart.com>
#' 

params <- list()

if (!'mlutils' %in% installed.packages()[ , 1]) {
  params$teradata_connector <- NULL
  params$dsn5_connector <- NULL
  params$teradata_UID <- rstudioapi::askForPassword('Teradata User')
  params$teradata_PWD <- rstudioapi::askForPassword('Teradata Password')
  params$dsn5_UID <- rstudioapi::askForPassword('DSN5 User')
  params$dsn5_PWD <- rstudioapi::askForPassword('DSN5 Password')
  params$n_cores <- parallel::detectCores() - 2
} else {
  if (!interactive()) {
    Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio-server/bin/pandoc", LANG = "en_US.UTF-8")
    setwd('/home/rstudio/building-blocks-app')
  }
  params$teradata_connector <- 'teradata-production-connector'
  params$dsn5_connector <- 'db2-production-connector'
  params$teradata_UID <- NULL
  params$teradata_PWD <- NULL
  params$dsn5_UID <- NULL
  params$dsn5_PWD <- NULL
  params$n_cores <- 1
}

eval(parse('global.R', encoding = 'UTF-8'))
options(stringsAsFactors = FALSE)

### Leer los queries desde archivos SQL
raw_query <- readLines('etl/sql/forecast-current.sql')
raw_precio_futuro <- readLines('etl/sql/precio_futuro.sql')
raw_ventas <- readLines('etl/sql/ventas.sql')
raw_ventas_ly <- readLines('etl/sql/ventas-pasado.sql')
raw_business_units <- readLines('etl/sql/business-units.sql')

## Parámetros para enviar a los queries
wm_month <- 0
query_params <- c('\\?NUM_FUTMONTHS' = as.character(wm_month))
query_params_ly <- c('\\?NUM_PAST_MONTHS' = as.character(abs(12 - wm_month))) # El query ya lo hace negativo

## Procesamiento de los queries

### Preparar los queries
query <- raw_query %>% 
  paste(collapse = '\n') %>% 
  str_replace_all(query_params)

query_precio_futuro <- raw_precio_futuro %>% 
  paste(collapse = '\n') %>%
  str_replace_all(query_params)

query_ventas <- raw_ventas %>% 
  paste(collapse = '\n')

query_ventas_ly <- raw_ventas_ly %>%
  paste(collapse = '\n') %>%
  str_replace_all(query_params_ly)

query_business_units <- raw_business_units %>% 
  paste(collapse = '\n')


## Archivos temporales
temp_query <- paste0('etl/qry_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')
temp_precio_futuro <- paste0('etl/prc_fut_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')
temp_ventas <- paste0('etl/vts_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')
temp_ventas_ly <- paste0('etl/ventas_ly_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')
temp_business_units <- paste0('etl/business_units_', format(today() %m+% months(wm_month), "%Y%m"),'TEMP.csv')

### Correr queries
flog.info(substr(query, 1, 100))
result_query <- run_query(
  db = 'teradata',
  UID = params$teradata_UID,
  PWD = params$teradata_PWD,
  connector = params$teradata_connector,
  query = query
)
write_csv(result_query, temp_query)

flog.info(substr(query_precio_futuro, 1, 100))
result_precio_futuro <- run_query(
  db = 'dsn5',
  UID = params$dsn5_UID,
  PWD = params$dsn5_PWD,
  connector = params$dsn5_connector,
  query = query_precio_futuro
)
write_csv(result_precio_futuro, temp_precio_futuro)

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
query <- as_tibble(result_query) %>% 
  set_names(tolower(names(.))) %>%
  mutate(cat_nbr = as.numeric(cat_nbr))
precio_futuro <- as_tibble(result_precio_futuro) %>% 
  set_names(tolower(names(.)))
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
bp_mes <- year(today()) %>% 
  add(0:1) %>% 
  read_bp() %>%
  filter(mes == as.numeric(format(today() %m+% months(wm_month), "%Y%m")))

## Unir datos con los diferentes inputs
result <- left_join(query, precio_futuro, by = c('old_nbr'='item_nbr', 'formato')) %>%
  bind_rows(bp_mes) %>%
  select(-c(count_item, mes)) %>%
  full_join(ventas, by = c('item_nbr', 'formato')) %>%
  mutate_at(
    vars(
      dept_nbr.x,
      dept_nbr.y,
      cat_nbr.x,
      cat_nbr.y,
      old_nbr.x,
      old_nbr.y,
      repl_group_nbr.x,
      repl_group_nbr.y,
      upc_nbr.x,
      upc_nbr.y,
      tipo_art.x,
      tipo_art.y,
      costo.x,
      costo.y,
      precio_fut,
      precio.x,
      precio.y,
      precio_iva_fut,
      precio_iva.x,
      precio_iva.y,
      fcst_base,
      press_qty,
      display_qty
    ), ~ as.numeric(.x)
  ) %>%
  mutate_at(
    vars(
      item1_desc.x,
      item1_desc.y
    ), ~ as.character(.x)
  ) %>%
  ### Tomar los valores no nulos de las columnas que terminan en .x y .y
  mutate(
    dept_nbr = coalesce(dept_nbr.x, dept_nbr.y),
    cat_nbr = coalesce(cat_nbr.x, cat_nbr.y),
    old_nbr = coalesce(old_nbr.x, old_nbr.y),
    repl_group_nbr = coalesce(repl_group_nbr.x, repl_group_nbr.y),
    upc_nbr = coalesce(upc_nbr.x, upc_nbr.y),
    item1_desc = coalesce(item1_desc.x, item1_desc.y),
    tipo_art = coalesce(tipo_art.x, tipo_art.y),
    costo = coalesce(costo.x, costo.y),
    precio = coalesce(precio_fut, precio.x, precio.y),
    precio_iva = coalesce(precio_iva_fut, precio_iva.x, precio_iva.y)
  ) %>%
  select(-ends_with('.x'), -ends_with('.y'), -c(precio_fut, precio_iva_fut)) %>%
  left_join(squads, by = c('dept_nbr', 'cat_nbr')) %>%
  rename(
    departamento = dept_nbr,
    categoria = cat_nbr,
    forecast_base_piezas = fcst_base,
    forecast_promocional_piezas = fcst_prom,
    display_piezas = display_qty,
    presentation_piezas = press_qty,
    venta = pos_sales,
    venta_piezas = pos_qty
  ) %>%
  mutate(
    forecast_base = forecast_base_piezas * precio,
    forecast_promocional = forecast_promocional_piezas * precio,
    presentation = presentation_piezas * precio,
    display = display_piezas * precio * 1.00000,
    item1_desc = str_replace_all(item1_desc, '\\W', ''),
    fecha_calculo = today(),
    ym = as.numeric(format(today() %m+% months(wm_month), "%Y%m"))
  )


## Sumarizar para obtener el archivo de resumen
result_summary <- result %>%
  group_by(., departamento, categoria, formato, tribu, squad, fecha_calculo, ym) %>%
  summarise_at(
    vars(
      forecast_base_piezas,
      forecast_promocional_piezas,
      venta_piezas,
      presentation_piezas,
      display_piezas,
      venta,
      bp,
      forecast_base,
      forecast_promocional,
      presentation,
      display
    ),
    ~ sum(as.numeric(.x), na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  rename(
    ventas_piezas = venta_piezas,
    ventas = venta
  ) %>% 
  left_join(ventas_ly, by = c('departamento', 'categoria', 'formato')) %>%
  replace_na(list(ventas_ly = 0, ventas_ly_piezas = 0))
  

det_dir <- get_directory(group = 'detail', date = today() %m+% months(wm_month))
if(!dir.exists(det_dir)) dir.create(det_dir)
sum_dir <- get_directory(group = 'summary', date = today() %m+% months(wm_month))
if(!dir.exists(sum_dir)) dir.create(sum_dir)

## Escribir archivos de salida
write_csv(result_summary, get_filename(dir = sum_dir, group = 'summary', date = today() %m+% months(wm_month)))
write_csv(result, get_filename(dir = det_dir, group = 'detail', date = today() %m+% months(wm_month)))

## Eliminar archivos temporales
file.remove(temp_query)
file.remove(temp_precio_futuro)
file.remove(temp_ventas)
file.remove(temp_ventas_ly)
file.remove(temp_business_units)



