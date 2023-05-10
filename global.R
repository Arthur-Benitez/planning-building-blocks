
library(plotly)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(ggthemes)
library(shinyjs)
library(shinycssloaders)
library(futile.logger)
library(magrittr)
library(jsonlite)
library(shinyWidgets)
library(lubridate)
library(RODBC)
library(stringi)

## readr necesita esto para funcionar en estas computadoras
options(readr.default_locale=readr::locale(tz = ''))

## Loggear session info al levantar el deployment
flog.info(toJSON(list(
  message = "R SESSION INFO",
  details = list(
    session_info = paste(capture.output(sessionInfo()), collapse = '\n')
  )
)))

## Asegurar que los locales están bien seteados
if (Sys.info()[['sysname']] != 'Windows') {
  flog.info(toJSON(list(
    message = "INITIAL R LOCALE",
    details = list(
      locale = Sys.getlocale()
    )
  )))
  tribble(
    ~category, ~locale,
    "LC_CTYPE", "en_US.UTF-8",
    "LC_NUMERIC", "C",
    "LC_TIME", "en_US.UTF-8",
    "LC_COLLATE", "en_US.UTF-8",
    "LC_MONETARY", "en_US.UTF-8",
    "LC_MESSAGES", "en_US.UTF-8"
  ) %>% 
    pwalk(function(category, locale){
      tryCatch({
        old_locale <- Sys.getlocale(category)
        if (locale != old_locale) {
          flog.info(toJSON(list(
            message = "CHANGING LOCALE",
            details = list(
              category = category,
              locale = old_locale
            )
          )))
          Sys.setlocale(category, locale)
          flog.info(toJSON(list(
            message = "CHANGED LOCALE",
            details = list(
              category = category,
              locale = list(
                old = old_locale,
                new = locale
              )
            )
          )))
        }
      }, error = function(e){
        flog.info(toJSON(list(
          message = "FAILED CHANGING LOCALE",
          details = list(
            category = category,
            locale = old_locale
          )
        )))
      })
    })
}
flog.info(toJSON(list(
  message = "R LOCALE",
  details = list(
    locale = Sys.getlocale()
  )
)))

## Estamos trabajando localmente o en producción?
if (dir.exists('prod')) {
  app_deployment_environment <- 'prod'
} else if (dir.exists('dev')) {
  app_deployment_environment <- 'dev'
}

app_version <- '0.6.2'
app_version_date <- '2020-11-04'

# Parámetros globales -----------------------------------------------------

## Columnas requeridas en input (case-insensitive)
req_cols <- c(
  'departamento',
  'categoria',
  'tribu',
  'squad',
  'formato',
  'fecha_calculo',
  'ym',
  'bp',
  'forecast_base',
  'forecast_promocional',
  'ventas',
  'presentation',
  'display',
  'ventas_ly',
  'forecast_base_piezas',
  'forecast_promocional_piezas',
  'ventas_piezas',
  'presentation_piezas',
  'display_piezas',
  'ventas_ly_piezas'
)

## Valor para seleccionar todos los departamentos, categorías o formatos
nombre_todos <- 'Todos'

## Formatos de negocio (como vienen --> como queremos que se vean)
formatos <- c(
  nombre_todos,
  "BODEGA" = "Bodega Aurrera",
  "MIBODEGA" = "Mi Bodega Aurrera",
  "BAE" = "Bodega Aurrera Express",
  "SUPERAMA" = "Superama",
  "SUPERCENTER" = "Supercenter"
)
names(formatos)[formatos == nombre_todos] <- nombre_todos

## Formatos de negocio cortos (bonitos --> cortos)
formatos_short <- c(
  nombre_todos,
  "Bodega Aurrera" = 'BOD',
  "Mi Bodega Aurrera" = 'MIB',
  "Bodega Aurrera Express" = 'BAE',
  "Superama" = 'SUP',
  "Supercenter" = 'SCE'
)
names(formatos_short)[formatos_short == nombre_todos] <- nombre_todos

## Redondeo a utilizar (ej. 1 = pesos, 1e3 = miles, etc)
precision_numeros <- 1

## Nombres de todos los filtros
time_filters <- c('ym', 'fecha_calculo')
time_pred_filters <- 'ym'
business_unit_filters <- c('departamento', 'tribu', 'squad', 'categoria', 'formato')
all_filters <- c(time_filters, business_unit_filters)

ibp_filters <- c('tribu', 'squad', 'canal', 'marcas_estrategicas', 'vendor_nbr', 'dept_nbr', 'clan', 'cat_nbr', 'estado', 'nodo', 'reg_nielsen', 'formato')

# Folders para guardar la información actualizada
temp_folder <- 'data/temp'
fcst_folder <- 'data/forecast'
sales_folder <- 'data/sales'
sales_ea_folder <- 'data/sales_ea'
bp_folder <- 'data/bp'
result_data_file <- 'data/test2'
result_metadata_file <- 'data/complete_data_metadata'
choices_file <- 'data/unique_choices'

# Meses de anticipación con los que se congela el forecast
forecast_frozen_months <- 2
# Parámetro para forzar la sobreescritura de complete_data
override_data_file <- FALSE

run_date <- ymd('2021-05-01')
# Fechas útiles
dates <- list(
  this_year = year(run_date),
  last_year = year(run_date) - 1,
  this_month = month(run_date),
  this_ym = as.numeric(format(run_date, "%Y%m")),
  last_ym = as.numeric(format(run_date - months(1), "%Y%m")),
  # Para las carpetas de guardado
  last_year_yms = (year(run_date) - 1) * 100  + 1:12,
  this_year_yms = (year(run_date)) * 100  + 1:12,
  this_year_past_yms = (year(run_date) * 100) + 1:(month(run_date)-1),
  this_year_future_yms = (year(run_date) * 100) + (month(run_date)+ 2):(month(run_date)+ 3),
  # Para sustituir en los queries
  this_year_begin  = floor_date(  run_date, unit = 'year'),
  this_year_end    = ceiling_date(run_date, unit = 'year')  - days(1),
  last_year_begin  = floor_date(  run_date, unit = 'year')  - years(1),
  last_year_end    = ceiling_date(run_date, unit = 'year')  - days(1) - years(1),
  last_month_begin = floor_date(  run_date, unit = 'month') - months(1),
  last_month_end   = floor_date(  run_date, unit = 'month') - days(1),
  next_month_begin = ceiling_date(run_date, unit = 'month'),
  work_month_begin = ceiling_date(run_date, unit = 'month') + months(1),
  work_month_end   = ceiling_date(run_date, unit = 'month') + months(3) - days(1)
)

mes_name_vector <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio', 'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

meses <- tibble(
  mes = 1:12,
  mes_name = mes_name_vector
)

## Nombres de todos los simuladores
all_simulators <- c('sim_forecast_promocional', 'sim_no_resurtible', 'sim_fulfillment')

## Login & Auth
### Path a base de datos de usuarios
user_data_path <- paste0(app_deployment_environment, '/etc/psswd')
### Nivel de permisos por tipo de usuario
clearance_levels <- c(
  'owner' = 0,
  'admin' = 1,
  'basic' = 2
)


# Funciones y módulos -----------------------------------------------------

## Usar esto en lugar de source(., encoding = 'UTF-8') porque truena a menos que cambiemos el locale del sistema con Sys.setlocale('LC_CTYPE', 'en_US.UTF-8') 
## Ver: https://stackoverflow.com/questions/5031630/how-to-source-r-file-saved-using-utf-8-encoding
eval(parse('lang.R', encoding = 'UTF-8'))
eval(parse('modules/functions.R', encoding = 'UTF-8'))
eval(parse('modules/login.R', encoding = 'UTF-8'))
eval(parse('modules/filters.R', encoding = 'UTF-8'))
eval(parse('modules/waterfall.R', encoding = 'UTF-8'))
eval(parse('modules/downloads.R', encoding = 'UTF-8'))

eval(parse('modules/simulator-functions.R', encoding = 'UTF-8'))
eval(parse('modules/resumen.R', encoding = 'UTF-8'))
eval(parse('modules/usage-stats.R', encoding = 'UTF-8'))
eval(parse('modules/faq.R', encoding = 'UTF-8'))

eval(parse('modules/get-data.R', encoding = 'UTF-8'))
eval(parse('modules/filters-ibp.R', encoding = 'UTF-8'))
eval(parse('modules/overview.R', encoding = 'UTF-8'))
eval(parse('modules/demand-plan.R', encoding = 'UTF-8'))
eval(parse('modules/view-colaboration.R', encoding = 'UTF-8'))

