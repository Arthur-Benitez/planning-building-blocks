#' Obtener catálogo de unidades de negocio incluyendo Tribu y Squad
#' 
#' Input:
#'   * Consulta de SQL de Business Units
#'   
#' Output:
#'   * Archivo txt con el catálogo de VP, departamento, tribu, squad, categoría y nombre de categoría
#' 
#' Autor: Eduardo Castro <eduardo.castro0@walmart.com>
#' Colaboración: Felipe Gerard <felipe.gerard@walmart.com>
#'               Arturo Benitez <arturo.benitez@walmart.com>
#' 

eval(parse('global.R', encoding = 'UTF-8'))

ter <- odbcDriverConnect(sprintf("Driver={Teradata};DBCName=WMG;AUTHENTICATION=ldap;AUTHENTICATIONPARAMETER=%s", rstudioapi::askForPassword('User@@Password')))
raw_business_units <- readLines('etl/sql/business-units.sql')

query_business_units <- raw_business_units %>% 
  paste(collapse = '\n')

flog.info(substr(query_business_units, 1, 100))
result_business_units <- sqlQuery(ter, query_business_units)

business_units <- as_tibble(result_business_units) %>%
  set_names(tolower(names(.)))
write_tsv(business_units, 'data/business-units.txt')

## Cerrar conexión
odbcClose(ter)
