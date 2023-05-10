tribu_data <- read_tsv('data/business-units.txt') %>%
  filter(!is.na(tribu)) %>%
  select(departamento, categoria, tribu, squad) %>%
  mutate(
    departamento = pad_id(departamento),
    categoria = get_deptcat(departamento, pad_id(categoria))
  )
  

agregar_tribu <- function(data, tribu_data) {
  if(all(data$tribu == '0')) {
    data <- left_join(data, tribu_data, by = c('departamento', 'categoria')) %>%
      rename(
        tribu = tribu.y,
        squad = squad.y
      ) %>%
      select(-c(tribu.x, squad.x))
  }
  data
}

gen_agregar_tribu <- function(tribu_data) {
  function(data) {
    agregar_tribu(data, tribu_data)
  }
}

prepare_data_exceptions <- list(
  init = identity,
  # Agrupar formatos de bodegas para ciertos departamentos cuyo BP estaba a total Bodega
  agrupar_bodegas = function(data) {
    data %>%
      mutate(
        formato = ifelse(departamento %in% c('05', '06', '87') & formato %in% c('Mi Bodega Aurrera', 'Bodega Aurrera Express'), 'Bodega Aurrera', formato)
      )
  },
  # Agregar tribu y squad de manera retroactiva
  set_tribu = gen_agregar_tribu(tribu_data),
  # Agregar ventas y ventas_ly dummy en cero si no existen
  add_ventas_ly = function(x) {
    for (v in c('ventas_ly', 'ventas_ly_piezas')) {
      if (is.null(x[[v]])) {
        x[[v]] <- 0
      }
    }
    x
  },
  ## Filtrar departamentos
  filtrar_departamentos = function(data) {
    data <- data %>%
      filter(!departamento %in% c('49', '50', '53', '500', '75', '99'))
    return(data)
  },
  # Cambiar tribu y squads a mayúsculas sin acentos
  format_tribu = function(data) {
    data %>% mutate(
      tribu = stringi::stri_trans_general(tribu, 'LATIN-ASCII') %>%
        toupper(),
      squad = stringi::stri_trans_general(squad, 'LATIN-ASCII') %>%
        toupper()
    )
  }
)
