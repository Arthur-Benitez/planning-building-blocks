
if ('mlutils' %in% installed.packages()[ , 1] && !interactive()) {
  setwd('/home/rstudio/building-blocks-app')
}

eval(parse('global.R', encoding = 'UTF-8'))
eval(parse('etl/exceptions.R', encoding = 'UTF-8'))

## Leer un CSV y prepararlo
parse_file <- function(filename, req_cols, formatos) {
  filename %>% 
    read_csv(guess_max = 50000) %>% 
    set_names(tolower(names(.))) %>%
    init_col(req_cols) %>% 
    mutate_at(vars(departamento, categoria, ventas, ventas_piezas), list(~replace_na(., 0))) %>% 
    mutate_at(vars(tribu, squad), list(~replace_na(., '-'))) %>%
    mutate(
      formato = formatos[formato],
      departamento = pad_id(departamento),
      categoria = get_deptcat(departamento, pad_id(categoria)),
      tribu = as.character(tribu),
      squad = as.character(squad),
      bp = as.numeric(bp)
    ) %>% 
    group_by(formato, departamento, fecha_calculo, ym) %>% 
    mutate(
      no_resurtible = 0,
      #gap = (bp - (ventas + forecast_base + forecast_promocional + no_resurtible)), # Incluir gap en 'mutate_at' y 'select'
      fulfillment = presentation + display,
      fulfillment_piezas = presentation_piezas + display_piezas,
      empty_cat = any(get_cat(categoria) == '00'),
      empty_bp = all(bp == 0),
      bp = case_when(
        empty_bp ~ as.numeric(NA),
        empty_cat & get_cat(categoria) != '00' ~ as.numeric(NA),
        TRUE ~ bp
      )
    ) %>%
    ungroup() %>%
    mutate_at(vars(ventas, forecast_base, forecast_promocional, no_resurtible, bp, ventas_ly, presentation, display, fulfillment, ends_with('_piezas')),
              list(~format_number(., divide = precision_numeros, digits = log10(precision_numeros)))) %>% 
    select(departamento, categoria, formato, tribu, squad, ventas, forecast_base, forecast_promocional, no_resurtible, bp, ventas_ly, ym, fecha_calculo, ends_with('_piezas'), presentation, display, fulfillment)
}

## Guardar archivos listos
force <- FALSE
list.files('data/summary', full.names = TRUE, recursive = TRUE) %>% 
  str_subset('[0-9]{8}_building_blocks_[0-9]{6}.csv') %>% 
  walk(function(filename){
    outfile <- str_replace(filename, '\\.csv', '.rds')
    if (!file.exists(outfile) || force) {
      flog.info(toJSON(list(
        message = 'PARSING FILE',
        details = list(
          filename = filename
        )
      )))
      x <- parse_file(filename, req_cols, formatos)
      for(f in prepare_data_exceptions) {
        x <- f(x)
      }
      flog.info(toJSON(list(
        message = 'WRITING FILE',
        details = list(
          filename = outfile
        )
      )))
      saveRDS(x, file = outfile)
    } else {
      flog.info(toJSON(list(
        message = 'SKIPPING FILE',
        details = list(
          filename = outfile,
          reason = 'File already exists'
        )
      )))
    }
  })


## Checar que los cambios no afecten datasets anteriores (salvo agregar la columna de ventas)
# x <- parse_file('data/20190101_building_blocks_201902.csv', req_cols = req_cols, formatos = formatos)
# y <- read_rds('data/20190101_building_blocks_201902.rds')
# daff::diff_data(y, x) %>% daff::render_diff()