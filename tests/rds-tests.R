library(testthat)

eval(parse('global.R', encoding = 'UTF-8'))

files <- list.files('data/summary', full.names = TRUE, recursive = TRUE) %>%
  str_subset('[0-9]{8}_building_blocks_[0-9]{6}.rds')

dfs <- files %>% 
  map(read_rds)


for (i in seq_along(files)) {
  test_that(sprintf('%s está bien', files[[i]]), {
    df <- dfs[[i]]
    expect_true(!any(df$vicepresidencia == '0')) # No existen registros con vicepresidencia == '0'
    expect_true(all(nchar(df$categoria) == 6)) # Todas las categorías tienen 6 caracteres ('01::25'))
    expect_true(all(df$formato %in% c('Mi Bodega Aurrera', 'Bodega Aurrera Express', 'Bodega Aurrera', 'Supercenter', 'Superama'))) # Todos los registros tienen un formato válido
    expect_true(!all(df$tribu == '0')) # No todas las tribus son '0'
    expect_true(!any(df$vicepresidencia == '0')) # No existen registros con vicepresidencia == '0'
  })
}
