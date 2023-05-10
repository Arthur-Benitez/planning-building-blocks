# Setup
library(tidyverse)

# Files reading
## Business units
business_units <- read_tsv('data/business-units.txt') %>%
  set_names(tolower(names(.)))

## New tribu/squad
new_tribu <- read_csv(file.choose())

## Data update function
update_business_units <- function(catalog, new_data, replace = FALSE) {
  ## Si replace es FALSE, se regresa el join del catálogo con la tribu nueva para comparar la información antes de reemplazarla
  if (!replace) {
    catalog %>%
      right_join(new_data, by = c('departamento', 'categoria')) %>%
      select(departamento, categoria, cat_name_old = categoria_name.y, cat_name_new = categoria_name.x, tribu_old = tribu.y, tribu_new = tribu.x, squad_old = squad.y, squad_new = squad.x)
  } else {
    catalog %>%
      left_join(new_data, by = c('departamento', 'categoria')) %>%
      ## Le da preferencia a la tribu y squad de new_data
      mutate(
        tribu = coalesce(tribu.y, tribu.x),
        squad = coalesce(squad.y, squad.x)
      ) %>%
      select(-matches('.x|.y'))
  }
}

## Function call
result <- update_business_units(bu, new_tribu, replace = TRUE)
write_csv(result, paste0('data/business-units_', format(Sys.time(), '%Y-%m-%d_%H-%M-%S'), '.csv'))
write_csv(result, 'data/business-units.txt')
