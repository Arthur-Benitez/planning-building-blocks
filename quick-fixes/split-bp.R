
library(tidyverse)
library(readxl)
library(futile.logger)

input_file <- 'data/BP/BP.xlsx'
output_dir <- 'data/BP/2019/'
overwrite <- FALSE

## Create output dir if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
} else if (length(list.files(output_dir)) > 0 && !overwrite) {
  stop(sprintf('Folder %s already exists and is not empty. Delete its contents or set overwrite to TRUE.'))
}

## Read current BP
bp <- read_excel(input_file)

## Escribir 
bp %>% 
  arrange(mes, dept_nbr, cat_nbr, formato) %>% 
  group_by(dept_nbr) %>% 
  group_split(keep = TRUE) %>% 
  walk(function(x){
    dept <- unique(x$dept_nbr)
    outfile <- file.path(output_dir, sprintf('BP-D%02d.csv', dept))
    write_excel_csv(
      x = x,
      path = outfile,
      na = ''
    )
    flog.info('Done writing file %s.', outfile)
  })



new <- read_bp(2019)

old <- bp %>% 
  mutate(
    mes = as.numeric(mes)
  )
all.equal(old, new)

daff::render_diff(daff::diff_data(old, new))

