
library(tidyverse)
library(readxl)
options(readr.default_locale=readr::locale(tz = ''))

d <- read_excel('data/20181126 Building blocks Dic.xlsx', 'Sheet1')


d %>% 
  group_by(
    Vicepresidencia = VP,
    Departamento = DEPT_NBR,
    Categoria = CAT_NBR,
    Formato = FORMATO
  ) %>% 
  summarise(
    BP = sum(BP, na.rm = TRUE),
    Forecast_Base = sum(`FCST_BASE$`, na.rm = TRUE),
    Forecast_Promocional = sum(`FCST_PROM$`, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    fecha_calculo = '2018-11-26',
    ym = 201812
  ) %>% 
  write_csv('data/20181126_building_blocks_201812.csv')
