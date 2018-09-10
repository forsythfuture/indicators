library(tidyverse)
library(data.table)


source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')
source('functions/comparison_counties.R')


# create dataframe of NC's ten largest counties
counties <- load_comparisons()

# get area codes of ten largest counties minus Forsyth
area_codes <- puma_area_code('Forsyth', 'puma_counties.csv', puma12 = TRUE)

state = 37
area_code = area_codes$PUMA
years = c('12', '13', '14', '15', '16')
data_directory = 'pums'


urban_palma <- palma_years(state = state, area_code = area_code, 
                            years = years, data_directory)

# create dataset of all Palmas
palmas <- read_csv('i_social_justice/data/us_palma.csv') %>%
  bind_rows(read_csv('i_social_justice/data/urban_palma.csv')) %>%
  bind_rows(read_csv('i_social_justice/data/forsyth_palma.csv'))

write_csv(palmas, 'i_social_justice/data/palmas.csv')
