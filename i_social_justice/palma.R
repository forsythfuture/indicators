library(tidyverse)
library(data.table)


source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')
source('functions/comparison_counties.R')


# create dataframe of NC's ten largest counties
counties <- load_comparisons()

# get area codes of ten largest counties minus Forsyth
area_codes <- puma_area_code(counties$county[-1], 'puma_counties.csv', puma12 = TRUE)

state = 37
area_code = area_codes$PUMA
years = c('12', '13', '14', '15', '16')
data_directory = 'pums'


urban_palma12 <- palma_years(state = state, area_code = area_code, 
                               years = years, data_directory)

# bind data sets of palmas
palma <- read_csv('Forsyth_palma_16.csv') %>%
  bind_rows(read_csv('urban_palma_16.csv')) %>%
  bind_rows(read_csv('US_palma_16.csv'))

#write_csv(palma, 'i_social_justice/data/palma.csv')

