library(tidyverse)
library(data.table)


source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')
#source('functions/comparison_counties.R')


# create dataframe of NC's ten largest counties
#counties <- load_comparisons()

# get area codes of ten largest counties minus Forsyth
#area_codes <- puma_area_code(counties$county[-1], 'puma_counties.csv', puma12 = TRUE)

state = NA #37
area_code = NA # area_codes$PUMA
years = c('06', '07', '08', '09', '10', '11')
data_directory = 'pums'


urban_palma <- palma_years(state = state, area_code = area_code, 
                            years = years, data_directory)