library(tidyverse)
library(data.table)


source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')
source('functions/comparison_counties.R')

# create dataframe of NC's ten largest counties
#counties <- load_comparisons()
state = NA #37
area_code = NA #c(1301, 1302, 1303)
years = c('12', '13', '14', '15', '16')
data_directory = 'pums'


palma_years(state = NA, area_code = NA, years, data_directory)



