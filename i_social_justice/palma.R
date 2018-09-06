library(tidyverse)
library(data.table)
library(pryr)


source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')
source('functions/comparison_counties.R')

# create dataframe of NC's ten largest counties
counties <- load_comparisons()

# find PUMA code of counties
puma_names <- puma_area_code(counties$county, 'puma_counties.csv')

state = 37

year <- '14'
data_directory <- 'pums'

filter_g <- 'ST %in% state & PUMA %in% puma_names$PUMA'

a <- palma_single(filter_g, year, data_directory)
