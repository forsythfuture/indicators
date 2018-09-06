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

# filter for forsyth
forsyth_codes <- filter(puma_names, cntyname == 'Forsyth NC')

state = 37

year <- '14'
data_directory <- 'pums'


forsyth <- palma_single(state = 37, area_code = forsyth_codes$PUMA, year, data_directory)



