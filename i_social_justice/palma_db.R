library(tidyverse)
library(DBI)
library(data.table)

source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

### palmas for all counties in NC in all years ###

# initiate list to store all Palmas
county_palma <- data.frame()

# iterate through each year, calculating Palma
for (yr in seq(2016, 2017)) {

  county_palma_yr <- palmas_complete(con, 
                                     year = yr, 
                                     level = 'county', 
                                     state = 37, 
                                     area_code = NA)
  
  county_palma <- bind_rows(county_palma, county_palma_yr)

}



codes <- puma_area_code(letters, 'puma_counties.csv')
