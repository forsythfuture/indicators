library(tidyverse)
library(DBI)
library(data.table)

source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

### palmas for all counties in NC in all years ###

# initiate list to store all Palmas
state_palma <- data.frame()

# iterate through each year, calculating Palma
for (yr in seq(2006, 2017)) {

  state_palma_yr <- palmas_complete(con = con, 
                                     year = yr, 
                                     level = 'county', 
                                     state = 37, 
                                     area_code = NA)
  
  state_palma <- bind_rows(state_palma, state_palma_yr)
  
  # file name to write out
  write_file <- paste0('i_social_justice/puma_palmas_state', as.character(yr), '.csv')
  write_csv(state_palma, write_file)

}

### palmas for all PUMAS in NC in all years ###

# initiate list to store all Palmas
puma_palma <- data.frame()

# iterate through each year, calculating Palma
for (yr in seq(2017, 2017)) {
  
  puma_palma_yr <- palmas_complete(con = con, 
                                     year = yr, 
                                     level = 'puma', 
                                     state = 37, 
                                     area_code = NA)
  
  puma_palma <- bind_rows(puma_palma, puma_palma_yr)
  
  # file name to write out
  write_file <- paste0('i_social_justice/puma_palmas_code', as.character(yr), '.csv')
  write_csv(puma_palma, write_file)
  
}



county <- bind_rows(
  read_csv('i_social_justice/puma_palmas2006.csv'),
  read_csv('i_social_justice/puma_palmas2016.csv'),
  read_csv('i_social_justice/puma_palmas2017.csv'))
area_code <- bind_rows(
  read_csv('i_social_justice/puma_palmas_code2016.csv'),
  read_csv('i_social_justice/puma_palmas_code2017.csv')) %>%
  mutate(geography = as.character(geography))

a <- bind_rows(county, area_code) 


a %>%
  write_csv(., 'i_social_justice/data/palma_county_puma.csv')