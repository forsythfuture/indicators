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

### Create dataset to be used in analysis; place data in format to be used with shiny app

## change puma code with puma name

# counties to keep
geo_areas <- c('Forsyth', 'Guilford', 'Durham')
# 
# # remove PUMA data prior to 2012
# # puma codes changed, so comparisons are difficult
# palma <- read_csv('i_social_justice/data/palma_county_puma.csv') %>%
#   filter(!(level == 'puma' & year < 2012)) %>%
#   # only keep comparison counties
#   filter(level == 'puma' | geography %in% geo_areas)
# 
# # import puma code names
# codes <- read_csv('puma_counties.csv') %>%
#   # only keep puma12 name and number
#   select(puma12, PUMA12name) %>%
#   # convert codes to character, so it can be merged with plama dataset
#   mutate(puma12 = as.character(puma12)) %>%
#   # delete duplicates
#   unique()
# 
# # add PUMA names to rows that are for PUMA (not counties)
# palma_shiny <- left_join(palma, codes, by = c('geography' = 'puma12')) %>%
#   # if row is for a PUMA (not county) then use puma name for gepgraphy
#   mutate(geography = ifelse(.$level == 'puma',
#                             .$PUMA12name, .$geography)) %>%
#   select(-PUMA12name)
# 
# write_csv(palma, 'i_social_justice/data/palma_shiny.csv')

#### Put dataset in format for use with Shiny app
palma <- read_csv('i_social_justice/data/palma_final.csv') %>%
  # calculate MOE and CV
  mutate(moe = se * 1.645,
         cv = (se / palma) * 100) %>%
  rename(geo_description = geography, type = level, estimate = palma) 

# add county name to PUMA rows
codes <- read_csv('puma_counties.csv') %>%
  # only keep puma12 name and number
  select(puma12, cntyname, PUMA12name) %>%
  # convert codes to character, so it can be merged with plama dataset
  mutate(puma12 = as.character(puma12)) %>%
  # only keep comparison counties
  select(-puma12) %>%
  # delete duplicates
  unique()

# add county names to palma dataset
palma <- left_join(palma, codes, by = c('geo_description' = 'PUMA12name')) %>%
  mutate(cntyname = ifelse(.$type %in% c('county', 'state'), .$geo_description, .$cntyname)) %>%
  # replace ' NC' in county name with nothing
  mutate(cntyname = str_replace_all(cntyname, ' NC', '')) %>%
  # only keep comparison counties
  filter(cntyname %in% c('North Carolina', 'Forsyth', 'Guilford', 'Durham'))

write_csv(palma, 'i_social_justice/data/palma_shiny.csv')

### palmas for all PUMAS in NC in all years ###

# # initiate list to store all Palmas
# puma_palma <- data.frame()
# 
# # iterate through each year, calculating Palma
# for (yr in seq(2017, 2017)) {
#   
#   puma_palma_yr <- palmas_complete(con = con, 
#                                      year = yr, 
#                                      level = 'puma', 
#                                      state = 37, 
#                                      area_code = NA)
#   
#   puma_palma <- bind_rows(puma_palma, puma_palma_yr)
#   
#   # file name to write out
#   write_file <- paste0('i_social_justice/puma_palmas_code', as.character(yr), '.csv')
#   write_csv(puma_palma, write_file)
#   
# }
# 
# 
# 
# county <- bind_rows(
#   read_csv('i_social_justice/puma_palmas2006.csv'),
#   read_csv('i_social_justice/puma_palmas2016.csv'),
#   read_csv('i_social_justice/puma_palmas2017.csv'))
# area_code <- bind_rows(
#   read_csv('i_social_justice/puma_palmas_code2016.csv'),
#   read_csv('i_social_justice/puma_palmas_code2017.csv')) %>%
#   mutate(geography = as.character(geography))
# 
# a <- bind_rows(county, area_code) 
# 
# 
# a %>%
#   write_csv(., 'i_social_justice/data/palma_county_puma.csv')