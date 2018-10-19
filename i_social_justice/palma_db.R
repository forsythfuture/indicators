library(tidyverse)
library(DBI)
library(data.table)


source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')

by_county = TRUE

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

household_incomes <- house_incomes(con, 2016, state = 37, area_code = NA)

groupings <- function(household_incomes, level, year) {
  
  # this function determines what groupings to use in calculating Palma
  # Input:
  #     household_incomes: dataframe of household incomes created by house_incomes
  #     levels: 'US', 'state', 'county', 'puma'
  
  # convert level to lowercase
  level <- str_to_lower(level)
  
  # replace PUMA codes with county names if true
  if (level %in% c('county', 'counties')) {
    
    # dataframe of all NC puma codes and county names
    # codes are different starting in 2012, so ensure we are pulling in the right year
    if (year > 2011) {
      
      nc_codes <- puma_area_code(letters, 'puma_counties.csv') %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    } else {
      
      nc_codes <- puma_area_code(letters, 'puma_counties.csv', puma12 = FALSE) %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    }
    
    # add county name to income dataframe
    incomes <- household_incomes %>%
      left_join(nc_codes, by = 'PUMA') %>%
      rename(group = cntyname)

  } else if (level %in% c('united states', 'us')) {
    
    incomes$group <- 'US'
    
  } else if (level %in% c('state', 'states')) {
    
    incomes$group <- incomes$ST
    
  } else if (level %in% c('puma', 'pums')) {
    
    income$group <- income$PUMA
    
  }
  
  return(income)
  
}

codes <- puma_area_code(letters, 'puma_counties.csv')
