library(tidyverse)
library(data.table)
library(DBI)

source('functions/puma_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

# housing PUMs variables
house_vars <- c('SERIALNO', # serial number of housing unit; used to match housing units and populations 
                'ST', # state
                'PUMA', # four digit PUMA code
                'TYPE', # Type of husing unit; 1 is housing unit, which are the only units we need
                'HINCP') # housing costs as a percentage of income

# population variables that are needed
pop_vars <- c('SERIALNO', # serial number, matches with housing unit serial number
              'RELP', # relationship of person in household; 0 is the reference person
              'ST', # state
              'PUMA', # PUMA code
              'AGEP', # age
              'RAC1P', # race
              'HISP') # hispanic origin


for (yr in years) {
  yr <- 2017
  # must define weight variable names within year loop because names change dpending on year
  # replciate weight variable names are lower case until 2017 and upper case starting in 2017
  weight_names <- ifelse(yr >= 2017, 'WGTP', 'wgtp')
  replicate_weights <- c('WGTP', paste0(weight_names, seq(1, 80)))
  
  # add the year's replicate weights to housing variables
  house_yr_vars <- c(house_vars, replicate_weights)

  # import and clean population variables
  pop <- tbl(con, table_name('population', yr)) %>%
    # only keep needed variables
    select(!!pop_vars) %>%
    filter(ST == 37, # only keep NC, which is state number 37
           # we only want the deomographic data from the person filling out the survey
           # this person is indicated by RELP == 0
           RELP == 0,
           # only keep Forsyth County PUMA data
           PUMA %in% c(1801, 1802, 1803)) %>%
    # remove unneeded columns
    select(-RELP, -PUMA, -ST) %>%
    collect() %>%
    # recode race and create age bins
    clean_demographics(., c(0, 24, 44, 64, 150))
  
  # import housing variables
  house <- tbl(con, table_name('housing', yr)) %>%
    # only keep needed variables
    select(!!house_yr_vars) %>%
    filter(ST == 37, # only keep NC, which is state number 37
           # only keep Forsyth County PUMA data
           PUMA %in% c(1801, 1802, 1803),
           # only keep housing units; remove institutional units
           TYPE ==1) %>%
    # remove unneeded columns
    select(-TYPE) %>%
    collect() %>%
    # add county names
    groupings(., 'county', yr)
  
  # iterate through each weight and replciate weight
  for (wgt in replicate_weights) {
    wgt <- weight_names
    
    incomes <- house %>%
      # only select needed housing columns and one weight
      select_at(c('SERIALNO', 'PUMA', 'HINCP', wgt)) %>%
      # merge population data
      left_join(pop, by = 'SERIALNO')
    
  }
  
  
  # calculate income quintiles and add income quintile category to each house
  income_quintiles <- quantile(house$HINCP, probs = seq(0, 1, by = .2), na.rm=TRUE)
  
}