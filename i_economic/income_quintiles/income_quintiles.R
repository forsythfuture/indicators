library(tidyverse)
library(data.table)
library(DBI)

source('functions/puma_functions.R')
source('i_economic/income_quintiles/income_quintiles_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

# housing PUMs variables
house_vars <- c('SERIALNO', # serial number of housing unit; used to match housing units and populations 
                'ST', # state
                'PUMA', # four digit PUMA code
                'TYPE', # Type of husing unit; 1 is housing unit, which are the only units we need
                'HINCP') # housing costs as a percentage of income

for (yr in years) {
  yr <- 2017
  
  # population variables that are needed
  # the name of one population variable depends on year, so these variables
  # must be defined inside the year loop
  pop_vars <- c('SERIALNO', # serial number, matches with housing unit serial number
                # relationship to reference person; variable name changed in 2010
                ifelse(yr < 2010, 'REL', 'RELP'),
                'ST', # state
                'PUMA', # PUMA code
                'AGEP', # age
                'RAC1P', # race
                'HISP') # hispanic origin
  
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
           # only keep Forsyth County PUMA data
           PUMA %in% c(1801, 1802, 1803)) %>%
    collect() 
  
  # change REL column name to RELP if REL is a column (less than 2010)
  # this allows us to use the same column names for all years
  pop <- if ('REL' %in% colnames(pop)) rename(pop, RELP = REL) else pop
  
  # recode race and create age bins
  pop <- pop %>%
    filter(RELP == 0) %>%
    # remove unneeded columns
    select(-RELP, -PUMA, -ST) %>%
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
  
  # merge housing and population
  incomes <- left_join(house, pop, by = 'SERIALNO')
  
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