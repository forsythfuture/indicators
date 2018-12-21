##########################################################################
#
# This script imports the PUMs data that is needed for housing cost burden
#
###########################################################################

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
                'OCPIP', # housing costs as a percentage of income
                'GRPIP', # gross rent as a percentage of income
                'WGTP'
                )

# initialize empty dataframe to add all years to
housing_burden <- data.frame()

years <- seq(2006, 2017)

for (yr in years) {
  print(yr)
  
  # population variables that are needed
  # msut be defined in year loop because name depends on year
  pop_vars <- c('SERIALNO', # serial number, matches with housing unit serial number
                # relationship to reference person; variable name changed in 2010
                ifelse(yr < 2010, 'REL', 'RELP'),
                'ST', # state
                'PUMA', # PUMA code
                'AGEP', # age
                'RAC1P', # race
                'HISP' # hispanic origin
  )
  
  # import and clean population variables
  pop <- tbl(con, table_name('population', yr)) %>%
    # only keep needed variables
    select(!!pop_vars) %>%
    filter(ST == 37 # only keep NC, which is state number 37
           ) %>%
    collect()
  
  # change REL column name to RELP if REL is a column (less than 2010)
  # this allows us to use the same column names for all years
  pop <- if ('REL' %in% colnames(pop)) rename(pop, RELP = REL) else pop
  
  pop <- pop %>%
    filter(#group == 'Forsyth',
           # we only want the deomographic data from the person filling out the survey
           # this person is indicated by RELP == 0
           RELP == 0) %>%
    # recode race and create age bins
    clean_demographics(., c(0, 24, 44, 64, 150))
  
  # must define weight variable names within year loop because names change dpending on year
  # replciate weight variable names are lower case until 2017 and upper case starting in 2017
  weight_names <- ifelse(yr >= 2017, 'WGTP', 'wgtp')
  replicate_weights <- c('WGTP', paste0(weight_names, seq(1, 80)))
  
  # add the year's replicate weights to housing variables
  house_yr_vars <- c(house_vars, replicate_weights)
  
  # import housing variables
  house <- tbl(con, table_name('housing', yr)) %>%
    # only keep needed variables
    select(!!house_yr_vars) %>%
    filter(ST == 37, # only keep NC, which is state number 37
           # only keep housing units; remove institutional units
           TYPE ==1
    ) %>%
    collect() %>%
    groupings(., 'county', yr) %>%
    # some years have replicate weight column names that are capitalized,
    # while other years have lower case names
    # convert all column names to lower case to ensure rows properly bind
    rename_all(funs(stringr::str_to_lower(.))) #%>%
    #filter(group == 'Forsyth')
  
  # join population data to housing data by merging on serial number and PUMA
  housing_burden <- left_join(house, pop, by = c('SERIALNO', 'PUMA', 'ST')) %>%
    # remove rows that are missing both percentage going to rent and percentage going to housing
    filter(!is.na(OCPIP) | !is.na(GRPIP)) %>%
    # remove unneeded variables
    select(-SERIALNO, -ST, -PUMA, -TYPE, -RELP) %>%
    # convert to long form where each row is either rent or housing percentage share
    gather('housing_status', 'percentage_housing', OCPIP, GRPIP) %>%
    # change wording of housing_status so that it is more descriptive
    mutate(housing_status = recode(housing_status, OCPIP = 'owner',
                                                   GRPIP = 'renter')) %>%
    # remove missing values
    drop_na(percentage_housing) %>%
    # add year and county name
    mutate(year = !!yr) %>%
           #cntyname = 'Forsyth County, NC') %>%
    bind_rows(., housing_burden)
  
}

# write out results as an Rds object
saveRDS(housing_burden, 'i_economic/housing_burden/housing_burden.rds')
