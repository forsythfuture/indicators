library(tidyverse)
library(data.table)
library(DBI)

source('functions/puma_functions.R')
source('i_demographics/puma_family_structure_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

############# Create dataset of all people, all years ###################

# housing PUMs variables
house_vars <- c('SERIALNO', # serial number of housing unit; used to match housing units and populations 
                'ST', # state
                'PUMA', # four digit PUMA code
                'FES', # family structure
                'FPARC') # number of related children

# initialize data frame to store all year information
demographics <- data.frame()

years <- 2017

for (yr in years) {
  
  print(yr)
  
  # population variables that are needed
  # the name of one population variable depends on year, so these variables
  # must be defined inside the year loop
  pop_vars <- c('SERIALNO', # serial number, matches with housing unit serial number
                # relationship to reference person; variable name changed in 2010
                ifelse(yr < 2010, 'REL', 'RELP'),
                'ST', # state
                'PUMA', # PUMA code
                'SEX', # age
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
    # only keep NC, which is state number 37
    filter(ST == 37,
           PUMA == 1801) %>%
    collect() 
  
  # change REL column name to RELP if REL is a column (less than 2010)
  # this allows us to use the same column names for all years
  pop <- if ('REL' %in% colnames(pop)) rename(pop, RELP = REL) else pop
  
  # only need primary householder, because we only need race from this person
  pop <- pop %>%
     filter(RELP == 0) %>%
  # remove unneeded columns
     select(-RELP, -ST) 
  
  # import housing variables
  house <- tbl(con, table_name('housing', yr)) %>%
    # only keep needed variables
    select(!!house_yr_vars) %>%
    filter(ST == 37,
           PUMA == 1801,
           # only keep households that are families
           # in each of the next two columns, NA represents no family
           !is.na(FES),
           !is.na(FPARC)) %>%
    collect()
  
  # merge housing and population
  demographics <- inner_join(house, pop, by = c('SERIALNO', 'PUMA')) %>%
    # add year
    mutate(year = !!yr,
           # if the year is 2017, remove the 2017 from the start of the SERIALNO
           SERIALNO = if (!!yr == 2017) as.integer(str_replace_all(.$SERIALNO, '^2017', '')) else .$SERIALNO) %>%
    # 2017 replicate weight column are upper case, while other years are lower case
    # rename all columns to upper case, so they match
    rename_all(funs(toupper(.))) %>%
    bind_rows(., demographics)

}

# recode races
demographics <- clean_race(demographics)

# recode family structure (FES)
fes_recode <- c(1=1, 2=1, 3=1, 4=1, # married families
                2=5, 2-6, # males
                3=7, 3=8) # females

# recode presence of children (FPARC)
fparc_recode <- c(1=1, 2=1, 3=1, # children under 18
                  4=2) # no children

###################################
# create dataset that is the total count of each race, and total count overall
# this will be used to calculate percentage of families within each category

# create dataset for counts by demographic
total_counts <- demographics %>%
  group_by(RAC1P, PUMA, YEAR) %>%
  summarize(count = n()) %>%
  # remove other races
  filter(RAC1P != 4)

# create dataset for counts by total
# and add to dataset for counts by race
# totals will be labeled a race of 4
total_counts <- demographics %>%
  group_by(PUMA, YEAR) %>%
  summarize(count = n()) %>%
  # create column for race, needed so we can bind this 
  mutate(RAC1P = 4) %>%
  bind_rows(total_counts)

####################################
# create counts for each family structure and race

demographics %>%
  select(-starts_with('WGT')) %>%
  head(20)

