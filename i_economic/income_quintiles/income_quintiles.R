library(tidyverse)
library(data.table)
library(DBI)

source('functions/puma_functions.R')
source('i_economic/income_quintiles/income_quintiles_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

############# Create dataset of all people, all years ###################

# initialize dataframe to store all people, all years
incomes <- data.frame()

# housing PUMs variables
house_vars <- c('SERIALNO', # serial number of housing unit; used to match housing units and populations 
                'ST', # state
                'PUMA', # four digit PUMA code
                'TYPE', # Type of husing unit; 1 is housing unit, which are the only units we need
                'HINCP') # housing costs as a percentage of income

years <- seq(2006, 2017)

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
    # only keep NC, which is state number 37
    filter(ST == 37) %>%
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
           # only keep housing units; remove institutional units
           TYPE ==1,
           # do not keep rows with NA values for income
           !is.na(hincp)) %>%
    # remove unneeded columns
    select(-TYPE) %>%
    collect() %>%
    # add county names
    groupings(., 'county', yr)
  
  # merge housing and population
  incomes <- left_join(house, pop, by = 'SERIALNO') %>%
  # add year
    mutate(year = !!yr,
      # if the year is 2017, remove the 2017 from the start of the SERIALNO
      SERIALNO = if (!!yr == 2017) as.integer(str_replace_all(.$SERIALNO, '^2017', '')) else .$SERIALNO) %>%
    # 2017 replicate weight column are upper case, while other years are lower case
    # rename all columns to lower case, so they match
    rename_all(funs(tolower(.))) %>%
    bind_rows(., incomes)
  
}
  
################ Calculate income quintile information ######################

# calculate income quintile percentages for each year and all replciate weights
demo_perc <- lapply(tolower(replicate_weights), 
                    function(x) find_quint_perc(incomes, x))

# total_se <- demo_perc[[1]]
# for (i in seq_along(demo_perc)) {
#   
#   print(i)
#   
#   total_se <- full_join(total_se, demo_perc[[i]],
#                         by = c('year', 'group', 'subtype', 'quintile', 'type'))
#   #print(nrow(demo_perc[[i]]))
#   
#   #n <- if ( nrow(demo_perc[[i]]) == 2149 ) n + 0 else n + 1
#   
# }
# 
# total_se_na <- t(total_se[!complete.cases(total_se),]) %>% as.data.frame()

# calculate standard errors
# first replicate weight is missing a row, so remove
demo_perc_se <- find_se(demo_perc)

# pull out quintile percentages of primary weights and
# add standard errors to dataframe of quintile percentages of primary weight
perc_primary <- demo_perc[[1]] %>%
  # rename group column as geo_description
  rename(geo_description = group) %>%
  # only keep the needed geographic units
  filter(geo_description %in% c('Forsyth', 'Guilford', 'Durham', 'North Carolina')) %>%
  # add standard error column
  mutate(se = !!demo_perc_se[[1]],
         # add ' County, NC' to county names
         geo_description = ifelse(geo_description %in% c('Forsyth', 'Guilford', 'Durham'),
                                  paste0(geo_description, ' County, NC'), geo_description))

# write out as csv
write_csv(perc_primary, 'i_economic/income_quintiles/data/quintiles_perc_data.csv')
