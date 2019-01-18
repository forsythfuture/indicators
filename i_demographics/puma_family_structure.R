library(tidyverse)
library(data.table)
library(DBI)

source('functions/puma_functions.R')
source('i_demographics/puma_family_structure_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

############# Create dataset of all people, all years ###################

# counties to keep
counties <- c('Forsyth', 'Durham', 'Guilford', 'Mecklenberg')

# housing PUMs variables
house_vars <- c('SERIALNO', # serial number of housing unit; used to match housing units and populations 
                'ST', # state
                'PUMA', # four digit PUMA code
                'FES', # family structure
                'FPARC') # number of related children

# initialize data frame to store all year information
demographics <- data.frame()

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
           # only keep households that are families
           # in each of the next two columns, NA represents no family
           !is.na(FES),
           !is.na(FPARC)) %>%
    collect()
  
  # merge housing and population
  demographics <- inner_join(house, pop, by = c('SERIALNO', 'PUMA')) %>%
    # 2017 replicate weight column are upper case, while other years are lower case
    # rename all columns to upper case, so they match
    rename_all(funs(toupper(.))) %>%
    # add year
    mutate(YEAR = !!yr,
           # if the year is 2017, remove the 2017 from the start of the SERIALNO
           SERIALNO = if (!!yr == 2017) as.integer(str_replace_all(.$SERIALNO, '^2017', '')) else .$SERIALNO) %>%
    # add county names
    groupings(., 'county', yr) %>%
    # do not need PUMA since we have counties
    select(-PUMA, -ST) %>%
    # only keep needed counties
    filter(group %in% counties) %>%
    bind_rows(., demographics)

}

rm(house)
rm(pop)
gc()
################################
# recode values

# recode races
demographics <- clean_race(demographics)

# recode family structure (FES)
# 1: married families
# 2: males
# 3: females
fes_recode <- c(`1`=1, `2`=1, `3`=1, `4`=1, `5`=2, `6`=2, `7`=3, `8`=3)

# recode presence of children (FPARC)
# 1: children under 18
# 2: no children under 18
fparc_recode <- c(`1`=1, `2`=1, `3`=1, `4`=2)

demographics <- demographics %>%
  mutate(FES = recode(FES, !!!fes_recode),
         FPARC = recode(FPARC, !!!fparc_recode))

###################################

# create list of counts for each weight
counts <- counts_list(demographics)

# find standard error of counts and percentages
se_counts <- find_se(counts, 5)
se_perc <- find_se(counts, 8)

# add standard errors, moe, cv
counts <- counts[[1]] %>%
  mutate(se_family_count = se_counts[[1]],
         se_percentage = se_perc[[1]],
         moe_family_count = 1.96 * se_family_count,
         moe_percentage = 1.96 * se_percentage,
         cv_family_count = round(100 * (se_family_count / family_count), 2),
         cv_percentage = round(100 * (se_percentage / percentage), 2))

colnames(counts) <- c('county', 'year', 'family_structure', 'children', 'family_count', 'race', 
                      'total_count', 'percentage', 'se_family_count', 'se_percentage',
                      "moe_family_count", "moe_percentage", "cv_family_count", "cv_percentage")

# reorder columns
counts <- counts %>%
  select(county:children, race, everything())

# write_csv(counts, 'i_demographics/data/pums_NC_counties.csv')
