library(tidyverse)
library(data.table)
library(DBI)

source('i_economic/median_wages/median_wages_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

pop_vars <- c('SERIALNO', 'SPORDER', 'PUMA', 'ST', 'AGEP', 'SEX', 'RAC1P', 'HISP', 'PERNP', 'WKW', 'WKHP', 'SCH')

yr <- 2017
state <- 37

# for each year

# create table name
tbl_name <- as.character(yr) %>%
  str_extract(., '[0-9][0-9]$') %>%
  paste0('p_', .)

tbl <- tbl(con, tbl_name) %>%
  select(!!pop_vars) %>%
  filter(ST == !!state,
         PUMA == 1801,
         # filter out people 65 and over
         AGEP < 65,
         # filter out those currently in school
         # 1 is those not in school
         SCH == 1) %>%
  collect() %>%
  # recode race and create age bins
  clean_demographics(.)

# recode weeks worked from integer category to actual number
hours_recode <- list(`1` = 51,
                     `2` = 48.5,
                     `3` = 43.5,
                     `4` = 33,
                     `5` = 20,
                     `6` = 7)

# create lookup table of counties and PUMAs, so that county names can be added
counties <- create_counties(yr)

tbl <- tbl %>%
  # recode weeks wored during year
  # 2007 already shows number so don't recode
  mutate(weeks_worked = if (yr != 2007) recode(.$WKW, !!!hours_recode) else .$WKW,
         # calcualte hourly rate by dividing wages by total hours worked
         # total hours worked is weeks worked times hours per week
         wage = PERNP / (weeks_worked * WKHP)) %>%
  # eliminate values 0 or less as this likely represent self-employed people with odd circumstances
  # this also eliminates null values
  filter(wage > 0) %>%
  # drop unneeded variables
  select(-PERNP:-weeks_worked) %>%
  # join in county names based on PUMA
  left_join(counties, by = 'PUMA')

### import weights table

# must define weight variable names within year loop because names change dpending on year
# replciate weight variable names are lower case until 2017 and upper case starting in 2017
weight_names <- ifelse(yr >= 2017, 'PWGTP', 'pwgtp')

replicate_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))

# replicate weight variables
pop_weights <- c('SERIALNO', 'SPORDER', 'PWGTP', replicate_weights)

# import all table weights
wgt_tbl <- tbl(con, tbl_name) %>%
  filter(ST == !!state,
         PUMA == 1801,
         # filter out people 65 and over
         AGEP < 65,
         # filter out those currently in school
         # 1 is those not in school
         SCH == 1) %>%
  select(!!pop_weights) %>%
  collect() %>%
  # divide all weights by 2 to reduce compute time
  # since we are just looking at medians, this should have little impact on the number
  # use celing so numbers are at least one
  mutate_at(vars(PWGTP, !!replicate_weights),
            funs(ceiling(./2)))

##### calculate median wages

# initialize list to store median values for each demographic
median_demo <- list()

## loop though each demographic

demo <- 'RAC1P'

# create columns to group on,
# if it is a demographic column group on that and county,
# if it is the total, just group on county
group_cols <- if (demo == 'total') 'cntyname' else c(demo, 'cntyname')

# attache weight column to dataset containing wages
median_wage <- tbl %>%
  # join wage data
  left_join(wgt_tbl, by = c('SERIALNO', 'SPORDER')) %>%
  # group by required columns, based on demographic
  group_by_at(group_cols)

# find median wage by demographic for all replciate weights
median_demo <- lapply(replicate_weights, 
                      function(x) find_median(median_wage, demo, x))

# calcualte standard error
median_se <- find_se(median_demo)

# add standard errors to dataset containing median wage values
# first dataframe in list contains median vage values from primary weight
median_demo <- median_demo[[1]] %>%
  ungroup() %>%
  mutate(se = median_se[[1]],
         # adde column for demographic
         type = demo,
         year = yr)

colnames(median_demo) <- c('subtype', 'cntyname', 'estimate', 'se', 'type', 'year')
  






         