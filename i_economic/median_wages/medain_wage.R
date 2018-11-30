library(tidyverse)
library(data.table)
library(DBI)

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

pop_vars <- c('PUMA', 'ST', 'AGEP', 'RAC1P', 'HISP', 'PERNP', 'WKW', 'WKHP', 'SCH')

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
  collect()

# recode weeks worked from integer category to actual number
hours_recode <- list(`1` = 51,
                     `2` = 48.5,
                     `3` = 43.5,
                     `4` = 33,
                     `5` = 20,
                     `6` = 7)

tbl <- tbl %>%
  # recode weeks wored during year
  # 2007 already shows number so don't recode
  mutate(weeks_worked = if (yr != 2007) recode(.$WKW, !!!hours_recode) else .$WKW,
         # calcualte hourly rate by dividing wages by total hours worked
         # total hours worked is weeks worked times hours per week
         wage = PERNP / (weeks_worked * WKHP)) %>%
  # eliminate values 0 or less as this likely represent self-employed people with odd circumstances
  # this also eliminates null values
  filter(wage > 0)

### import weights table

# must define weight variable names within year loop because names change dpending on year
# replciate weight variable names are lower case until 2017 and upper case starting in 2017
weight_names <- ifelse(yr >= 2017, 'PWGTP', 'pwgtp')

# replicate weight variables
pop_weights <- c('SERIALNO', 'PWGTP', paste0(weight_names, seq(1, 80)))

# import all table weights
wgt_tbl <- tbl(con, tbl_name) %>%
  select(!!pop_weights) %>%
  filter(ST == !!state,
         PUMA == 1801,
         # filter out people 65 and over
         AGEP < 65,
         # filter out those currently in school
         # 1 is those not in school
         SCH == 1)




         