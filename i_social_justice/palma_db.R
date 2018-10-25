library(tidyverse)
library(DBI)
library(data.table)

source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

housing <- tbl(con, 'h_17') %>%
  select(SERIALNO, HINCP, FINCP) %>%
  filter(SERIALNO == 2017000183945) %>%
  collect()


population <- tbl(con, 'p_17') %>%
  select(SERIALNO, INTP, OIP, PAP, RETP, SEMP, SSIP, SSP, WAGP, PINCP) %>%
  filter(SERIALNO == 2017000183945) %>%
  collect()

df <- house_incomes(con, year = 2017, state = 37, area_code = c(1801))

df_zero <- filter(df, tax_liability == 0) %>%
  arrange(desc(HINCP))

head(df_zero$SERIALNO)

# initiate list to store all Palmas
state_palma <- data.frame()

# iterate through each year, calculating Palma
for (yr in seq(2006, 2017)) {
  
  print(yr)

  state_palma_yr <- palmas_complete(con = con, 
                                     year = yr, 
                                     level = 'state', 
                                     state = 37, 
                                     area_code = NA)
  
  state_palma <- bind_rows(state_palma, state_palma_yr)
  
  # file name to write out
  write_file <- paste0('i_social_justice/puma_palmas_state', as.character(yr), '.csv')
  write_csv(state_palma, write_file)

}

num_in_taxes <- filter(taxes, SERIALNO %in% df_zero$SERIALNO)
tax_to_taxsim <- read_csv('tax_puma_cal/nc_to_taxsim_online/taxes_to_taxsim_2017.csv', col_names = FALSE) %>%
  filter(X1 %in% df_zero$SERIALNO)
write_csv(tax_to_taxsim, 'to_taxsim_test.csv', col_names = FALSE)



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