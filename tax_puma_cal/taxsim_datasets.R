##########################################################################
#
#  This program uses the TAXSIM functions to create datasets of tax liabilities.
#  These datasets are exported to csv files that can be run in the TAXSIM online
#
###########################################################################

library(tidyverse)
library(tidyverse)
library(DBI)
library(data.table)

source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')
source('tax_puma_cal/taxsim_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")
year <- 2017

# iterate through each year and create dataset for TAXSIM; then save dataset as csv file

for (yr in seq(2006, 2017)) {
  
  print(yr)
  
  # create file name of output based on year
  file_name <- paste0('taxes_', as.character(yr), '.csv')
  
  yr <- 2017
  
  a <- pop_taxes(con, yr) #%>%
  
  
  c <- a %>% group_by(SERIALNO) %>% summarize(income = sum(e00200p))
  
  write_csv(a, 'test_2017.csv', col_names = FALSE)
  from_taxsim/
    #write_csv(., file_name, col_names = FALSE)
  b <- house_incomes(con, 2017, state = 37, area_code = 1801)
}

b <- house_incomes(con, 2017, state = 37, area_code = 1801)

tax <- read_delim('tax_puma_cal/nc_from_taxsim/calculated_2017.txt', 
                   delim = ' ') %>%
  mutate(taxes = fiitax + siitax + fica) %>%
  group_by(taxsim_id) %>%
  summarize(total_taxes = sum(taxes))

c <- full_join(b, tax, by - )
