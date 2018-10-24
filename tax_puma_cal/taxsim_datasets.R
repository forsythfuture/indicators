##########################################################################
#
#  This program uses the TAXSIM functions to create datasets of tax liabilities.
#  These datasets are exported to csv files that can be run in the TAXSIM online
#
###########################################################################

library(tidyverse)
library(DBI)
library(data.table)

source('functions/puma_functions.R')
source('tax_puma_cal/taxsim_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

# iterate through each year and create dataset for TAXSIM; then save dataset as csv file

for (yr in seq(2006, 2016)) {
  
  print(yr)
  
  # create file name of output based on year
  file_name <- paste0('tax_puma_cal/nc_to_taxsim_online/taxes_to_taxsim_', as.character(yr), '.csv')
  
 # calculate taxable income and write out results
  taxes_to_taxsim <- pop_taxes(con, yr) #%>%
    #write_csv(file_name, col_names = FALSE)

}

taxes_from_taxsim <- read_delim('tax_puma_cal/nc_from_taxsim_online/tax_from_taxsim_online_2017.txt', delim = ' ') %>%
  mutate(tax_liability = fiitax + siitax + fica) 

taxes_from_taxsim_complete <- taxes_from_taxsim%>%
  group_by(taxsim_id) %>%
  summarize(total_taxes = sum(tax_liability)) %>%
  mutate(taxsim_id = as.numeric(taxsim_id))

taxes_to_taxsim_complete <- taxes_to_taxsim %>%
  mutate(total_income = primary_income + spouse_income) %>%
  group_by(SERIALNO) %>%
  summarize(family_income = sum(total_income)) %>%
  mutate(SERIALNO = as.numeric(SERIALNO))

taxes_full <- left_join(taxes_from_taxsim_complete, taxes_to_taxsim_complete, by = c('taxsim_id' = 'SERIALNO'))
