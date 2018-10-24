############################################################
#
# This file imports PUMs data and creates datasets
# that can be used in the TAXSIM online model at 
# https://users.nber.org/~taxsim/taxsim27/
#
###############################################################

library(tidyverse)
library(DBI)

# import custom functions
source('tax_puma_cal/taxsim_functions.R')


con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")
year <- 2017

# iterate through each year and create dataset for TAXSIM; then save dataset as csv file

for (yr in seq(2017, 2017)) {
  
  print(yr)
  
  # create file name of output based on year
  file_name <- paste0('tax_puma_cal/to_taxsim_', as.character(yr), '.csv')
  
  # calcualte taxable income and write out dataset
  df <- pop_taxes(con, yr)
  
}