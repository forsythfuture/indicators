##########################################################################
#
#  This program uses the TAXSIM functions to create datasets of tax liabilities.
#  These datasets are exported to csv files that can be run in the TAXSIM cli program
#
###########################################################################

# import functions
source('tax_puma_cal/taxsim_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

# iterate through each year and create dataset for TAXSIM; then save dataset as csv file

for (yr in seq(2006, 2016)) {
  
  # create file name of output based on year
  file_name <- paste0('taxes_', as.character(yr), '.csv')
  
  pop_taxes(con, yr) %>%
    write_csv(., file_name)
  
}
