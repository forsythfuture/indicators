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

#########################################################################################

### iterate through each year and create dataset for TAXSIM; then save dataset as csv file

for (yr in seq(2006, 2017)) {
  
  print(yr)
  
  # create file name of output based on year
  file_name <- paste0('tax_puma_cal/nc_to_taxsim_online/taxes_to_taxsim_', as.character(yr), '.csv')
  
 # calculate taxable income and write out results
 pop_taxes(con, yr) %>%
    write_csv(file_name, col_names = FALSE)

}


###########################################################################
#
# use online taxsim to calculate taxes, and store output as a txt file by 
# copying and pasting results into text file
#
############################################################################

############################################################################

### take estimated tax liability from taxsim output and distill into estimate household tax liability

# create list of all tax output files
file_names <- list.files('tax_puma_cal/nc_from_taxsim_online', full.names = TRUE)

# import all files into a list
tax_liab <- lapply(file_names, read_delim, delim = ' ') %>%
  # currently each year is in a separerate list element
  # combine all years into a single datarame
  # this is possible because there is a year column
  bind_rows() %>%
  # calculate total tax liability, which is the sum of
  # federal income, state income, and payroll taxes (FICA)
  mutate(total_taxes = fiitax + siitax + fica) %>%
  # group by year and serial number to calculate household taxes
  group_by(taxsim_id, year) %>%
  summarize(tax_liability = sum(total_taxes)) %>%
  ungroup() %>%
  # change name of taxsim_id to SERIALNO so that it matches PUMA terminology
  rename(SERIALNO = taxsim_id) %>%
  # sort by year and serial no
  arrange(year, SERIALNO)

# write out tax liabilities
#write_csv(tax_liab, 'tax_puma_cal/nc_tax_liabilities.csv')
