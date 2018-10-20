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

# iterate through each year and create dataset for TAXSIM; then save dataset as csv file

for (yr in seq(2006, 2017)) {
  
  print(yr)
  
  # create file name of output based on year
  file_name <- paste0('taxes_', as.character(yr), '.csv')
  
  a <- pop_taxes(con, yr) #%>%
    #write_csv(., file_name, col_names = FALSE)
  b <- house_incomes(con, 2017, state = 37, area_code = 1801)
}

c <- full_join(a, b, by = 'SERIALNO')

c <- select(a, RECID, fips) %>%
  group_by(RECID) %>%
  summarize(fips = mean(fips)) %>%
  rename(SERIALNO = RECID) %>%
  # in 2017, serial IDs have leading '2017'; remove this
  mutate(SERIALNO = as.character(SERIALNO),
         SERIALNO = str_replace_all(SERIALNO, '^2017', ''),
         SERIALNO = as.integer(SERIALNO))

d <- full_join(house, c, by = 'SERIALNO')


# the top part has the wrong states
# for now we only need NC, so filter for NC and change state code

# get list of files that were output above
list_files <- list.files('tax_puma_cal/raw_incomes')
dir <- 'tax_puma_cal/raw_incomes/'

# add directory to file names
file_path <- paste0(dir, list_files)

# iterate through each file
for (i in seq_along(file_path)) {
  
  print(file_path)
  
  a <- read_csv(file_path[12], col_names = FALSE) %>%
    filter(X3 == 37) %>%
    mutate(X3 = 34) %>%
    # replafce any NA values with 0
    mutate_all(funs(replace_na(., 0)))  %>%
    # in script, EITC dependents is maxed at 3;
    # make total number
    mutate(X10 = X7) %>%
    write_csv(., paste0('tax_puma_cal/nc_to_taxsim/', list_files[i]), col_names = FALSE)
  
}

a$X1 <- as.integer(str_replace_all(as.character(a$X1), '^2017', ''))

a <- a[a$X7 <= 15,]
write_csv(a, paste0('tax_puma_cal/nc_to_taxsim/', list_files[12]), col_names = FALSE)
