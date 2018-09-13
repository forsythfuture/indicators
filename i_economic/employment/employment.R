################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/misc_functions.R')

# import and clean employment data
df <- ff_import_acs('zip_files/aff_download.zip', 
                    'zip_files', years = seq(2006, 2016, 1))

# write out data frame
write_csv(df, 'i_economic/employment/data/employment_all_years_counties.csv')

## import Forsyth census tract data for 2016

# unzip census tract files
unzip_files('aff_download.zip', file_folder)

# clean census tract files
tracts <- clean_acs(paste0(file_folder, 'ACS_16_5YR_S2301_with_ann.csv'), 2016)

# write out data frame
#write_csv(tracts, paste0(file_folder, 'employment_tracts.csv'))
