################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/misc_functions.R')

### import each year's data and bind to previous year

## import US, NC state and NC county data

# 2006-2016 is in a zip file, unzip files and add to folder

file_folder = 'i_economic/employment/data'

unzip_files('zip_files/employment_past_counties.zip', file_folder)

# list of files (each file represents a year of data)
data_files <- paste0(file_folder, '/', list.files(file_folder))

# list of years
years <- seq(2006, 2016, 1)

### iterate through each file and year, extract data, and bind to previous year

# initialize dataframe
emp <- data.frame()

for (i in seq_along(data_files)) {

  # import one year of data
  df <- clean_acs(data_files[i], years[i])
  
  # bind to previous year
  emp <- emp %>%
    bind_rows(df)
}

# remove individual yearly files
file.remove(data_files)

# write out data frame
write_csv(emp, paste0(file_folder, '/', 'employment_all_years_counties.csv'))

## import Forsyth census tract data for 2016

# unzip census tract files
unzip_files('aff_download.zip', file_folder)

# clean census tract files
tracts <- clean_acs(paste0(file_folder, 'ACS_16_5YR_S2301_with_ann.csv'), 2016)

# write out data frame
#write_csv(tracts, paste0(file_folder, 'employment_tracts.csv'))
