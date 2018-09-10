################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/misc_functions.R')

### import each year's data and bind to previous year

file_folder = 'i_economic/employment/data'

# list of files (each file represents a year of data)
data_files <- paste0(file_folder, '/', list.files(file_folder))

# list of years
years <- seq(2006, 2017, 1)

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

# write out data frame
#write_csv(emp, 'employment_all_years_counties.csv')