################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/misc_functions.R')

# folder to put raw data into
data_path <- 'i_economic/employment/data/raw_data'

# full path and file name of zip file
zip_file <- 'zip_files/employment_all_counties_years.zip'

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
write_csv(df, 'i_economic/employment/data/employment_all_years_counties.csv')
