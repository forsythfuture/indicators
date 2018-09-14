################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/misc_functions.R')

# folder to put raw data into
data_path <- 'i_economic/homeownership/data/raw_data'

# full path and file name of zip file
zip_file <- 'zip_files/homeownership_fc_all__years.zip'

# import and clean homeownership data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
write_csv(df, 'i_economic/homeownership/data/homeownership_fc_all__years.csv')
