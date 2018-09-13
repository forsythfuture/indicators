###########################################################
#
# This script imports and cleans Median Income data
# Median Income is found on AFF at table S1903
#
###########################################################

library(tidyverse)
source('functions/misc_functions.R')

# folder to put raw data into
data_path <- 'i_economic/median_income/data/raw_data'

# full path and file name of zip file
zip_file <- 'zip_files/median_income.zip'

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
write_csv(df, 'i_economic/median_income/data/median_income.csv')
