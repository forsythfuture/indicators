###########################################################
#
# This script imports and cleans Median Income data
# Median Income is found on AFF at table S1903
#
###########################################################

library(tidyverse)
source('functions/acs_load_funcs.R')

## median income table without gender

# folder to put raw data into
data_path <- 'i_economic/median_income/data/raw_data'

# full path and file name of zip file
zip_file <- 'zip_files/median_income.zip'

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
#write_csv(df, 'i_economic/median_income/data/median_income_all_years_counties.csv')

## median income table with gender

# folder to put raw data into
data_path <- 'i_economic/median_income/data/raw_data/gender_income'

# full path and file name of zip file
zip_file <- 'zip_files/median_income_sex.zip'

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
#write_csv(df, 'i_economic/median_income/data/median_income_gender_all_years_counties.csv')
