########################################################
# 
# This script imports and cleans poverty tables
#
########################################################

library(tidyverse)
source('functions/acs/acs_misc_functions.R')

# folder to put raw data into
data_path <- 'i_economic/poverty/data/raw_data'

# full path and file name of zip file
zip_file <- 'zip_files/poverty.zip'

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
#write_csv(df, 'i_economic/poverty/data/poverty_all_years_counties.csv')

### age data is in a different table

# folder to put raw data into
data_path <- 'i_economic/poverty/data/raw_data2'

# full path and file name of zip file
zip_file <- 'zip_files/poverty_by_age.zip'

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
#write_csv(df, 'i_economic/poverty/data/poverty_age_groups.csv')
