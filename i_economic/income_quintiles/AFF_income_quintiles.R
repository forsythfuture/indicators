###########################################################
#
# This script imports and cleans Quintiles data
# Income quintiles are found in AFF table B19080
#
###########################################################

library(tidyverse)
source('functions/acs_load_funcs.R')

# folder to put raw data into
data_path <- 'i_economic/income_quintiles/data/raw_data'

# full path and file name of zip file
zip_file <- 'zip_files/quintiles_aff.zip'

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame
write_csv(df, 'i_economic/income_quintiles/data/income_quintiles_all_years_counties.csv')

