################################################################

library(tidyverse)
source('functions/acs/acs_misc_functions.R')

# folder to put raw data into
data_path <- '/Users/joannakelly/Dropbox/Indicators/i_education/post_secondary_completion/data/raw_data/age'
data_path <- '/Users/joannakelly/Dropbox/Indicators/i_education/post_secondary_completion/data/raw_data/black'
data_path <- '/Users/joannakelly/Dropbox/Indicators/i_education/post_secondary_completion/data/raw_data/hispanic'
data_path <- '/Users/joannakelly/Dropbox/Indicators/i_education/post_secondary_completion/data/raw_data/white'
data_path <- '/Users/joannakelly/Dropbox/Indicators/i_education/post_secondary_completion/data/raw_data/sex'
data_path <- '/Users/joannakelly/Dropbox/Indicators/i_education/post_secondary_completion/data/raw_data/total_pop'

# full path and file name of zip file
zip_file <- '/Users/joannakelly/Dropbox/Indicators/zip_files/educ_attain_age.zip'
zip_file <- '/Users/joannakelly/Dropbox/Indicators/zip_files/educ_attain_black.zip'
zip_file <- '/Users/joannakelly/Dropbox/Indicators/zip_files/educ_attain_hispanic.zip'
zip_file <- '/Users/joannakelly/Dropbox/Indicators/zip_files/educ_attain_white.zip'
zip_file <- '/Users/joannakelly/Dropbox/Indicators/zip_files/educ_attain_sex.zip'
zip_file <- '/Users/joannakelly/Dropbox/Indicators/zip_files/educ_attain_pop_sex_by_age.zip'

ff_unzip_files(zip_file,data_path)

# import and clean employment data
df <- ff_import_acs(zip_file,
                    data_path, 
                    years = seq(2006, 2017, 1))

# write out data frame

write_csv(df, '/Users/joannakelly/Dropbox/Indicators/i_education/post_secondary_completion/data/educ_attain_sex.csv')

