########################################################
# 
# This script imports and cleans poverty tables
#
########################################################
install.packages('tidyverse')
library(tidyverse)
source('functions/acs/acs_misc_functions.R')

# folder to put raw data into
data_path <- 'i_demographics/data/raw_data'

family_structure_zip <- paste0('zip_files/', list.files('zip_files', pattern='family_structure_'))

## iterate through each zip file, download and clean data, and place dataset in list

# this creates the raw data files
# it does nothing but create the raw data files
for (i in seq_along(family_structure_zip)) {
  
  ff_import_acs(family_structure_zip[i],
                data_path, 
                years = seq(2006, 2017, 1))
}


## this section creates the single dataset for export

# initialize list to store each seperate post sec dataset
family_structure_list <- list()

# folder to put raw data into; will be deleted
data_path <- 'i_demographics/data/raw_data2'

# this iterate creates the single dataset
for (i in seq_along(family_structure_zip)) {
  
  df <- ff_import_acs(family_structure_zip[i],
                      data_path, 
                      years = seq(2006, 2017, 1))
  
  # add filename as a column
  df$file <- family_structure_zip[i]
  
  family_structure_list[[i]] <- df
  
  # remove all files from folder
  file.remove(paste0(data_path, '/', list.files(data_path)))
  
}

# bind all datasets into a single dataset
family_structure <- bind_rows(family_structure_list) %>%
  # convert filenames to descriptive label for row
  mutate(file = str_replace_all(file, '.*_aa.*', 'African American')) %>%
  mutate(file = str_replace_all(file, '.*_hl.*', 'Hispanic/Latino')) %>%
  mutate(file = str_replace_all(file, '.*_total.*', 'total')) %>%
  mutate(file = str_replace_all(file, '.*_white.*', 'White, non-Hispanic'))

# 'raw_data2' file should be empty and can be deleted

# write out data frame
write_csv(family_structure, 'i_demographics/data/family_structure_all_years.csv')




