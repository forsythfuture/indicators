################################################################
# This file creates the data set for homeownership rates.
################################################################

library(tidyverse)
source('functions/acs_load_funcs.R')

# note: to bring in multiple fiels you must first run the import function once
# to create raw files; then run the import function again, but putting raw data into
# different folder and immediately deleting

# folder to put raw data into
data_path <- 'i_education/post_secondary_completion/data/raw_data'

# full path and file name of zip files
# there are various homeownership zip files, and they all start with 'homeowner_'
# identify the home ownership files and paste directory to file names
post_sec_zip <- paste0('zip_files/', list.files('zip_files', pattern='educ_attain_'))

## iterate through each zip file, download and clean data, and place dataset in list

# initialize list to store each seperate post secondary completion dataset
post_sec_list <- list()

# this iterate creates the raw data files
for (i in seq_along(post_sec_zip)) {
  
  ff_import_acs(post_sec_zip[i],
                data_path, 
                years = seq(2006, 2017, 1))
  
  post_sec_list[[i]] <- df
  
}


# this iterate creates the single dataset



# folder to put raw data into; will be deleted
data_path <- 'i_education/post_secondary_completion/data/raw_data2'

# this iterate creates the single dataset
for (i in seq_along(post_sec_zip)) {
  
  df <- ff_import_acs(post_sec_zip[i],
                      data_path, 
                      years = seq(2006, 2017, 1))
  
  # add filename as a column
  df$file <- post_sec_zip[i]
  
  post_sec_list[[i]] <- df
  
  # remove all files from foder
  file.remove(paste0(data_path, '/', list.files(data_path)))
  
}

# bind all datasets into a single dataset
post_sec <- bind_rows(post_sec_list) %>%
  # convert filenames to descriptive label for row
  mutate(file = str_replace_all(file, '.*_aa.*', 'African American')) %>%
  mutate(file = str_replace_all(file, '.*_age.*', 'age group')) %>%
  mutate(file = str_replace_all(file, '.*_hl.*', 'Hispanic/Latino')) %>%
  mutate(file = str_replace_all(file, '.*_total.*', 'total')) %>%
  mutate(file = str_replace_all(file, '.*_sex.*', 'gender')) %>%
  mutate(file = str_replace_all(file, '.*_white.*', 'White, non-Hispanic'))
  
# write out data frame
write_csv(post_sec, 'i_education/post_secondary_completion/data/post_sec_all_years.csv')
