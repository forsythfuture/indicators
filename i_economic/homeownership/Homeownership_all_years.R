################################################################
# This file creates the data set for homeownership rates.
################################################################

library(tidyverse)
source('functions/acs_load_funcs.R')

# note: to bring in multiple fiels you must first run the import function once
# to create raw files; then run the import function again, but putting raw data into
# different folder and immediately deleting

# folder to put raw data into
data_path <- 'i_economic/homeownership/data/raw_data'

# full path and file name of zip files
# there are various homeownership zip files, and they all start with 'homeowner_'
# identify the home ownership files and paste directory to file names
homeowner_zip <- paste0('zip_files/', list.files('zip_files', pattern='homeowner_'))

## iterate through each zip file, download and clean data, and place dataset in list



# thi iterate creates the raw data files
for (i in seq_along(homeowner_zip)) {
  
  ff_import_acs(homeowner_zip[i],
                data_path, 
                years = seq(2006, 2017, 1))
  
  homeowner_list[[i]] <- df
  
}


# this iterate creates the singel dataset

# initialize list to store each seperate homeowner dataset
homeowner_list <- list()

# folder to put raw data into; will be deleted
data_path <- 'i_economic/homeownership/data/raw_data2'

# this iterate creates the singel dataset
for (i in seq_along(homeowner_zip)) {
  
  df <- ff_import_acs(homeowner_zip[i],
                      data_path, 
                      years = seq(2006, 2017, 1))
  
  # add filename as a column
  df$file <- homeowner_zip[i]
  
  homeowner_list[[i]] <- df
  
  # remove all files from foder
  file.remove(paste0(data_path, '/', list.files(data_path)))
  
}

# write out data frame
write_csv(df, 'i_economic/homeownership/data/homeownership_fc_all__years.csv')
