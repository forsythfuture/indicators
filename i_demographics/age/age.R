################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/misc_functions.R')

# folder to put raw data into
data_path <- '/Users/joannakelly/Dropbox/Indicators/i_demographics/age/data/raw_data' 

# full path and file name of zip file
zip_file <- '/Users/joannakelly/Dropbox/Indicators/zip_files/age_all_years.zip'

ff_unzip_files(zip_file,data_path)

#Delete 2 extra files from raw data

df <- ff_import_acs_tmp(data_path,
                  years = seq(2006, 2017, 1))

# write out data frame
write_csv(df, 'i_demographics/age/data/age_all_years.csv')

##******************************************************************************************************
#created temporary function to deal with extra files in zip folder
ff_import_acs_tmp <- function(raw_data_path, years) {
  
  # This file takes as input a .zip file of AFF downloaded data
  # and outputs a sinlge cleaned data set of all the files in the .zip file
  
  # input:
  #  zip_file: the file name and full path to the zip file
  #  raw_data_path: The folder that the raw data should be copied to
  #  years: vector of years represented in the data
  
  
  # list of files in zip file (each file represents a year of data)
  data_files <- paste0(raw_data_path, '/', dir(path=raw_data_path, pattern=".csv"))
  
  ### iterate through each file and year, extract data, and bind to previous year
  
  # initialize dataframe
  df_full <- data.frame()
  
  for (i in seq_along(data_files)) {
    
    # import one year of data
    df <- ff_clean_acs(data_files[i], years[i])
    
    # bind to previous year
    df_full <- df_full %>%
      bind_rows(df)
  }
  
  return(df_full)

}
