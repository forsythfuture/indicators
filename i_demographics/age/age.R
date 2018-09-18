################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)
source('functions/acs/acs_misc_functions.R')

# folder to put raw data into
data_path <- 'i_demographics/age/data/raw_data' 

# full path and file name of zip file
zip_file <- 'zip_files/age_all_years.zip'

df <- ff_import_acs_tmp(zip_file,
                        data_path,
                  years = seq(2006, 2017, 1))

# write out data frame
write_csv(df, 'i_demographics/age/data/age_all_years.csv')

for (file in list.files('i_demographics/age/data/raw_data')) {
  print(file)
}
junk <- dir(path="i_demographics/age/data/raw_data", pattern="S0101") # ?dir
file.remove(junk) # ?file.remove

library(tidyverse)

ff_unzip_files('zip_files/age_all_years.zip','i_demographics/age/data/raw_data')


ff_unzip_files <- function(file_path, output_dir) {
        
        # This function takes a zipped file of ACS data, unzips it,
        # and only keeps the data files
        
        # Input:
        # file_path: path to zip file
        # output_dir: directory for outpt
        
        # unzip files
        unzip(file_path, exdir = output_dir)
        
        # we do not want text files and metadata files; metadata is in data file
        # delete these files
        # need to paste directory to file names
        files_remove <- paste0(output_dir, '/', list.files(path = output_dir, pattern = 'metadata|txt|S1903'))
        file.remove(files_remove)
}

files_remove <- paste0('i_demographics/age/data/raw_data', '/', list.files(path = 'i_demographics/age/data/raw_data'), pattern = 'S0101', inv=T, value=T)
file.remove(files_remove)


if (file in list.files('i_demographics/age/data/raw_data')) 

##******************************************************************************************************
ff_import_acs_tmp <- function(zip_file, raw_data_path, years) {
        
        # This file takes as input a .zip file of AFF downloaded data
        # and outputs a sinlge cleaned data set of all the files in the .zip file
        
        # input:
        #  zip_file: the file name and full path to the zip file
        #  raw_data_path: The folder that the raw data should be copied to
        #  years: vector of years represented in the data
        
        # unzip files
        # they will be temporarily stored in the same folder as the zip files
        ff_unzip_files(zip_file, raw_data_path)
        
        # list of files in zip file (each file represents a year of data)
        data_files <- paste0(raw_data_path, '/', dir(path=raw_data_path, pattern=".csv"))
        
        # ensure that the raw data files only contain those with the required table number
        data_files <- str_subset(data_files,pattern="S0101")
        
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
