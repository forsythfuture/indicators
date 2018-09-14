################################################################
# This file contains functions that unzip and clean ACS files.
################################################################

library(tidyverse)


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
  files_remove <- paste0(output_dir, '/', list.files(path = output_dir, pattern = 'metadata|txt'))
  file.remove(files_remove)
  
}

ff_clean_acs <- function(file_name, year) {
  
  # This function takes as input an ACS file as downloaded from AFF,
  # adds descriptions, and converts to long form
  
  # Input:
  #   file_name: The file name to a csv file downloaded from AFF
  
  df <- read_csv(file_name)
  
  # The data is in wide-form where each item is a different column
  # we want to convert to long form where there are columns for estimate and and moe
  # and each row is a different item
  # The first row contains the column description
  # It includes the word 'Estimate;' for estimates and 'Margin of Error;' for moe
  # To convert to long form create two datasets, one for estimates and one for MOE
  # the datasets will then be merged
  
  df <- df %>%
    # delete unneeded columns
    select(-`GEO.id`, -`GEO.id2`) %>%
    # change column name
    rename(geo_description = `GEO.display-label`)
  
  # first row (under column header) contains metadata (description), so extract and place into dataframe
  # also include column headers because the headers will be used to merge metadata with data
  metadata <- t(df[1,]) %>%
    as.data.frame() %>%
    # column headers are the row names, convert to a column so that descriptions can be merged with data set
    mutate(label = row.names(.)) %>%
    # rename column
    rename(description = V1) %>%
    # filter out first row, which is the column description
    .[2: nrow(.),]
  
  # since the descriptions are extracted, we can delete the row containing them
  df <- df[2:nrow(df),]

  # convert from wide to long, where each estimate and moe is on a different row
  df <- df %>% 
    gather(label, estimate, -geo_description) %>%
    # combine descriptions, which are in the metadata object, with the data set containing data
    left_join(metadata, by = 'label')
      
  # create two different datasets, one for MOE and one for estimates
  # these two data sets will be merged so that there is one data set with the estimate and moe as seperate columns
  moe <- df %>%
    filter(str_detect(description, 'Margin of Error; ')) %>%
    # rename moe estimate column so it has a different name than the column with estimate values
    rename(moe = estimate)
  df <- df %>%
    filter(str_detect(description, 'Estimate; ')) 
  
  # estimate and moe data sets should have the same number of rows,
  # and the rows should match by row number
  # for example the fourth row for moe data set should contain the moe
  # for the fourth row of the estimate data set
  
  # first ensure each data set has the same number of rows, if not stop and throw error message
  if (nrow(df) != nrow(moe)) {
    stop('The estimate and moe data sets do not contain the same number of rows.')
  }
  
  # add moe values to data frame
  df$moe <- moe$moe
  # add year as a column
  df$year <- year
  

  df <- df %>%
    # convert estimate, and moe to numbers, description from factor to character
    mutate(estimate = as.numeric(estimate),
           moe = as.numeric(moe),
           description = as.character(description)) %>%
    # the ACS default is a 90% MOE, convert to 95% margin of error
    # reference: A compass for understanding and using ACS data, October 2008, A-12 
    mutate(moe = round((1.96/1.645) * moe, 2)) %>%
    # calcualte standard error and cv
    # reference: A compass for understanding and using American Community Survey Data, Oct. 2008, A-12
    mutate(se = round(moe / 1.96, 2),
           cv = round((se / estimate)*100, 2)) %>%
    # counties have the county name and then 'County, North Carolina'
    # only keep the copunty name in the geographic description
    separate(geo_description, into = c('geo_description', 'waste'), sep = 'County, ', fill='right') %>%
    # change order of columns
    select(geo_description, label, year, description, estimate, moe, se, cv)
    
  return(df)
  
}

ff_import_acs <- function(zip_file, raw_data_path, years) {
  
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