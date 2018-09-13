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
  
  # first row (under column header) contains metadata, so extract and place into dataframe
  # also include column headers because the headers will be used to merge metadata with data
  metadata <- t(df[1,])
  
  # delete first row
  df <- df[2: nrow(df), ]
  
  # create two datasets, one for estimates and one for MOE
  # datasets will be merged
  estimates <- df %>%
    select(contains('GEO'), contains('EST'))
  
  moe <- df %>%
    select(contains('GEO'), contains('MOE'))
  
  # convert from wide to long, where each estimate and moe is on a different row
  df <- df %>% 
    select(-GEO.id) %>%
    gather(label, estimate, -GEO.id2, -`GEO.display-label`) %>%
    rename(geo_id = GEO.id2, geo_description = `GEO.display-label`)
  
  # filter out moe; so that it can be added as an additional column to the year dataset
  moe <- df %>%
    filter(str_detect(label, 'MOE'))
  
  # remove MOE rows
  df <- df %>%
    filter(!str_detect(label, 'MOE'))
  
  # add MOE as a new column to main dataset
  df$moe <- moe$estimate
  
  # add description to dataframe
  metadata <- as.data.frame(metadata)
  metadata$label <- rownames(metadata)
  

  df <- df %>%
    # convert geo_id, estimate, and moe to numbers
    mutate(geo_id = as.numeric(geo_id),
           estimate = as.numeric(estimate),
           moe = as.numeric(moe)) %>%
    # the ACS default is a 90% MOE, convert to 95% margin of error
    # reference: A compass for understanding and using ACS data, October 2008, A-12 
    mutate(moe = round((1.96/1.645) * moe, 2)) %>%
    # calcualte standard error and cv
    # reference: A compass for understanding and using American Community Survey Data, Oct. 2008, A-12
    mutate(se = round(moe / 1.96, 2),
           cv = round((se / estimate)*100, 2))
 
  # set year
  df$year <- year
  
  colnames(metadata) <- c('description', 'label')
  
  df <- left_join(df, metadata, by = 'label')
  
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
  
  # copy raw csv files to folder of users choice
  
  # create list of files names for raw csv files
  # file_names <- str_extract(data_files, '[/].*csv$')
  # append list of file names to the folder path
  # this creates the full file path for each csv file
  # end_location <- paste0(rep(raw_data_path, length(data_files)), file_names)
  
  # copy csv files from current location in the zip folder to final location
  # file.copy(data_files, end_location, overwrite = TRUE)
  
  return(df_full)
  
}