################################################################
# This file creates the data set for employment rates.
################################################################

library(tidyverse)

unzip_files <- function(file_path, output_dir) {
  
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
  files_remove <- paste0(file_folder, '/', list.files(path = file_folder, pattern = 'metadata|txt'))
  file.remove(files_remove)
  
}

clean_acs <- function(file_name, year) {
  
  # This function takes as input an ACS file as downlaoded from AFF,
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
    # convert geo_id, estimate, and moe to integers
    mutate(geo_id = as.numeric(geo_id),
           estimate = as.numeric(estimate),
           moe = as.numeric(moe),
           # add standard errors
           # reference: A compass for understanding and using American Community Survey Data, Oct. 2008, A-12
           se = moe / 1.645)
 
  # set year
  df$year <- year
  
  colnames(metadata) <- c('description', 'label')
  
  df <- left_join(df, metadata, by = 'label')
  
  return(df)
  
}