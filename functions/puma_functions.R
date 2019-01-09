###################################################################################
# This file contains functions for working with PUMA data
###################################################################################

library(data.table)
library(tidyverse)

puma_area_code <- function(counties, names_file, puma12 = TRUE) {
  
  # This function takes as input a vector of county names
  # and return all PUMA area codes within a county
  
  # convert vector of county names into single RE string
  counties <- paste(counties, collapse = '|')
  
  # create columns to select based on whether user is selcting PUMA10 or PUMA2K
  cols <- if(puma12 == TRUE)  c('puma12', 'cntyname') else c('puma2k', 'cntyname')
  
  # read in table of PUMA names and filter for counties of interest
  puma_names <- read_csv(names_file) %>%
    # filter for needed counties
    filter(str_detect(cntyname, counties)) %>%
    # extract area code for either 2000 or 2010
    select(cols)
  
  # rename first column so that it has the same name, whether from 2K or 2010
  # this is also the same name as the PUMA area code column in the PUMA datasets
  names(puma_names)[1] <- 'PUMA'
  
  # sort by area code
  puma_names <- arrange(puma_names, PUMA)
  
  return(puma_names)
}

clean_race <- function(df) {
  
  # this function recodes race values as follows:
  #   Recode Hispanics as 100 in RAC1P column
  #   Recode race to 1 = white, 2 = AA, 3 = Hispanic, and 4 = other
  #   Delete hispanic column
  
  df <- df %>%
    # make race 100 for Hispanic if person is of hispanic origin
    mutate(RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P)) %>%
    select(-HISP)
  
  # recode race to group races we will not use into one race
  # this will reduce the number of groups and speed up grouping
  
  race_labels <- c(seq(1, 9), 100)
  race_recode <- c(1, 2, rep(4, 7), 3)
  
  df$RAC1P <- plyr::mapvalues(df$RAC1P, race_labels, race_recode)
  
  return(df)
  
}

clean_demographics <- function(df, age_bins) {
  
  # This function cleans the demographics of a PUMS dataset.
  # It does the following:
  #   Bin ages
  #   Recode Hispanics as 100 in RAC1P column
  #   Recode race to 1 = white, 2 = AA, 3 = Hispanic, and 4 = other
  #
  # age_bins: vector of ages with each number representing the highest
  #           age in the group
  #
  # Data frame must have columns for each of these demographics to work
  
  # create labels that are the end age
  age_labels <- age_bins[-1]
  
  df <- df %>%
    # create age bins
    mutate(AGEP = cut(AGEP, breaks = !!age_bins, 
                      labels = !!age_labels, 
                      include.lowest = TRUE),
           # convert to integer
           AGEP = as.integer(as.character(AGEP)),
           # make race 100 for Hispanic if person is of hispanic origin
           RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P)) %>%
    select(-HISP)
  
  # recode race to group races we will not use into one race
  # this will reduce the number of groups and speed up grouping
  
  race_labels <- c(seq(1, 9), 100)
  race_recode <- c(1, 2, rep(4, 7), 3)
  
  df$RAC1P <- plyr::mapvalues(df$RAC1P, race_labels, race_recode)
  
  return(df)
  
}

table_name <- function(table_type, year) {
  
  # This function creates either the population or housing 
  # table name given the year
  # parameters:
  #   table_type: either 'population', or 'housing'
  #   year: the year
  
  # for housing tables use 'h' as prefix for table name
  # for population tables use 'p' as prefix for table names
  # if table_type is nether of these two, return stop message
  if (table_type == 'population') {
    
    prefix <- 'p_'
    
  } else if (table_type =='housing') {
    
    prefix <- 'h_'
    
  } else {
    
    stop("table_type must be either 'population' or 'housing'")
    
  }
  
  # create table name
  tbl_name <- as.character(year) %>%
    str_extract(., '[0-9][0-9]$') %>%
    paste0(prefix, .)
  
  return(tbl_name)
  
}

groupings <- function(df, level, year) {
  
  # this function determines what groupings to use in calculating Palma
  # Input:
  #     df: dataframe of PUMA values
  #     levels: 'US', 'state', 'county', 'puma'
  
  # convert level to lowercase
  level <- str_to_lower(level)
  
  # replace PUMA codes with county names if true
  if (level %in% c('county', 'counties')) {
    
    # dataframe of all NC puma codes and county names
    # codes are different starting in 2012, so ensure we are pulling in the right year
    if (year > 2011) {
      
      nc_codes <- puma_area_code(letters, 'functions/puma_counties.csv') %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    } else {
      
      nc_codes <- puma_area_code(letters, 'functions/puma_counties.csv', puma12 = FALSE) %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    }
    
    # add county name to income dataframe
    df <- df %>%
      left_join(nc_codes, by = 'PUMA') %>%
      rename(group = cntyname)
    
  } else if (level %in% c('united states', 'us')) {
    
    # make integer to conserve ram
    df$group <- 0
    
  } else if (level %in% c('state', 'states')) {
    
    df$group <- df$ST
    
  } else if (level %in% c('puma', 'pums')) {
    
    df$group <- df$PUMA
    
  }
  
  return(df)
  
}