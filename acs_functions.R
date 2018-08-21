########################################################################################################
# This file contains commonly used functions to access ACS data with the tidycensus and acs packages.
# The functions import and manipulate ACS data.
#
# All function names should start with 'ff' so that when code is reviewed it is clear that the 
# functon was custom built.
########################################################################################################

library(tidyverse)
library(tidycensus)

# This dataframe serves as a master list for comparison counties
# If comparison counties change only this list needs to be updated
compare <- tribble(~county, ~state,
                   #---/---#
                   'Forsyth', 'NC',
                   'Guilford', 'NC',
                   'Pulaski', 'AR')

# save the current year as an object
# this allows us to update all datasets without entering the current year in every script
current_year <- 2017

# This function loads all acs variable names. It supplies parameters and values to other functions,
# and will not be called within R scripts
var_names16 <- load_variables(2016, "acs1")

########### Import data  ###########

# The following functions import data into ACS
# The only function that needs to be run in R scripts is 'ff_import_acs_county'
# The general framework is as follows:
#     tidycensus essentially only allows users to import one geographic unit, one year, and one table
#     at a time. These three tasks are broken down into three different functions. Then, the function
#     'ff_import_acs_county' combines these three functions, so that multiple tables, geographic units, and
#     years can be imported with one function call.
#
# Important Note: All data imports with a 95% confidence level of the MOE
# This differs from ACS fact finder, which shows the 90% confidence level


ff_table_names <- function(table_number) {
  
  # The following function creates a vector of table names for all tables within a group.
  # For example, entering the parameter 'B20017' returns B20017, B20017A, B20017B, etc.
  # This function add table and variable descriptions to the data, for ease of understanding
  
  table_names <- var_names16 %>%
    # filter data set of table names for tables that start with the entered table number
    filter(str_detect(.$name, str_c('^', table_number))) %>%
    # remove letter at the end of the varible names, since these letters are not at the end of the data table
    mutate(name = str_sub(.$name, 1, -2),
           # for variable decription, replace '!!' with new line '\n'
           label = str_replace_all(.$label, '!!', '\n'))
  
  # remove variables from table names; this leaves us with the different tables
  # the varible names are after the '_'; therefore use regular expressions to remove characters after '_';
  # then store the distinct table names in a vector
  #table_names$table_num <- str_match(table_names$name, '(.*)_') %>% # isolte table name
  #  .[1:nrow(table_names), 2] %>% # only keep column with table name
  #  unique() # only keep unique values
  
  return(table_names)
}



ff_import_tables_county <- function(table_number, state, county, year_end) {
  
  # This function returns a dataframe of multiple table numbers at one time
  
  # pull table descriptions and numbers of all tables
  table_decription_df <- ff_table_names(table_number)
  
  # the table_description parameter is the dataframe output of ff_table_names
  # this command seperates table numbers from the columns describing them
  table_nums <- str_match(table_decription_df$name, '(.*)_') %>% # isolate table name
    .[1:nrow(table_decription_df), 2] %>% # only keep column with table name
    unique() # only keep unique values
  
  # itereate through each table number
  for (i in seq_along(table_nums)) {

    # the first dataframe must be created seperately from the others,
    # and then the other dataframes will bs merged into the first one
    
    if (i == 1) {
      
      # create first dataframe
      # others will be merged into this one
      first_table <- get_acs(geography = "county", 
                             table = table_nums[[i]], year = year_end, survey = 'acs1',
                             state = state, county = county,
                             moe_level = 95) %>%
        # add variable descriptions to table
        left_join(table_decription_df, by = c('variable' = 'name'))
      
      print(table_nums[[i]])
      
    } else {
      
      # create dataframes for other tables, and merge them into the main dataframe
      other_tables <- get_acs(geography = "county", 
                              table = table_nums[[i]], year = year_end, survey = 'acs1',
                              state = state, county = county,
                              moe_level = 95)  %>%
        # add variable descriptions to table
        left_join(table_decription_df, by = c('variable' = 'name'))
      
      first_table <- first_table %>%
        bind_rows(other_tables)
      
      print(table_nums[[i]])
      
    }
  }
  
  return(first_table)
  
}




ff_import_years_county <- function(table_number, state, county, year_start, year_end) {
  
  # This function returns tables for a series of years; for a single geographic area
  # 1 year ACS only goes back to 2011
  
  # vector of the years that are needed
  years <- seq(year_start, year_end, 1)
  
  
  # iterate through each year, pulling all tables for the given year
  for (year in years) {
  
    # if the year is the first year in a series, or if only one year is needed,
    # create the initial dataframe, which subsequent years will be added to
    
    if ((length(years) == 1) | (which(years == year) == 1)) {
      
      year_first <- ff_import_tables_county(table_number, state, county, year)
      
      # add column indicating year
      year_first$year <- year
      
      print(year)
      
    } else {
      
      # import table for subesuent year
      year_next <- ff_import_tables_county(table_number, state, county, year)
      
      # add column indicating year
      year_next$year <- year
      
      # bind this table to the first year's data frame
      year_first <- year_first %>%
        bind_rows(year_next)
      
      print(year)
    }
  }
  
  return(year_first)
  
}



ff_import_acs_county <- function(table_number, state, county, year_start, year_end) {
  
  # This function returns a table for multiple geographic units (county and state combinations)
  # When importing data, this is the only function needed
  
  # The state and county parameters are separate vectors of all the states and counties
  # the length of each vector must be the same, and they must overlap.
  # For example, the third state must match the third county
  
  # Ensure state and county vectors are the same length.
  # Throw an error if they are different lengths
  if (length(state) != (length(county)))
    stop("State and county vectors must be the same length")
  
  # iterate through each state and county vector, pulling all tables and years
  
  for (i in seq_along(1:length(county))) {
    
    # if the geo is the first in a vector, or if only one geo is needed,
    # create the initial dataframe, which subsequent years will be added to
    
    if (i == 1) {
    
      geo_first <- ff_import_years_county(table_number, state[[i]], county[[i]], year_start, year_end)
      
      print(paste(state[[i]], county[[i]], sep=', '))
    
    # create dataframes for subsequent geographic areas and merge to first data frame
      
    } else {
      
      geo_next <- ff_import_years_county(table_number, state[[i]], county[[i]], year_start, year_end)
      
      geo_first <- geo_first %>%
        bind_rows(geo_next)
      
      print(paste(state[[i]], county[[i]], sep=', '))
      
    }
    
  }
  
  return(geo_first)

}


######################################################

########### Data calculations  ###########

# The following functions perform calculations on ACS data,
# such as adding proportions and conducting tests

######################################################

########### Misc  ###########

# The following functions are miscellaneous


ff_ethnicity <- function(df) {
  
  # This function filters for the following ethnicities:
  #    White Alone, Not Hispanic or Latino; Black or African American Alone; Hispanic or Latino
  # It takes as input a dataframe of ACS data create by 'ff_import_acs_county'
  
  # keep these ethnicities
  keep_ethnicities <- c('ALL', 'WHITE ALONE, NOT HISPANIC OR LATINO', 
                        'BLACK OR AFRICAN AMERICAN ALONE', 'HISPANIC OR LATINO')
  
  # Ethnicity is located in 'concept' column, so wrangle this column to extract ethnicity
  # and place in its own column
  df$ethnicity <- df$concept %>%
    # Identify the ethnicity part of the 'concept' variable.
    # This part is at the end of the line, and surrounded by brackets.
    str_match("[(][A-Z| |,]*?[)]$") %>%
    # remove brackets - first and last character - from ethnicity
    str_sub(2, -2) %>%
    # NA values for ethnicity represent totals, change NA values to 'all'
    replace_na('ALL')
  
  # filter for column in the listed ethnicities
  df <- filter(df, ethnicity %in% keep_ethnicities)
  
  return(df)
}


ff_update_county <- function(master_df_path, table_num, state, county, current_year) {
  
  # This function updates the csv files by adding the current year's data
  
  # import current year ACS data
  update_df <- ff_import_acs_county(table_number, state, county, current_year, current_year)
  
  # import local copy of master csv dataset and bind updated data to it
  master_df <- read_csv(master_df_path) %>%
    bind_rows(update_df)
  
  return(master_df)
}
