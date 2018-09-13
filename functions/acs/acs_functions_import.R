########################################################################################################
#
# Import ACS data.
#
# The following functions import data into ACS using teh census API and tidycensus package.
# The only function that needs to be run in R scripts is 'ff_import_acs' (the final function)
# The general framework is as follows:
#     tidycensus essentially only allows users to import one geographic unit, one year, and one table
#     at a time. These three tasks are broken down into three different functions: 
#     
#       ff_import_tables returns all tables for a specific geographic unit and year
#    
#       ff_import_years calls ff_inport_tables and returns the tables created by ff_import_tables for 
#       a series of years
#
#       ff_import_acs then calls ff_import_years and returns all tables for multiple counties for all
#       the years created by ff_import_years
#
# The final result is that ff_import_acs can be used to return tables for multiple years and geographic units
#
# Important Note: All data imports with a 95% confidence level of the MOE
# This differs from ACS fact finder, which shows the 90% confidence level
#
########################################################################################################

library(tidyverse)
library(tidycensus)

ff_table_names <- function(table_number) {
  
  # Load all acs variable names.
  var_names <- read_csv('acs_variable_names.csv')
  
  # The following function creates a vector of table names for all tables within a group.
  # For example, entering the parameter 'B20017' returns B20017, B20017A, B20017B, etc.
  # This function add table and variable descriptions to the data, for ease of understanding
  
  table_names <- var_names %>%
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



ff_import_tables <- function(table_number, state, county, year_end) {
  
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




ff_import_years <- function(table_number, state, county, year_start, year_end) {
  
  # This function returns tables for a series of years; for a single geographic area
  # 1 year ACS only goes back to 2011
  
  # vector of the years that are needed
  years <- seq(year_start, year_end, 1)
  
  
  # iterate through each year, pulling all tables for the given year
  for (year in years) {
    
    # if the year is the first year in a series, or if only one year is needed,
    # create the initial dataframe, which subsequent years will be added to
    
    if ((length(years) == 1) | (which(years == year) == 1)) {
      
      year_first <- ff_import_tables(table_number, state, county, year)
      
      # add column indicating year
      year_first$year <- year
      
      print(year)
      
    } else {
      
      # import table for subesequent year
      year_next <- ff_import_tables(table_number, state, county, year)
      
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



ff_import_acs <- function(table_number, state, county, year_start, year_end) {
  
  # This function returns a table for multiple geographic units (county and state combinations)
  # When importing data, this is the only function needed
  #
  # The state and county parameters are separate vectors of all the states and counties
  # the length of each vector must be the same, and they must overlap.
  # For example, the third state must match the third county.
  # When running this script, state and counties can be identified by running
  # the file comparison_counties.R. This will return a data frame of comparison counties.

  # Ensure state and county vectors are the same length.
  # Throw an error if they are different lengths
  if (length(state) != (length(county)))
    stop("State and county vectors must be the same length")
  
  # iterate through each state and county vector, pulling all tables and years
  
  for (i in seq_along(1:length(county))) {
    
    # if the geo is the first in a vector, or if only one geo is needed,
    # create the initial dataframe, which subsequent years will be added to
    
    if (i == 1) {
      
      geo_first <- ff_import_years(table_number, state[[i]], county[[i]], year_start, year_end)
      
      print(paste(state[[i]], county[[i]], sep=', '))
      
      # create dataframes for subsequent geographic areas and merge to first data frame
      
    } else {
      
      geo_next <- ff_import_years(table_number, state[[i]], county[[i]], year_start, year_end)
      
      geo_first <- geo_first %>%
        bind_rows(geo_next)
      
      print(paste(state[[i]], county[[i]], sep=', '))
      
    }
    
  }
  
  # calcuate standard error
  # note: since 95% confidence intervals are used, the moe is divided
  # by 1.96, not 1.645 when using the 90% moe's from AFF
  geo_first$se <- geo_first$moe / 1.96
  
  return(geo_first)
  
}
