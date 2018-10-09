########################################################################################################
# # Import ACS data.
#
# Note: This is for working with Tidycensus package.
#
# The following functions import data into ACS using teh census API and tidycensus package.
# The only function that needs to be run in R scripts is 'ff_import_tidycensus' (the final function)
# The general framework is as follows:
#     tidycensus essentially only allows users to import one geographic unit, one year, and one table
#     at a time. These three tasks are broken down into three different functions: 
#     
#       ff_import_tables returns all tables for a specific geographic unit and year
#    
#       ff_import_years calls ff_import_tables and returns the tables created by ff_import_tables for 
#       a series of years
#
#       ff_import_tidycensus then calls ff_import_years and returns all tables for multiple counties for all
#       the years created by ff_import_years
#
# The final result is that ff_import_tidycensus can be used to return tables for multiple years and geographic units
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



ff_import_tables <- function(geography, table_number, state=NULL, county=NULL, year_end, acs_data) {
  
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
      first_table <- get_acs(# use county for geography if there is a county listed in the parameters,
        # use state for geography if county is null
        geography = geography, 
        table = table_nums[[i]], 
        year = year_end,
        # use 'acs1' for data from 2015 and later; and use 'acs/acs1' for data prior to 2015
        survey = ifelse(year_end >= 2015, str_match(acs_data, '[/](.*)')[[2]], acs_data),
        state = state, 
        county = county,
        moe_level = 95) %>%
        # add variable descriptions to table
        left_join(table_decription_df, by = c('variable' = 'name'))
      
    } else {
      
      # create dataframes for other tables, and merge them into the main dataframe
      other_tables <- get_acs(# use county for geography if there is a county listed in the parameters,
        # use state for geography if county is null
        geography = geography, 
        table = table_nums[[i]], 
        year = year_end, 
        survey = ifelse(year_end >= 2015, str_match(acs_data, '[/](.*)')[[2]], acs_data),
        state = state, 
        county = county,
        moe_level = 95)  %>%
        # add variable descriptions to table
        left_join(table_decription_df, by = c('variable' = 'name'))
      
      first_table <- first_table %>%
        bind_rows(other_tables)
      
    }
  }
  
  return(first_table)
  
}




ff_import_years <- function(geography, table_number, state=NULL, county=NULL, 
                            year_start, year_end, acs_data) {
  
  # This function returns tables for a series of years; for a single geographic area
  # 1 year ACS only goes back to 2011
  
  # vector of the years that are needed
  years <- seq(year_start, year_end, 1)
  
  
  # iterate through each year, pulling all tables for the given year
  for (year in years) {
    
    # if the year is the first year in a series, or if only one year is needed,
    # create the initial dataframe, which subsequent years will be added to
    
    if ((length(years) == 1) | (which(years == year) == 1)) {
      
      year_first <- ff_import_tables(geography, table_number, state, county, year, acs_data)
      
      # add column indicating year
      year_first$year <- year
      
      print(year)
      
    } else {
      
      # import table for subesequent year
      year_next <- ff_import_tables(geography, table_number, state, county, year, acs_data)
      
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



ff_import_tidycensus <- function(geography, table_number, state=NULL, county=NULL, 
                          year_start, year_end, acs_data = 'acs_acs1') {
  
  # This function returns a table for multiple geographic units (county and state combinations)
  # When importing data, this is the only function needed
  #
  # The geography parameter reflects the geographic level of interest and can be 'us', 'state', or 'county
  #
  # The state and county parameters are separate vectors of all the states and counties
  # the length of each vector must be the same, and they must overlap.
  # For example, the third state must match the third county.
  # When running this script, state and counties can be identified by running
  # the file comparison_counties.R. This will return a data frame of comparison counties.
  #
  # The output also calculates and returns the standard error.
  
  # Ensure state and county vectors are the same length.
  # Throw an error if they are different lengths
  # This is only needed if the geography is count
  if (!is.null(county)) {
    if (length(state) != (length(county)))
      stop("State and county vectors must be the same length")
  }
  
  # Multiple counties from the same state can be imported in a single call to the API.
  # This is faster than importing each county individually.
  # The next section ensures multiple copunties are improted at once
  
  # initialize start and end values of counties within the same state
  county_start <- c()
  county_end <- c()
  
  # iterate through each state in the vector of states
  for (single_state in state) {
    
    # if the state is a duplicate, extract county start and end points for state
    county_start <- append(county_start, which(state == single_state)[1])
    county_end <- append(county_end, last(which(state == single_state)))
    
  }
  
  # remove duplicate states
  state <- state[duplicated(state) == FALSE]
  # remove duplicate county start and end points
  county_start <- county_start[duplicated(county_start) == FALSE]
  county_end <- county_end[duplicated(county_end) == FALSE]
  
  
  # create sequence to itereate through
  # if county is null, only iterate once; otherwise number of itereations equals number of states
  #iterates <- if_else(is.null(county), as.integer(1), length(state))
  iterates <- ifelse(is.null(state), 1, length(state))
  
  # iterate through each state and county vector, pulling all tables and years
  
  for (i in seq_len(iterates)) {
    
    # if the geo is the first in a vector, or if only one geo is needed,
    # create the initial dataframe, which subsequent years will be added to
    
    if (i == 1) {
      
      geo_first <- ff_import_years(geography, table_number, state[[i]], county[county_start[i]:county_end[i]],
                                   year_start, year_end, acs_data)
      
      # create dataframes for subsequent geographic areas and merge to first data frame
      
    } else {
      
      geo_next <- ff_import_years(geography, table_number, state[[i]], county[county_start[i]:county_end[i]], 
                                  year_start, year_end, acs_data)
      
      geo_first <- geo_first %>%
        bind_rows(geo_next)
      
    }
    
  }
  
  # calcuate standard error
  # note: since 95% confidence intervals are used, the moe is divided
  # by 1.96, not 1.645 when using the 90% moe's from AFF
  geo_first$se <- geo_first$moe / 1.96
  
  # calculate the cv
  geo_first$cv <- geo_first$se / geo_first$estimate
  
  return(geo_first)
  
}