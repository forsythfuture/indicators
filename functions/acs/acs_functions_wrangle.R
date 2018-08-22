#######################################################################################
#
# Data wrangling and cleaning
#
# The following functions wrangle and clean up the acs data.
# For example, they allow users to easily filter data by ethnicity and variable names.
#
#######################################################################################


ff_acs_ethnicity <- function(df) {
  
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



ff_acs_keep_vars <- function(df, variables) {
  
  # this program filters for specific variables
  # the variables are three digit numbers that are shown as
  # the last three digits in the 'variables' column
  
  # Input includes the dataframe of ACS data and the variables that are needed
  # variables are entered as a three digit string (ex: '001')
  # important: variables must be netered as strings
  
  # check vector of variables and ensure it is a character vector
  # if not, through error message
  if (!is.character(variables))
    stop("The vector of variable names must be characters.To create characters, surround numbers in ''.")
  
  df_vars <- df %>%
    # extract last three digits, which are the variables, and place in new column
    mutate(variable_code = str_extract(.$variable, "[0-9]{3}$")) %>%
    # filter this column for the specific variables
    filter(variable_code %in% variables) %>%
    # remove column that is only the variable cod
    select(-variable_code)
  
  return(df_vars)
}
