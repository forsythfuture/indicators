#######################################################################################
#
# Data wrangling and cleaning
#
# The following functions wrangle and clean up the acs data.
# For example, they allow users to easily filter data by ethnicity and variable names.
#
#######################################################################################



ff_acs_ethnicity <- function(df, ethnicity_column) {
  
  # This function filters for the following ethnicities:
  #    White Alone, Not Hispanic or Latino; Black or African American Alone; Hispanic or Latino
  # It takes as input a dataframe of ACS data and an object signifying the column name that contains ethnicity information
  # The output is the original dataframe with an additional column called 'ethnicity' added
  
  # keep these ethnicities; these are how the ethnicities are worded in downloaded ACS files
  keep_ethnicities <- c('White alone, not Hispanic or Latino', 'Black or African American',
                        'Hispanic or Latino origin')
  
  # using ACS ethnicites, create regular expression that will search for any of the ethnicities
  # the '|' signifies or
  re_ethnicities <- paste(keep_ethnicities, collapse = '|')
  
  # convert ethnicity column object to eunquo object
  # this allows us to use the object as a column name within dplyr
  ethnicity_column <- enquo(ethnicity_column)
  
  df <- df %>%
    # filter for rows that contain ethnicity information; 
    # !! signifies that the column name is stored in an object, and is not a column in the dataframe 
    filter(str_detect(!! ethnicity_column, re_ethnicities)) %>%
    # create new column that is only the name of the ethnicity
    mutate(ethnicity = str_extract(!! ethnicity_column, re_ethnicities)) %>%
    # convert ethnicity names to Forsyth Futures conventions
    mutate(ethnicity = ifelse(.$ethnicity == 'Black or African American', 'African American',
                              ifelse(.$ethnicity == 'Hispanic or Latino origin', 'Hispanic/Latino',
                                     ifelse(.$ethnicity == 'White alone, not Hispanic or Latino', 'White, non-Hispanic', 'Not sure'))))
  
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
