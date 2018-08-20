library(tidycensus)
library(tidyverse)

# census key must be installed on local computer
# to install census key go to 

census_api_key("1c53569703082594a9d8c422e7bb2d19c9bfba4f", install = TRUE)

# load and save variables
var_names16 <- load_variables(2016, "acs1")

# find all B20017 tables and variables
gender_income_vars <- var_names16 %>% 
  filter(str_detect(.$name, "^B20017")) %>%
  # remove letter at the end of the varible names, since these letters are not at the end of the data table
  mutate(name = str_sub(.$name, 1, -2),
         # for variable decription, replace '!!' with new line '\n'
         label = str_replace_all(.$label, '!!', '\n'),
         # only keep the ethnicity part of the c'concept' variable
         # this part is at the end of the line, and surrounded by brackets
         # we need to keep this part, so gender pay ratios can be isolated by ethnicity
         concept = str_match(.$concept, "[(][A-Z| |,]*?[)]$")) %>%
  # remove brackets - first and last character - from ethnicity
  mutate(concept = str_sub(.$concept, 2, -2)) %>%
  # NA values for ethnicity represent totals, change NA values to 'all'
  mutate(concept = replace_na(.$concept, 'All')) %>%
  # rename 'concept' column
  rename(ethnicity = concept)

# remove variables from table names; this leaves us with the different B20017 tables
# the varible names are after the '_'; therefore use regular expressions to remove characters after '_';
# then store the distinct table names in a vector
gender_income_tables <- str_match(gender_income_vars$name, '(.*)_') %>% # isolte table name
  .[1:nrow(gender_income_vars), 2] %>% # only keep column with table name
  unique() # only keep unique values

# pull data for each table; store date in a list, with each element in the list as a different table

# initialize list to store data
gender_income_list <- list()

for (i in seq_along(gender_income_tables)) {

  # the first dataframe must be created seperately from the others,
  # and then the other dataframes will bs merged into the first one
  
  if (i == 1) {
    
    # create first dataframe
    # others will be merged into this one
    gender_income <- get_acs(geography = "county", 
                             table = gender_income_tables[[i]], year = 2016, survey = 'acs1',
                             state = "NC", county = "Forsyth") %>%
      # add variable descriptions to table
      left_join(gender_income_vars, by = c('variable' = 'name'))
    
  } else {
    
    # create dataframes for other tables, and merge them into the main dataframe
    gender_income_single <- get_acs(geography = "county", 
                                    table = gender_income_tables[[i]], year = 2016, survey = 'acs1',
                                    state = "NC", county = "Forsyth")  %>%
      # add variable descriptions to table
      left_join(gender_income_vars, by = c('variable' = 'name'))
    
    gender_income <- gender_income %>%
      bind_rows(gender_income_single)
    
  }
}

# create vector of the needed ethnicities, so dataframe cab be filtered for these ethnicities
ethnicities <- c("All", "WHITE ALONE, NOT HISPANIC OR LATINO", "HISPANIC OR LATINO", "BLACK OR AFRICAN AMERICAN ALONE")

get_acs(geography = "county", 
        table = gender_income_tables[[i]], year = 2008, survey = 'acs1',
        state = "NC", county = "Forsyth")
