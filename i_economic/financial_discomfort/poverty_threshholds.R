# This script creates a dataset that includes poverty threshholds for all years
# each row is a different year / characteristic combination

library(tidyverse)

# list of threshhold csv files (one for each year)
thresh_files <- list.files('i_economic/financial_discomfort/threshhold_data/', full.names = TRUE)

# size and number of children columns have the numbers as strings
# create lookup table to convert to number
level_key <- list(weight = '0', none = '0', one = '1', two = '2', three = '3', four = '4',
                  five = '5', six = '6', seven = '7', eight = '8', nine = '9')

# initialize dataset to store all years of poverty thresholds
poverty_thresholds <- data.frame()

# itereate through each yearly file, importing, cleaning, and adding to master dataset
for (i in seq_along(thresh_files)) {
  
  # find out year from file name
  thresh_year <- str_match(thresh_files[i], '([0-9][0-9]).csv')[2] %>%
    as.integer() %>%
    sum(2000)
  
  print(thresh_year)
  
  # import each yearly poverty threshhold csv file and add year
  poverty_thresholds <- read_csv(thresh_files[i]) %>%
    # relabel column descriptions
    # for the ones we don't need, call them 'delete'
    # we will jsut use the weighted average for one and two person, so don't need ages
    mutate(size = c('one', 'delete', 'delete', 
                    'two', 'delete', 'delete',
                    'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'),
           year = thresh_year) %>%
    # these two rows are only needed fi you don't know age
    # since we will know age, we do not need
    filter(size != 'delete') %>%
    # don't need weighted column since it is only needed 
    #select(-weighted) %>%
    # convert to wide form
    gather(num_children, poverty_thresh, -size, -year) %>%
    # delete rows with NA values for threshhold
    # these will be rows with more children than household members
    filter(complete.cases(.)) %>%
    # we only need weighted columns for one and two person households
    # remove all other weighted columns
    filter(!(!(size %in%c('one', 'two')) & num_children == 'weighted')) %>%
    # make number of children column lower case so that we can replace strings with
    # numbers for size and number of children with same command
    mutate(num_children = tolower(num_children)) %>%
    # convert strings of numbers to numbers
    mutate_at(vars(size, num_children), 
              funs(as.integer(recode(., !!!level_key)))) %>%
    bind_rows(poverty_thresholds, .)
  
}

# converty poverty amount to 250% of poverty
poverty_thresholds$poverty_thresh <- poverty_thresholds$poverty_thresh * 2.5

# write out file of pvoerty thresholds
#write_csv(poverty_thresholds, 'i_economic/financial_discomfort/thresholds.csv')
