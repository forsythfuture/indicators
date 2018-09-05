########################################################
#
# This file ontains functions to calculate the Palma
#
# The functions are divided into various processes:
#   - calculate palma for one year
#   - calcualte Palma for weights
#   _ calculate the palma for mutliple years
#   - calculate Palma for geographies
#
########################################################

library(tidyverse)
library(data.table)

year <- '14'
data_directory <- 'pums'

palma_single <- function(geography, year, data_directory) {
  
    ### This function returns a list, where each item in the list is a vector if household
    ### incomes based on replicate weights
  

    # Create housing variables
    house_vars <- c('RT', 'TYPE', 'SERIALNO', 'PUMA', 'ST', 'ADJINC', 'HINCP')
    house_weights <- c('WGTP', paste0('wgtp', seq(1, 80))) 
    
    # Create population variables
    pop_vars <- c('SERIALNO', 'AGEP')
    
    # create function to agjust for age based on equivalence scale
    equivalence_scale <- function(num_adults, num_children) {
      
      ifelse(# if num adults is one or two, and no children
        num_adults <= 2 & num_children == 0, num_adults^0.5,
        ifelse(# if single parent
          num_adults == 1 & num_children >= 1,
          (num_adults + 0.8 * 1 + 0.5 * (num_children-1))^0.7,
          # other families
          (num_adults + 0.5 * num_children)^0.7
        )
      )
    }
    
    # initialize list where each element in list will store one weighted vector of incomes
    palma_vec <- c()
    
    # create first part of file name and directory
    house_file <- paste0(data_directory, '/ss', year, 'hus')
    pop_file <- paste0(data_directory, '/ss', year, 'pus')
    
    for (letter in c('a', 'b')) {
      print('starting letter')
      ### import data ###
      housing <- fread(paste0(house_file, letter, '.csv'), select = c(house_vars, house_weights)) %>%
        # filter for housing units and for units with reported household income
        filter(RT == 'H', 
               TYPE == 1, # 1 is housing unit
               !is.na(HINCP), # income is not a missing value
               HINCP >= 0 # income is at least 0
               ) %>%
        # no longer need to check housing unit type
        select(-RT, -TYPE) %>%
        # merge housing with population data
        inner_join(fread(paste0(pop_file, letter, '.csv'), select = pop_vars),
                   by = 'SERIALNO') %>%
        # create categorical variable on whether person is adult or child
        mutate(status = ifelse(.$AGEP >= 18, 'adult', 'child'))
      print('data loaded')
      
      # create data set that shows number of adults in household in one column
      # and number of children in household in other column
      house_num <- housing %>%
        # delete weights
        select(-WGTP:-wgtp80) %>%
        # group by serial number (household)
        group_by(SERIALNO) %>%
        # count number of adults and children in household
        count(status) %>%
        # spread this information so that one column is num. adults and other is num. children
        spread(status, n) %>%
        # replace NA values (which represent 0 adults or children) with 0
        mutate(adult = replace_na(adult, 0),
               child = replace_na(child, 0)) %>%
        # replace NA values (which represent 0 adults or children) with 0
        mutate(adult = replace_na(adult, 0),
               child = replace_na(child, 0)) %>%
        # ungroup to save RAM
        ungroup()
      
      
      # add number of adults and children to primary housing data frame
      housing <- housing %>%
        # drop columns in original data frame that are not needed since the other
        # data frame contains number of adults and children
        select(-AGEP, -status) %>%
        # drop duplicate entries, which gets rid of individuals
        distinct() %>%
        # add data frame that shows number of adults and children
        inner_join(house_num, by = 'SERIALNO') %>%
        # create equivalence scale
        mutate(eq_scale = equivalence_scale(.$adult, .$child)) %>%
        # multiply income by exquivalency scale
        mutate(income = HINCP / eq_scale) %>%
        # drop children and adult columns
        select(-adult, -child, -HINCP, -eq_scale)
      
      # house_num no longer needed since it was joined to house
      rm(house_num)
      gc()
      
      print('starting replicate weights')
      # iterate through each replicate weight, creating vector of household incomes
      for (weight in house_weights) {
        
        # create data frame of incomes and weights
        # data frame is needed because for each replicate weight, values 0 or less are removed
        rep_weights <- data.frame(income = housing$income,
                                  wgt = housing[[weight]]) %>%
          filter(wgt >= 0)
        
        # convert data frame to vector of household incomes
        rep_weights <- rep.int(rep_weights$income, rep_weights$wgt) %>%
          sort()
        
        print('calculate Palma')
        print(weight)
        
        # calculate Palma for vector of household incomes
        palma_vec <- append(palma_vec, palma_cal(rep_weights))
        
        # delete weight variable in primary data frame to save RAM
        housing <- housing %>% select(-weight)
        
        rm(rep_weights)
        gc()

      }
      print('finished dataset')
      rm(housing)
      gc()
    }
    
    print('calculate standard error')
    # calculate standard error based on replicate weights
    st_error <- replicate_weights(palma_vec)
    print('create data frame of results')
    # enter data into single data frame with one row
    # this data frame can be appended to data frames from other years and geographies
    df <- data.frame(geo = geography,
                     year = year,
                     Palma = palma_vec[1],
                     se = st_error)
    
    return(df)
}

palma_cal <- function (income_vec) {
  
  # This function takes as input a vector of replicate weighted household incomes
  # and outputs the Palma
  
  # total number of households; used to find percentiles
  n_income <- length(income_vec)
  
  # total earnings of bottom 40% and top 10%
  bottom <- income_vec[1:floor(n_income*.40)] %>% sum()
  top <- income_vec[floor(n_income*.90):n_income] %>% sum()
  
  return(top / bottom)
  
}

replicate_weights <- function(palma_vec) {
  
  # This function takes as input a vector of Palma values; one value for each weight
  # its output is the standard error of the Palma
  # reference: 
  #   Public Use Microdata Sample (PUMS), Accuracy of the Data (2016), Census Bureau, p. 16
  
  # squared difference
  sq_diff <- (palma_vec - palma_vec[1])^2 %>%
      # sum of squared differences
      sum()
    
  # multiply by 4/80 and take square root
  sq_diff <- sqrt((4/80)*sq_diff)
    
  return(sq_diff)
}

palma_years <- function(geography, years, data_directory) {
  
  # This function creates the Palma for multiple years by using the function
  # that creates the Palma for a single year
  # Note: years must be a vector of the final two digits of the year as characters
  #       example: c('09', '10', '11')
  
  # initialize dataframe to store each year's results
  df <- data.frame(geo = character(),
                   year = character(),
                   Palma = double(),
                   se = double())
  
  # loop through each year and calculate Palma
  for (year in years) {
    palma_df <- palma_single(geography, year, data_directory)
    
    # bind individual year's results to data frame storing all years
    df <- df %>%
      bind_rows(palma_df)
  }
  return(df)
}
  
  
