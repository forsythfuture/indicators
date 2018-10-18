#######################################################
#
# This file contains functions to calculate the Palma
#
# The functions are divided into various processes:
#   - calculate palma for one year
#   - calcualte Palma for weights
#   _ calculate the palma for mutliple years
#
########################################################


palma_single <- function(state = NA, area_code = NA, year, data_directory) {
    
    ######## Old !!!!!!!!!!!!!!!!!! ################
  
    ### This function returns a list, where each item in the list is a vector if household
    ### incomes based on replicate weights
  
    # inputs:
    #   state: state FIPS code
    #   puma: vector of PUMA area codes
    #         area codes can be created from counties with the function puma_area_code()
    
    filter_geo <- paste0('ST %in% state & PUMA %in% area_code')

    # Create housing variables
    house_vars <- c('RT', 'TYPE', 'SERIALNO', 'PUMA', 'ST', 'ADJINC', 'HINCP')
    house_weights <- c('WGTP', paste0('wgtp', seq(1, 80))) 
    
    # Create population variables
    pop_vars <- c('SERIALNO', 'AGEP')
    
    # create function to adjust for age based on equivalence scale
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
    
    # initialize vector where each element will store one Palma of weighted vector of incomes
    palma_vec <- c()
    
    # create first part of file name and directory
    house_file <- paste0(data_directory, '/ss', year, 'hus')
    pop_file <- paste0(data_directory, '/ss', year, 'pus')
    
    # initiate list to hold each letter database
    house = list()
    
    for (letter in c('a', 'b')) {
      print(paste0('starting letter: ', letter))
      ### import data ###
      
        if (is.na(state)) {
          print('full data')
          house[[letter]] <- fread(paste0(house_file, letter, '.csv'), select = c(house_vars, house_weights))[
              RT == 'H' & TYPE == 1 & # filter for only housing units
              !is.na(HINCP) & HINCP >= 0][, # filter for positive household incomes
              c("RT","TYPE"):=NULL][ # remove these columns
              # merge with population dataset
              fread(paste0(pop_file, letter, '.csv'), select = pop_vars), 
                    nomatch=0L, on = 'SERIALNO'][, 
              AGEP := ifelse(AGEP >= 18, 'adult', 'child')] # convert age to either adult or child
          
      } else {
        print('filtered data')
        house[[letter]] <- fread(paste0(house_file, letter, '.csv'), select = c(house_vars, house_weights))[
          eval(parse(text = filter_geo)) & # filter for PUMA within state
          RT == 'H' & TYPE == 1 & # filter for only housing units
          !is.na(HINCP) & HINCP >= 0 # filter for positive household incomes
          ][, c("RT","TYPE"):=NULL][ # remove these columns
            # merge with population dataset
            fread(paste0(pop_file, letter, '.csv'), select = pop_vars), 
                  nomatch=0L, on = 'SERIALNO'][, 
            AGEP := ifelse(AGEP >= 18, 'adult', 'child')] # convert age to either adult or child
      }
      
      # if no households were returned based on the geographic filter,
      # move on to next lettered dataset
      if (nrow(house[[letter]]) == 0) {
        house <- house[-length(house)]
        print('deleted empyt list')
        next
      } else {
        print(nrow(house[[letter]]))
      }
      
      print('data loaded')
      
      # find number of adults and children for each family
      # will be merge with the primary dataset
      adults <- house[[letter]][AGEP == 'adult', .N, by = 'SERIALNO'][,
                        .(SERIALNO = SERIALNO, adults = N)] # rename variables
      child <- house[[letter]][AGEP == 'child', .N, by = 'SERIALNO'][,
                       .(SERIALNO = SERIALNO, child = N)] # rename variables
            
      # merge main housing, adults, child
      house[[letter]] <- merge(
        # outer join housing and adults
        merge(house[[letter]], adults, by = 'SERIALNO', all = TRUE),
        # outer join merged housing / adults with child
        child, by = 'SERIALNO', all = TRUE)[, 
        # replace missing values with 0 for number of children and adults
        child := ifelse(is.na(child), 0, child)][,
        adults := ifelse(is.na(adults), 0, adults)][,
        # divide income by equivalency scale
        income := HINCP / equivalence_scale(adults, child)][,
        # remove unneeded columns
        !c('adults', 'child', 'AGEP', 'HINCP', 'ADJINC')] %>%
        # filter out duplicate values
        unique()
      
      rm(adults)
      rm(child)
      gc()
    }
    
    # bind a and b data.tables
    house <- rbindlist(house)
    
      print('starting replicate weights')
      # iterate through each replicate weight, creating vector of household incomes
      for (weight in house_weights) {
        
        print(weight)
        
        # create data frame of incomes and weights
        # data frame is needed because for each replicate weight, values 0 or less are removed
        rep_weights <- data.table(income = house[, income],
                                  wgt = house[, ..weight])
        
        # change name of second column
        setnames(rep_weights, 2, 'wgt')

        rep_weights <- rep_weights[
                                  # filter out replicate weight values less than 1
                                  wgt > 0]

        # convert data frame to vector of household incomes
        rep_weights <- rep.int(rep_weights[, income], rep_weights[, wgt]) %>%
          # sort vector of incomes
          sort() %>%
          # find Palma of vector
          palma_cal()

        # append vector of palmas to appropriate list
        # sort when appending to avoid excess copying
        palma_vec <- append(palma_vec, rep_weights)
                    
        
        # delete weight variable in primary data frame to save RAM
        house <- house[, -..weight]
        
        rm(rep_weights)
        gc()

      }
      
      rm(house)
      gc()
    
    # calculate standard error based on a vector of replicate weights
    st_error <- replicate_weights(palma_vec)

    # enter data into single data frame with one row
    # this data frame can be appended to data frames from other years and geographies
    df <- data.frame(Palma = palma_vec[1],
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

palma_st_error <- function(palma_vec) {
  
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

palma_years <- function(state = NA, area_code = NA, years, data_directory) {
  
  ######## Old !!!!!!!!!!!!!!!!!! ################
  
  # This function creates the Palma for multiple years by using the function
  # that creates the Palma for a single year
  # Note: years must be a vector of the final two digits of the year as characters
  #       example: c('09', '10', '11')
  
  # initialize dataframe to store each year's results
  df <- data.frame(year = character(),
                   Palma = double(),
                   se = double())
  
  # loop through each year and calculate Palma
  for (year in years) {
    
    # print update on year
    print(year)
    
    # extract single year's results and place in dataframe
    palma_df <- palma_single(state = state, area_code = area_code, year, data_directory)
    
    # add year number to single year dataframe
    palma_df$year <- year
    
    # bind individual year's results to data frame storing all years
    df <- df %>%
      bind_rows(palma_df)
    
    rm(palma_df)
    
    # write out results to csv file for each yearly iteration
    write_csv(df, paste0('palma_', year, '.csv'))
    
    gc()
  }
  return(df)
}


equivalence_scale <- function(num_adults, num_children) {
  
  # create function to adjust for age based on equivalence scale
  
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

house_incomes <- function(con, year) {
  
  # This function returns a dataframe of household incomes for the given year
  # it returns income for the entire US
  
  # create table names
  yr <-str_extract(as.character(year), '[0-9][0-9]$')
  house_table <- paste0('h_', yr)
  pop_table <- paste0('p_', yr)
  
  # establish connection to tables
  housing <- tbl(con, house_table)
  population <- tbl(con, pop_table)
  
  # import these PUMS variables
  house_vars <- c('TYPE', 'SERIALNO', 'PUMA', 'ST', 'HINCP')
  pop_vars <- c('SERIALNO', 'AGEP') # population variables
  
  # import population data
  population <- population %>%
    select(!!pop_vars)
  
  
  house <- housing %>%
    select(!!house_vars) %>%
    filter(ST == 37,
           #PUMA %in% c(1801, 1802, 1803),
           TYPE == 1, # housing units only
           (!is.na(HINCP) & HINCP >= 0)) %>% # positive household income
    select(-TYPE) %>%
    # merge with population data
    left_join(population, by = 'SERIALNO') %>%
    # convert age to either adult or femal
    mutate(AGEP = ifelse(AGEP >= 18, 'adult', 'child'))
  
  # find number of adults and children for each family
  # will be merge with the primary dataset
  adults <- house %>%
    filter(AGEP == 'adult') %>%
    group_by(SERIALNO) %>%
    # count number of adults per household
    summarise(number_adults = n())
  
  child <- house %>%
    filter(AGEP == 'child') %>%
    group_by(SERIALNO) %>%
    # count number of children per household
    summarise(number_child = n())
  
  # merge primary dataset and number of adults and children
  house <- house %>%
    left_join(adults, by = 'SERIALNO') %>%
    left_join(child, by = 'SERIALNO') %>%
    collect() %>%
    # replace NA values in numbers of aduls and children to 0
    mutate_at(vars(c('number_adults', 'number_child')), funs(replace_na(., 0))) %>%
    mutate(income = HINCP / equivalence_scale(number_adults, number_child),
           # convert income to integer to save RAM
           income = as.integer(income)) %>%
    select(SERIALNO, PUMA, ST, income) %>%
    # dataframe has one row for each person in the household;
    # but since only household variables were kept, all rows for the same family will be duplicates
    # remove duplicates, which will leave one row per household
    distinct() %>%
    # convert to datatable
    as.data.table()
  
  return(house)
  
}