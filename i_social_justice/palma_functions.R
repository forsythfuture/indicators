#######################################################
#
# This file contains functions to calculate the Palma
#
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

house_incomes <- function(con, year, state = NA, area_code = NA) {
  
  # This function returns a dataframe of household incomes for the given year
  # it returns income for the entire US
  # for all states enter: seq(1, 100)
  # for all PUMAs enter seq(1, 10000)
  
  # vector that will return all states when filtered
  all_states <- seq(1, 100)
  # vector to return all PUMAs
  all_pumas <- seq(1, 10000)
  
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
    filter(# state and PUMA filter
           # if state or PUMA is na, use vector containing all states and pumas for filtering
           if (!is.na(!!state)) ST %in% !!state else ST %in% !!all_states,
           if (!is.na(!!area_code)) PUMA %in% !!area_code else PUMA %in% !!all_pumas,
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



palmas_complete <- function(con, year, level, state = NA, area_code = NA) {
  
  # run function to pull in household incomes
  household_incomes <- house_incomes(con, year, state = state, area_code = area_code)
  
  # create groupings that are needed to calculate Palma
  household_incomes <- grpupings(household_incomes, level, year)
  
  # start incorporating replciate weights
  
  # replicate weight variables
  house_weights <- c('WGTP', paste0('wgtp', seq(1, 80))) 
  
  # create connection with housing replicate weights
  weights_tbl <- tbl(con, "h_16") %>%
    filter(#PUMA %in% c(1801, 1802, 1803), # Winston Salem
      ST == 37 # North Carolina (need both puma and state)
    ) %>%
    select(SERIALNO, !!house_weights)
  
  # initialize list to store palma values
  # each item in list will contain dataframe of Palma values for all grouped
  # geographies and one replicate weight
  palmas <- list()
  
  # iterate through each replicate weight
  for (weight in house_weights) {
    
    # bring into memory dataframe with serialno and just one weight column
    wgt <- weights_tbl %>%
      select(SERIALNO, !!weight) %>%
      collect() %>%
      # convert to data table
      as.data.table()
    
    # this provides dataframe for entire US with income, weights, and geography
    # it can then be filtered by geography
    household_incomes_wgt <- wgt[household_incomes, on = 'SERIALNO']
    
    # change name of column so that it is the same of each iteration
    setnames(household_incomes_wgt, weight, 'wgt')
    
    palmas[[weight]] <- household_incomes_wgt[
      # filter out replicate weight values less than 1
      wgt > 0][
        # add additional rows based on the number of replicate weights
        rep(seq(.N), wgt), !"wgt"][
          # order based on income
          order(income, group)][
            # calculate palma for each group
            # use 'get' to convert string to object name
            ,.(palma_cal(income)), by = 'group']
    
  }
  
  # create list of column names for dataframe that contains all Palmas
  col_names <- append('group', house_weights)
  
  # currently, each weight is in a different dataframe
  # combine them all into one dataframe where each column is a replicate weight and each row is a geography
  palmas_full <- reduce(palmas, left_join, by = 'group')
  # rename variables
  colnames(palmas_full) <- col_names
  # transpose so that rows are weights and columsn are geographies
  palmas_t <- as.data.frame(t(palmas_full))
  # the first row is the PUMA numeber; save as object and remove
  pumas <- palmas_t[1,]
  palmas_t <- palmas_t[2:nrow(palmas_t),]
  
  # This sequence does the following:
  #   calculate st error of palmas
  #   convert list of standard errors to dataframe with PUMAs included
  #   bind standard error dataframe to dataframe containing palma values
  #
  # calculate standard error
  palma_complete <- sapply(palmas_t,  function(x) palma_st_error(as.vector(x))) %>%
    # bind with dataframe containing PUMAs
    bind_rows(pumas) %>%
    # transpose so that each row is a PUMA and each column is a SE
    t() %>%
    # convert back to dataframe
    as.data.frame() %>%
    # rename columns
    rename(se = V1, group = V2) %>%
    # bind to dataframe containing PUMA values
    left_join(palmas_full, ., by = 'group') %>%
    # remove replicate weight values
    select(group, WGTP, se) %>%
    rename(palma = WGTP)
  
  return(palma_complete)
  
}

groupings <- function(household_incomes, level, year) {
  
  # this function determines what groupings to use in calculating Palma
  # Input:
  #     household_incomes: dataframe of household incomes created by house_incomes
  #     levels: 'US', 'state', 'county', 'puma'
  
  # convert level to lowercase
  level <- str_to_lower(level)
  
  # replace PUMA codes with county names if true
  if (level %in% c('county', 'counties')) {
    
    # dataframe of all NC puma codes and county names
    # codes are different starting in 2012, so ensure we are pulling in the right year
    if (year > 2011) {
      
      nc_codes <- puma_area_code(letters, 'puma_counties.csv') %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    } else {
      
      nc_codes <- puma_area_code(letters, 'puma_counties.csv', puma12 = FALSE) %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    }
    
    # add county name to income dataframe
    household_incomes <- household_incomes %>%
      left_join(nc_codes, by = 'PUMA') %>%
      rename(group = cntyname)
    
  } else if (level %in% c('united states', 'us')) {
    
    # make integer to conserve ram
    household_incomes$group <- 0
    
  } else if (level %in% c('state', 'states')) {
    
    household_incomes$group <- household_incomes$ST
    
  } else if (level %in% c('puma', 'pums')) {
    
    household_incomes$group <- household_incomes$PUMA
    
  }
  
  return(household_incomes)
  
}