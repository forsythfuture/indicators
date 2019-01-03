#############################################################################
#
# This script creates a table of demographic information for the voter indicator
#
#############################################################################

library(tidyverse)
library(data.table)
library(DBI)

source('functions/puma_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

# function that calculates the population for a given demographic
population_demo <- function(df, demo_col, geo_col, weight_col, counties) {
  
  # this function calculates populations for all demographics for either counties or the state 
  
  # if the population calculation is for counties, only keep needed counties
  if (geo_col == 'group') {
  df <- df %>%
    filter(group %in% !!counties)
  }
  
  # if demo_col is 'total', we don't want to group by demographic
  if (demo_col == 'total') {
    
    df <- df %>% 
      group_by_at(geo_col) %>%
      summarize_at(vars(!!weight_col), funs(sum)) %>%
      mutate(type = !!demo_col,
             subtype = 9999) %>%
      rename(population = !!weight_col)
    
  } else {
  
    df <- df %>% 
      group_by_at(c(demo_col, geo_col)) %>%
      summarize_at(vars(!!weight_col), funs(sum)) %>%
      mutate(type = !!demo_col) %>%
      rename(population = !!weight_col,
             subtype = !!demo_col)
  
  }
    
  return(df)
}

# function that iterates through ech demographic, calculating population
population_all_demo <- function(df, demo_cols, geo_col, weight_col, counties) {
  
  # initialize dataframe to store all demographic populations
  demo_pop <- data.frame()
  
  for (demo in demo_cols) {
    
    demo_pop <-  population_demo(df, demo, geo_col, weight_col, counties) %>%
      bind_rows(demo_pop, .)
      
  }
  
  return(demo_pop)
  
}

# function that iterates through counties and whole state, and
# calculates populations for all demographics
population_geo <- function(df, demo_cols, geo_cols, weight_col, counties) {
  
  # initialize dataframe to store all demographic populations
  geo_pop <- data.frame()
  
  for (geo in geo_cols) {
    
    geo_pop <-  population_all_demo(df, demo_cols, geo, weight_col, counties) %>%
      bind_rows(geo_pop, .)
    
  }
  
  geo_pop <- geo_pop %>%
    mutate(group = ifelse(is.na(group), 'North Carolina', paste0(group, ' County, NC'))) %>%
    select(-ST)

  return(geo_pop)
  
}

find_se <- function(pop_values) {
  
  # This function takes the list of population values created when
  # 'population_geo' is used for all replciate weights, and 
  # calculates overall standard error
  
  # calcualte squared difference between primary weight
  # and every replicate weight
  sq_diff <- lapply(seq(2, length(pop_values)),
                    function(x) (pop_values[[1]][2] - pop_values[[x]][2])^2)
  
  # create dataframe to store all squared difference weight,
  # needed all of them in one dataframe so we can sum
  sum_sq_diff <- data.frame(a = rep(100, nrow(sq_diff[[1]])))
  
  # iterate through squared differences list, adding as columns to dataframe
  for (i in seq_along(sq_diff)) {
    
    sum_sq_diff[[i]] = sq_diff[[i]]
    
  }
  
  # sum the difference and multiply by 4/80
  # but first, transpose so that each column is all relicate weights
  # of a given demograhic; this will make it easier to sum all replciate weights
  sum_sq_diff <- sum_sq_diff %>%
    t() %>%
    as.data.frame() %>%
    summarize_all(funs(sqrt(sum(.)*(4/80)))) %>%
    # transpose back so that there is one column and each row represents different demographic
    t() %>%
    as.data.frame()
  
  return(sum_sq_diff)
  
}

# population variables that are needed
pop_vars <- c('ST', # state
              'PUMA', # PUMA code
              'SEX', # gender
              'AGEP', # age
              'RAC1P', # race
              'HISP',  # hispanic origin
              'CIT' # citizenship status
              )

comparisons <- c('Forsyth', 'Durham', 'Guilford')

pop_demo_master <- data.frame()

years <- c(seq(2006, 2016, 2), 2017)

for (yr in years) {
  
  print(yr)
  
  weight_names <- ifelse(yr >= 2017, 'PWGTP', 'pwgtp')
  replicate_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))
  
  # add the year's replicate weights to housing variables
  pop_yr_vars <- c(pop_vars, replicate_weights)
    
  # import and clean population variables
  pop <- tbl(con, table_name('population', yr)) %>%
    # only keep needed variables
    select(!!pop_yr_vars) %>%
    filter(ST == 37, # only keep NC, which is state number 37
           CIT != 5, # only keep citizens (non-citizens are labeled 5)
           AGEP >= 18 # only keep people over 18
    ) %>%
    collect() %>%
    # add county names
    groupings('county', yr) %>%
    select(-PUMA, -CIT) %>%
    # recode race and create age bins
    clean_demographics(., c(0, 25, 29, 49, 64, 84, 150))
  
  # find populations for demographics and each replicate weight
  pop_demo <- lapply(replicate_weights, 
                     function(x) population_geo(pop, c('total', 'RAC1P', 'SEX', 'AGEP'), c('group', 'ST'), x, comparisons))
  # find standard errors
  pop_se <- find_se(pop_demo)
  
  # add standard errors to dataset containing population values
  # first dataframe in list contains population values from primary weight
  pop_demo <- pop_demo[[1]] %>%
    ungroup() %>%
    mutate(se = round( pop_se[[1]], 0),
           year = yr)
  
  #colnames(pop_demo) <- c('subtype', 'geo_area', 'estimate', 'type', 'se', 'year')
  
  pop_demo_master <- bind_rows(pop_demo_master, pop_demo)
  
}

# relabel 2017 as 2018
# we have 2018 voter data, but not 2018 population data;
# so we are using 2017 population data for 2018
pop_demo_master$year <- ifelse(pop_demo_master$year == 2017, 2018, pop_demo_master$year)


write_csv(pop_demo_master, 'i_civic_engagement/electoral participation/elizabeth_analysis/voter_demographics.csv')