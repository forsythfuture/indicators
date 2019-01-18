#############################################################
#
#  This script contains functions for family structure
#
############################################################

find_counts <- function(df, wgt) {
  
  # This functio nfinds the coutns and percentages for each family structure.
  # It is used to find the counts and percentages for s single replicate weight
  print(wgt)
  
  ##################################
  # extend dataframe based on replicate weights
  df <- df %>%
    # remove rows with weights of 0 or less
    filter(.[[wgt]] > 0) %>%
    select(!!wgt, YEAR, group, RAC1P, FES, FPARC) %>%
    # extend dataframe based on weights and convert income to vector
    uncount(eval(parse(text = wgt)))
  
  ###################################
  # create dataset that is the total count of each race, and total count overall
  # this will be used to calculate percentage of families within each category
  
  # recode of races
  race_recode <- c(`1` = 'White, non-Hispanic',
                   `2` = 'African American',
                   `3` = 'Hispanic/Latino',
                   `4` = 'Total')
  
  # create dataset for counts by demographic
  total_counts <- df %>%
    group_by(RAC1P, group, YEAR) %>%
    summarize(total_count = n()) %>%
    # remove other races
    filter(RAC1P != 4)
  
  # create dataset for counts by total
  # and add to dataset for counts by race
  total_counts <- df %>%
    group_by(group, YEAR) %>%
    summarize(total_count = n()) %>%
    # create column for race, needed so we can bind this dataset with the dataset that includes race
    mutate(RAC1P = 4) %>%
    bind_rows(total_counts) %>%
    # recode races
    mutate(RAC1P = recode(RAC1P, !!!race_recode))
  
  ####################################
  # create counts for each family structure and race
  
  # create dataset for family counts by race
  family_counts <- df %>%
    group_by(group, YEAR, FES, FPARC, RAC1P) %>%
    summarize(family_count = n()) %>%
    # remove other races
    filter(RAC1P != 4)
  
  # create datasets for total family counts
  # and add to dataset for counts by race
  family_counts <- df %>%
    group_by(group, YEAR, FES, FPARC) %>%
    summarize(family_count = n()) %>%
    # create column for race, needed so we can bind this dataset with the dataset that includes race
    mutate(RAC1P = 4) %>%
    bind_rows(family_counts) %>%
    # need to ungroup so the grouping variables can be modified
    ungroup() %>%
    # recode races, family structures, and children
    mutate(RAC1P = recode(RAC1P, !!!race_recode),
           FES = recode(FES, 
                        `1` = 'Married-couple',
                        `2` = 'Male householder',
                        `3` = 'Female householder'),
           FPARC = recode(FPARC,
                          `1` = 'With related children under 18',
                          `2` = 'No related children under 18'))
  
  family_counts <- family_counts %>%
    # merge total with family to create percentages
    left_join(total_counts, c('group', 'YEAR', 'RAC1P')) %>%
    # calcualte percentages that represent the count of each family structure type
    # divided by the total count of the race
    mutate(percentage = family_count / total_count)
  
  return(family_counts)
  
}

counts_list <- function(df) {
  
  # this function uses find_counts to find the counts of all weights
  # and places them into a single list
  
  # extract the column names of all weights
  weight_names <- colnames(demographics) %>% 
    str_extract_all('WGTP.*') %>% 
    unlist()
  
  counts <- lapply(weight_names, function(x) find_counts(df, x))
  
}

find_se <- function(list_of_counts, col_num) {
  
  # This function takes the list of quintile percentages created when
  # 'find_quint_perc' is used for all replciate weights, and 
  # calculates overall standard error
  
  # col_num: the column number for the variable you want to calculate the replicate weight for
  
  # calcualte squared difference between primary weight
  # and every replicate weight for counts
  sq_diff <- lapply(seq(2, length(list_of_counts)),
                    function(x) (list_of_counts[[1]][col_num] - list_of_counts[[x]][col_num])^2)
  
  # create dataframe to store all squared difference weight,
  # need all of them in one dataframe so we can sum
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
