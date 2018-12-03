#########################################################
#
# This script contains functions for running the analysis
#
#########################################################

create_counties <- function(year) {
  
  # since estimated expenses are at the county level, we need to add counties
  counties <- read_csv('puma_counties.csv')
  
  # select PUMA area code based on year
  if (!!year < 2012) {
    
    counties <- counties %>%
      select(puma2k, cntyname) %>%
      rename(PUMA = puma2k)
    
  } else {
    
    counties <- counties %>%
      select(puma12, cntyname) %>%
      rename(PUMA = puma12)
    
  }
  
  # remove duplicate PUMAs
  # this will remove some counties since counties can span more than one PUMA
  counties <- counties %>%
    distinct(PUMA, .keep_all = TRUE) %>%
    # remove ' NC' from county name
    mutate(cntyname = str_replace_all(cntyname, ' NC', ''))
  
  return(counties)
  
}

clean_demographics <- function(df) {
  
  # This function cleans the demographics of a PUMS dataset.
  # It does the following:
  #   Bin ages
  #   Recode Hispanics as 100 in RAC1P column
  #   Recode race to 1 = white, 2 = AA, 3 = Hispanic, and 4 = other
  # Data frame must have columns for each of these demographics to work
  
  # create age categories to be used when calculating income insufficiency rates for age groups
  age_bins <- c(0, 17, 24, 44, 64, 150)
  # create labels that are the end age
  age_labels <- age_bins[-1]
  
  df <- df %>%
      # create age bins
      mutate(AGEP = cut(AGEP, breaks = !!age_bins, 
                             labels = !!age_labels, 
                             include.lowest = TRUE),
             # convert to integer
             AGEP = as.integer(as.character(AGEP)),
             # make race 100 for Hispanic of perso nis of hispanic origin
             RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P)) %>%
    select(-HISP)
    
    # recode race to group races we will not use into one race
    # this will reduce the number of groups and speed up grouping
    
    race_labels <- c(seq(1, 9), 100)
    race_recode <- c(1, 2, rep(4, 7), 3)
    
    df$RAC1P <- plyr::mapvalues(df$RAC1P, race_labels, race_recode)
    
    return(df)
  
}

find_median <- function(df, demo, col_extend) {
  
  # This function extends a column based on replicate weights and 
  # finds the median
  
  median_numbers <- df %>%
    # rename weights column so it is easier ot work with
    # in other parts of the function
    rename('wgt' = col_extend) %>%
    select_at(c('cntyname', demo, 'wage', 'wgt')) %>%
    uncount(wgt) %>%
    summarize(median(wage))
  
  return(median_numbers)
  
}

find_se <- function(median_values) {
  
  # Thisfunction takes the list of median values created when
  # 'find_median' is used for all replciate weights, and 
  # calculates overall standard error
  
  # calcualte squared difference between primary weight
  # and every replicate weight
  sq_diff <- lapply(seq(2, length(median_values)),
                    function(x) (median_values[[1]][3] - median_values[[x]][3])^2)
  
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
    summarize_all(funs(sum(.)*(4/80))) %>%
    # transpose back so that there is one column and each row represents different demographic
    t() %>%
    as.data.frame()
  
  return(sum_sq_diff)

}