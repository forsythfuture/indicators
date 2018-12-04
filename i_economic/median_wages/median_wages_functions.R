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
  age_bins <- c(0, 21, 29, 44, 64, 150)
  # create labels that are the end age
  age_labels <- age_bins[-1]
  
  df <- df %>%
      # create age bins
      mutate(AGEP = cut(AGEP, breaks = !!age_bins, 
                             labels = !!age_labels, 
                             include.lowest = TRUE),
             # convert to integer
             AGEP = as.integer(as.character(AGEP)),
             # make race 100 for Hispanic if person is of hispanic origin
             RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P)) %>%
    select(-HISP)
    
    # recode race to group races we will not use into one race
    # this will reduce the number of groups and speed up grouping
    
    race_labels <- c(seq(1, 9), 100)
    race_recode <- c(1, 2, rep(4, 7), 3)
    
    df$RAC1P <- plyr::mapvalues(df$RAC1P, race_labels, race_recode)
    
    return(df)
  
}

find_median <- function(df, demo, col_extend, geo_unit) {
  
  # This function extends a column based on replicate weights and 
  # finds the median
  
  median_numbers <- df %>%
    # rename weights column so it is easier ot work with
    # in other parts of the function
    rename('wgt' = col_extend) %>%
    # remove weights of zero
    filter(wgt > 0) %>%
    select_at(c('cntyname', demo, 'wage', 'wgt')) %>%
    uncount(wgt) %>%
    summarize(median_wage = median(wage)) %>%
    ungroup() %>%
    # add cntyname column if geo graphic unit is total, so it matches county format
    # also add ' County, NC' to counties
    mutate(cntyname = if (geo_unit == 'state') 'North Carolina' else paste0(.$cntyname, ' County, NC')) %>%
    rename(geo_area = cntyname) %>%
    select_at(c(demo, 'geo_area', 'median_wage'))
  
  return(median_numbers)
  
}

find_se <- function(median_values) {
  
  # This function takes the list of median values created when
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
    summarize_all(funs(sqrt(sum(.)*(4/80)))) %>%
    # transpose back so that there is one column and each row represents different demographic
    t() %>%
    as.data.frame()
  
  return(sum_sq_diff)

}

inflation_adjust <- function(df, wages_col, se_col, year_adjust, error = TRUE) {
  
  library(xts)
  library(lubridate)
  #########################################################################
  # This function takes as input a dataframe that contains dollar amounts
  # that must be adjusted for inflation and returns the same dataframe, 
  # but with an additional column for inflation ajusted dollar amounts
  #
  # Input:
  #   df: name of dataframe that contains dollar amounts
  #   wages_col: column name of column in dataframe containing dollar amounts
  #              entered as object name (no quotes), not string 
  #              (example: as wages and not "wages")
  #   se_col: column that contains standard error
  #   year_adjust: adjust all dollar amounts to this year
  #   errors: whether dataset contains standard errors that also need to be adjusted
  #
  # Output:
  #   The same dataframe, but with an additional column called 'estimate_adj'
  #
  #   !!!! Important Note: the column that contains years must be called 'year'
  #
  # Reference: US Census Bureau, A Compass for Understanding and Using ACS Data, 
  #             October 2008, A-22 
  #
  #########################################################################
  
  wages_col <- enquo(wages_col)
  
  if (error == TRUE) {
    se_col <- enquo(se_col)
  }
  
  # import CPI All items index data
  monthly_cpi <- read.table("http://research.stlouisfed.org/fred2/data/CPIAUCSL.txt",
                            skip = 53, header = TRUE)
  
  # extract year and place in its own column
  monthly_cpi$year <- year(monthly_cpi$DATE)
  
  # calculate mean CPI for the year
  yearly_cpi <- monthly_cpi %>% 
    group_by(year) %>% 
    summarize(cpi = mean(VALUE))
  
  # calculate inflation rate compared to adjustment year
  yearly_cpi$adj_factor <- yearly_cpi$cpi[yearly_cpi$year == year_adjust]/yearly_cpi$cpi
  
  # combine inflation adjusted wages to wages dataset
  df <- left_join(df, yearly_cpi, by = 'year') %>%
    # adjust income in the given year for inflation since the base year
    # multiply the wage amount in the current year by the adjustment factor
    mutate(estimate_adj = round(!! wages_col * adj_factor, 2))
  # recalculate adjusted se, moe , and cv only if needed
  if (error == TRUE) {
    df <- df %>%
      mutate(se_adj = round(!! se_col / adj_factor, 2)) %>%
      mutate(moe_adj = round(se_adj*1.96, 2),
             cv_adj = round( (se_adj / estimate_adj)*100, 2 ))
  }
  
  df <- df %>%
    select(-cpi, -adj_factor)
  
  return(df)
}