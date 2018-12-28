find_quint_perc <- function(df, wgt) {
  
  # This finction incorporates other functions
  # and finds the percentage of each demogrpahic falling within quintiles
  # it finds the percentages for one replicate weight
  
  # extend dataframe based on replicate weight
  #wgt <- enquo(wgt)
  
  print(wgt)
  
  # extract unique years so we can create quintiles by iterating through each year
  years <- unique(df$year)
  
  df <- df %>%
    # remove rows with weights of 0 or less
    filter(.[[wgt]] > 0) %>%
    select(!!wgt, year, group, agep, rac1p, hincp) %>%
    # extend dataframe based on weights and convert income to vector
    uncount(eval(parse(text = wgt)))
  
  # find quintiles of replicate weight
  quintiles <- quintile_vector(df)

  # assign each person a quintile
  df <- lapply(years,
         function(x) quintile_cut(df, quintiles, x)) %>%
    bind_rows()

  # for each county and demographic, calculate percentage in each quintile
  county <- df %>%
    # for counties, we only need the comparison counties
    filter(group %in% c('Forsyth', 'Guilford', 'Durham')) %>%
    # for guilford and durham, we only need 2017
    # year equals 2017 or group equals forsyth
    filter(year == 2017 | group == 'Forsyth')
  
  # calculate demographic percentiles
  county <- lapply(c('agep', 'rac1p', 'total'),
                  function(x) find_perc(county, x, geo_unit = 'county')) %>%
    bind_rows(.)

  # calculate percentages in each quintile for state / year / demogrpahic combinations
  state <- lapply(c('agep', 'rac1p', 'total'),
                  function(x) find_perc(df, x, geo_unit = 'state')) %>%
    bind_rows(.)

  # combine state and county estimates
  percentages <- bind_rows(county, state)

  return(percentages)
  
}

quintile_vector <- function(df) {

  # # find quintiles of each year
  quintiles <- df %>%
    group_by(year) %>%
    do(data.frame(t(quantile(.$hincp, probs = seq(0, 1, by = .2), na.rm=TRUE))))

  colnames(quintiles) <- c('year', 'zero', 'one', 'two', 'three', 'four', 'five')

  # the final quintile value is the highest income in the dataset
  # we need to increase this to an impossibly high number
  quintiles$five <- 9999999
  
  return(quintiles)
  
}

quintile_cut <- function(df, quintiles, year) {
  
  # This function takes as input the quintile values
  # and assigns each person to a quintile
  
  # create lables and breaks
  quintile_labels <- c(1, 2, 3, 4, 5)
  quintile_breaks <- as.numeric(quintiles[quintiles$year == year, -1])
  
  # assign each household to an income quintile
  df <- df %>%
    # only keep needed row
    filter(year == !!year) %>%
    mutate(quintile = cut(hincp, breaks = !!quintile_breaks, 
                          labels = !!quintile_labels, 
                          include.lowest = TRUE),
           quintile = as.integer(quintile))
  
  return(df)

}

find_perc <- function(df, demo, geo_unit = 'county') {
  
  # this functions calculates the percentage for each demographic
  # that fall within a quintile
  # input, dataframe of calculations and variable indicating demographic
  # input:
  #   df: dataframe created halfawy thoruhg find_quint_perc
  #   demo: the demographic variable to group on
  #         either enter column name or 'total' to calculate total 
  #         for each year / geographic unit combinations
  #   geo_unit" 'county' or 'state'
  
  # calculate percentiles for geographic area totals (no demographic)
  group_cols <- if (geo_unit == 'county') c('year', 'group', demo, 'quintile') else c('year', demo, 'quintile')
  
  # create a column of ones if we are calculating the total (grouping by total)
  # this will create a group with only the year and geographic unit as grouping variables
  if (demo == 'total') {
    
    df$total <- 1
    
  }
  
  # remove columns with NA values for the demographic
  df <- df[!is.na(df[[demo]]),]
  
  df <- df %>%
    group_by_at(group_cols) %>%
    # calculate total number of rows that area in each demographic / quintile combination
    summarize(n = n()) %>%
    # calculate the percentage of rows that are in each demogrpahic / quintile combination
    # no longer group by quintile (last column)
    group_by_at(group_cols[-length(group_cols)]) %>%
    # divide number in each demographic / quintile combination by number in each demogrpahic
    mutate(perc = n / sum(n)) %>%
    select(-n) %>%
    ungroup() %>%
    # add column specifying major demographic type
    mutate(type = !!demo) %>%
    # rename demogrpahic column, so it has the same name for all columns
    rename(subtype = !!demo)
  
  # if geographic unit is state (not county) changes need to be made to dataframe
  if (geo_unit != 'county') {
    
    # there will be no grouping column for geo graphic unit, 
    # add a column for the geographic unit so state and county have the same columns
    df$group <- 'North Carolina'
    
  }
  
  return(df)
  
}

find_se <- function(quintiles_list) {
  
  # This function takes the list of quintile percentages created when
  # 'find_quint_perc' is used for all replciate weights, and 
  # calculates overall standard error
  
  # calcualte squared difference between primary weight
  # and every replicate weight
  sq_diff <- lapply(seq(2, length(quintiles_list)),
                    function(x) (quintiles_list[[1]][5] - quintiles_list[[x]][5])^2)
  
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