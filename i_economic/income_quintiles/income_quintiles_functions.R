quintile_vector <- function(df, wgt) {
  
  # This function finds the income quintiles of PUMs data
  
  # extend dataframe based on weights and convert income to vector
  incomes <- df %>%
    select(!!wgt, HINCP) %>%
    uncount(!!wgt) %>%
    as.vector()
  
  # calculate quartiles of incomes
  quintiles <- quantile(incomes$HINCP, probs = seq(0, 1, by = .2), na.rm=TRUE) %>%
    as.data.frame(.) %>% 
    .[[1]] %>% 
    as.vector(.)
  
  # the final quintile value is the highest income in the dataset
  # we need to increase this to an impossibly high number
  quintiles[length(quintiles)] <- 99999999
  
  return(quintiles)
  
}

quintile_cut <- function(df, quintiles) {
  
  # This function takes as input the quintile values
  # and assigns each person to a quintile
  
  quintile_labels <- c(1, 2, 3, 4, 5)
  
  # assign each household to an income quintile
  df <- df %>%
    mutate(quintile = cut(HINCP, breaks = !!quintiles, 
                          labels = !!quintile_labels, 
                          include.lowest = TRUE))
  
  return(df)

}
