clean_demographics <- function(df) {
  
  # This function cleans the demographics of a PUMS dataset.
  # It does the following:
  #   Bin ages
  #   Recode Hispanics as 100 in RAC1P column
  #   Recode race to 1 = white, 2 = AA, 3 = Hispanic, and 4 = other
  # Data frame must have columns for each of these demographics to work
  
  # create age categories to be used
  age_bins <- c(0, 24, 44, 64, 150)
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