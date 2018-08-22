#############################################################################
#
# Data calculations
#
# The following functions perform calculations on ACS data,
# such as adding proportions and conducting significance tests.
#
##############################################################################


ff_acs_ratios <- function(df, num_estimate, num_moe, den_estimate, den_moe) {
  
  # This function calculates the ratios and 95% margins of error for the ratios
  # the inputs are column names from the dataframe, entered as strings
  # the formula comes from:
  #    U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-15
  #
  # The output is the same dataframe with the ratio and margin of error added to the dataframe
  
  # calculate the ratio
  ratio  <- df[[num_estimate]] / df[[den_estimate]]
  
  # calculate the numerator for the MOE formula
  moe_num <- sqrt(df[[num_moe]]^2 + (ratio^2 * df[[den_moe]]^2))
  
  # calculate MOE
  moe <- moe_num / df[[den_estimate]]
  
  # add ratio and MOE to dataframe
  df$ratio <- ratio
  df$ratio_moe <- moe
  
  return(df)
  
}