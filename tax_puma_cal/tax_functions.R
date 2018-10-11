#####################################################################
#
#  This script contains functions to calculate NC and fed tax rates 
#  from PUMS data
#
#####################################################################

df <- read_csv('i_income_insufficiency/test_income_pums.csv')

library(tidyverse)

taxable_income <- seq(0, 500000, by = 1000)
year <- 2016
status <- rep(c('single', 'married', 'hoh'), times = length(taxable_income)/3)

fed_income_rates <- function(taxable_income, year, status) {
  
  # This program calcuates taxes due based on taxable income
  # Input:
  #   taxable_income: vector of taxable income values
  #   status: character vector of status
  #           either 'single', 'married', or 'hoh' (head of household)

  # create vector of tax buckets and add high value to end for top rate max
  # create different vector of buckets for each tax status

  # change name for filtering
  yr <- year
  
  # read in table of marginal rates for years 2017-2006
  marginal_rates <- read_csv('tax_puma_cal/fed_maginal_rates.csv') %>%
    filter(yr == year) #%>%
    # convert to long form
    #gather('status', 'value', single_max:hoh_fulltax_cum)
    
  # initialize list to store buckets
  bracket_list <- list()
  
  # iterate through each tax status, and create bucket
  for (filing_status in c('single', 'married', 'hoh')) {
    
    # append high value to end of bucket to signify top rate for highest marginal rate
    bracket_list[[filing_status]] <- append( marginal_rates[[paste0(filing_status, '_min')]], 9999999 )
    
  }
  
  # function that calculates the income bracket for individuals
  bracket_finder <- function(income, status) {
    findInterval(income, bracket_list[[status]])
  }

    
  # create single dataframe of year, income, rates, and filing status
  df <- data.frame(year = year, income = taxable_income, status = status) %>%
    # calculate tax bracket
    mutate(bucket = mapply(bracket_finder, taxable_income, status))
  
  # # we currently only need the following columns in the marginal tax table
  marginal_rates <- select(marginal_rates, year, bracket, single_fulltax:hoh_fulltax, bucket) %>%
    # convert to long form for easier merging with dataframe of incomes
    gather('status', 'cumulative_tax', single_min:hoh_fulltax) %>%
    # replace statuses with same terminology in dataframe with incomes
    #mutate(status = str_replace_all(status, '_fulltax', ''))
  
  # convert marginal rates to long form for easier merging of desired attributes
  
  
  # merge the marginal tax rates and cumulative taxes paid into df with incomes
  dfA <- df %>%
    left_join(marginal_rates, by = c('year', 'bucket', 'status'))

  

  df <- data.frame(income = taxable_income, bucket = rates, filing_status = status)
  
  %>%
    # sort lowest to highest, so it matches with rates which are listed in the table from lowest to highest
    sort() %>%
    # convert each taxpayer's bracket to the highest marginal tax rate
    replace(seq_along(.), unique(.), marginal_rates$bracket)[.]
  
  
}