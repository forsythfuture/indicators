#####################################################################
#
#  This script contains functions to calculate NC and fed tax rates 
#  from PUMS data
#
#####################################################################

library(tidyverse)

taxable_income <- seq(5000, 130000, by = 1000)
status <- rep(c('single', 'married'), times = length(taxable_income)/2)
year <- rep(2015, length(taxable_income))
children <- rep(c(0,1,2,3,4,5), times = length(taxable_income)/5)[1:length(taxable_income)]
id <- seq(1, length(taxable_income))

a <- child_tax_credit(id, taxable_income, status, children)

child_tax_credit <- function(id, taxable_income, status, children) {
  
  # This function calculates a taxpayer's child tax credit
  # For 2017 and prior, the credit is 1000 for each child,
  # and it phases out at a rate of 0.05 over a given threshhold
  
  married_threshhold <- 110000
  single_threshhold <- 75000 # also hoh threshhold

  # check to ensure all statuses are either single, married or hoh
  # stop function and display error message if not
  ifelse(!(unique(status) %in% c('single', 'married', 'hoh')),
         stop('Status must be either: single, married, or hoh.'), 'Taxpayer status good')
  
  # merge all values into dataframe
  df <- data.frame(id = id, taxable_income = taxable_income,
                   status = status, children = children)
  
  # limits where credit phases out
  single_hoh_phaseout <- 75000
  married_phaseout <- 110000
  
  # calculate credit
  df$credit <- children * 1000
    
  # if person gets full credit, signify with 0 to show that adjustment is not applicable
  # if threshold drawdown does apply, signify with 1
  # we will then multiply this number by adjustment value to ensure that those who adjustment
  # does not apply get full credit
  df <- df %>%
    mutate(gets_adj = ifelse(status == 'single' & taxable_income < !!single_hoh_phaseout, 0,
                        ifelse(status == 'married' & taxable_income < !!married_phaseout, 0, 1))) %>%
    # adjust credit based on income and filing status
    mutate(adj_credit = ifelse(status == 'single',
                               df$credit - ( (taxable_income-!!single_hoh_phaseout)*0.05 ),
                            ifelse(status == 'married',
                                   credit - ( (taxable_income-!!married_phaseout) * 0.05), 999999)) * gets_adj) %>%
   # if adjustment is negative then there is no credit because person makes too much money
   # otherwise, credit is number of children times 1000 minus adjustment
   mutate(credit_amount = ifelse(adj_credit < 0, 0,
                                 credit - adj_credit))
  
  return(df)
  
}



eitc <- function(id, taxable_income, status, children, year, eitc_rate) {
  
  # This function calculates a taxpayer's earned income tax credit
  # Input:
  #   id: unique record identifier to keep track of rows
  #   taxable_income: vector of taxable incomes
  #   status: vector of taxpaying statuses (married or single)
  #   children: vector of number of children
  #   year: vector of years
  #   eitc_rate: table of etic rates in long-form
  
  # check to ensure all statuses are either single, married or hoh
  # stop function and display error message if not
  ifelse(!(unique(status) %in% c('single', 'married', 'hoh')),
         stop('Status must be either: single, married, or hoh.'), 'Taxpayer status good')
  
  # merge all values into dataframe
  df <- data.frame(id = id, taxable_income = taxable_income,
                   status = status, children = children, year = year)
  
  # three is the max children, so convert any # of children values above three to three
  df$children <- ifelse(df$children > 3, 3, df$children)
  
  # calculate eitc by looping through year, filing status and number of children
  
  # initialize df to store all results
  eitc_full <- data.frame()
    
  # start with year
  for(yr in unique(df$year)) {
  
    # filing status
    for (filing_status in unique(df$status)) {
      
      # then loop through number of children (0, 1, 2, and 3)
      # will be no more than three children due to correction above
      for(num_child in seq(0,3)) {
        
        # filter eitc rate table for year, status, and number of children
        # will result in one row
        eitc_single <- eitc_rate %>%
          filter(year == yr,
                 children == num_child,
                 status == filing_status)
        
        # create separate objects for eitc items
        phase_in_ends <- eitc_single$phase_in_ends[[1]]
        phase_in_rate <- eitc_single$phase_in_rate[[1]]
        phase_out_begins <- eitc_single$phase_out_begins[[1]]
        max_credit <- eitc_single$max_credit[[1]]
        phase_out_ends <- eitc_single$phase_out_ends[[1]]
        phase_out_rate <- eitc_single$phase_out_rate[[1]]
        
        df_status <- df %>%
          filter(year == yr,
                 status == filing_status,
                 children == num_child) %>%
          # calcualte eitc
          mutate(eitc = ifelse(
                  # if income is below when the phase out ends, eitc is income times phase in rate
                  .$taxable_income <= phase_in_ends, taxable_income * phase_in_rate,
                  # otherwise, if income is between where the phase in ends, and phase out begins,
                  # eitc is the set max credit
                  ifelse(
                    .$taxable_income <= phase_out_begins, max_credit,
                    # otherwise, if income less than where the phase out ends,
                    # eitc is the max credit minus the dollar amount after when the phase out begins 
                    # times phase out rate
                    ifelse(
                      .$taxable_income <= phase_out_ends,
                          max_credit-((.$taxable_income-phase_out_begins)*phase_out_rate),
                      # if income is after where the phase out ends you get nothing
                      0)))) %>%
          select(id, eitc)
        
        # bind to primary dataframe
        eitc_full <- bind_rows(eitc_full, df_status)
      }
    }
  }
  
  return(eitc_full)
  
}

taxes_due_year <- function(id, taxable_income, year, status, marginal_rates_file) {

  # This program calcuates taxes due based on taxable income
  # It uses the function 'taxes_due' to calculate taxes
  #
  # Input:
  #   id: record ID so output can be matched with row in dataframe
  #       that was used to create output
  #   taxable_income: vector of taxable income values
  #   years: vector of years
  #   status: character vector of status
  #           either 'single', 'married', or 'hoh' (head of household)
  #   marginal_rates_file: file name to table of rates

  # check to ensure all statuses are either single, married or hoh
  # stop function and display error message if not
  ifelse(!(unique(status) %in% c('single', 'married', 'hoh')),
    stop('Status must be either: single, married, or hoh.'), 'Taxpayer status good')
    
  # import and clean marginal rates table
  marginal_rates <- read_csv(marginal_rates_file) %>%
    rates_clean()
  
  # create vector of unique years, to loop through
  unique_years <- unique(year)
  
  # create dataframe of all data
  df <- data.frame(id = id,
                   taxable_income = taxable_income,
                   year = year,
                   status = status)
  
  # initialize dataframe to store all year's results
  df_full <- data.frame()
  
  # calcualte taxes for each year of data
  for(yr in unique_years) {
    
    df_year <- df %>%
      # only keep needed years
      filter(year == yr)
      
    # calculate taxes on year
    df_year <- taxes_due(id = df_year$id, taxable_income = df_year$taxable_income, 
                         year = yr, status = as.character(df_year$status), 
                         marginal_rates = marginal_rates)
    
    # add yearly to full dataset
    df_full <- bind_rows(df_full, df_year)
    
  }
  
  return(df_full)
  
}


taxes_due <- function(id, taxable_income, year, status, marginal_rates) {
  
  # This program calcuates taxes due based on taxable income
  # Input:
  #   id: record ID so output can be matched with row in dataframe
  #       that was used to create output
  #   taxable_income: vector of taxable income values
  #   year: year to calculate for taxes
  #   status: character vector of status
  #           either 'single', 'married', or 'hoh' (head of household)
  #   marginal_rates: table of rates, cleaned with 'rates_clean'

  # create vector of tax buckets and add high value to end for top rate max
  # create different vector of buckets for each tax status

  # change name for filtering
  yr <- year
  
  marginal_rates <- filter(marginal_rates, year == yr)
    
  # initialize list to store buckets
  bracket_list <- list()
  
  # iterate through each tax status, and create bucket
  for (filing_status in c('single', 'married', 'hoh')) {
    
    bracket_append <- marginal_rates %>%
      # filter for status
      filter(status == filing_status) %>%
      # only select column with the minimum marginal bucket
      select(min_bucket) %>%
      # convert to vector
      .[[1]]
    
    # append high value to end of bucket to signify top rate for highest marginal rate
    bracket_list[[filing_status]] <- append( bracket_append, 9999999 )
    
  }
  
  # function that calculates the income bracket for individuals
  bracket_finder <- function(income, status) {
    
    intervals <- bracket_list[[status]]
    
    findInterval(income, intervals )
  }
  
  # calculate income brackets for each income value
  brackets_income <- unlist(
                        Map(bracket_finder, 
                          income = taxable_income, 
                          status = status))
    
  # create single dataframe of year, income, bucket, and filing status
  df <- data.frame(year = year, income = taxable_income, 
                   status = as.character(status), bucket = brackets_income) %>%
    # to calculate total taxes, we need to add the following to our dataframe of incomes:
    #         tax rate, cumulative taxes up to bucket, minimum income for bucket
    left_join(marginal_rates[c('year', 'status', 'bucket', 'bracket', 'min_bucket', 'cumulative')], 
              by = c('year', 'status', 'bucket')) %>%
    # now, we can calcualte taxes by adding the cumualtive tax to the rate plus the difference between
    # income and the lower end of the bracket
    mutate(tax_liability = ((income - min_bucket) * bracket ) + cumulative) %>%
    # only return taxes
    select(tax_liability) %>%
    # add id
    mutate(id = id)
  
  return(df)
  
}

rates_clean <- function(table_rates) {
  
  # this function takes ina table of rates and cleans / puts in long form
  
  table_rates %>%
    # convert to wide where each status is a different row
    gather('status', 'min_bucket', single_min:hoh_min) %>%
    # change status description
    mutate(status = str_replace_all(status, '_min', '')) %>%
    group_by(year, status) %>%
    # calculate the high end of the bucket
    # subtract 1 so the buckets edges are not the same
    mutate(max_bucket = lead(min_bucket, 1, 9999999),
           # calculate bucekt's span
           bucket_span = max_bucket - min_bucket,
           # calculate taxes within the bucket
           bucket_tax = bucket_span * bracket,
           # calculate agggregate taxes up to the bucket
           cumulative = cumsum(bucket_tax),
           # need to move cumulative taxes down one bucket so they match
           cumulative = lag(cumulative, 1, 0)) %>%
    ungroup()
}