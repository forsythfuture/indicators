#####################################################################
#
#  This script contains functions to calculate NC and fed tax rates 
#  from PUMS data
#
#####################################################################

library(tidyverse)

# eitc_rate <- read_csv('tax_puma_cal/eitc_clean.csv')
# marginal_rates <- rates_clean(read_csv('tax_puma_cal/fed_rates_raw.csv'))
# std_deduct_table <- read_csv('tax_puma_cal/std_deduction.csv')
# personal_exempt_table <- read_csv('tax_puma_cal/personal_exemption.csv')
# 
# taxable_income <- seq(0, 500000, by = 1000)
# status <- append(rep(c('single', 'married'), times = length(taxable_income)/2), 'single')
# year <- rep(2017, length(taxable_income))
# children <- rep(c(0,1,2,3,4,5), times = length(taxable_income)/5)[1:length(taxable_income)]
# id <- seq(1, length(taxable_income))

a <- total_taxes(id, year, status, children, taxable_income,
                 std_deduct_table, personal_exempt_table,
                 marginal_rates, eitc_rate)

b <- taxes_due_year(id, taxable_income, year, status, marginal_rates)

earned_income <- eitc(id, taxable_income, status, children, year, eitc_rate)

total_taxes <- function(id, year, status, children, taxable_income,
                        std_deduct_table, personal_exempt_table,
                        marginal_rates, eitc_rate) {
  
  # This function calculates total taxes due by implementing functions 
  # that calculate individual taxes and deductions
  
  # check to ensure all statuses are either single, married or hoh
  # stop function and display error message if not
  ifelse(!(unique(status) %in% c('single', 'married', 'hoh')),
         stop('Status must be either: single, married, or hoh.'), 'Taxpayer status good')
  
  # calculate individual taxes and join into single dataframe
  df <- data.frame(id = id,
                   year = year,
                   status = status,
                   children = children,
                   taxable_income = taxable_income) %>%
    # calculate standard deduction and personal exemptions
    left_join(deduction_exemption(id, taxable_income, status, children, year, 
                                  std_deduct_table, personal_exempt_table), by = 'id') %>%
    # subtract standard deduction and personal exemption from taxable income (agi)
    mutate(agi = taxable_income - std_deduction - personal_exemption) %>%
    # if taxable income is negative, convert to 0
    mutate(agi = pmax(agi, 0)) %>%
    # calculate taxes due on age
    left_join(taxes_due_year(id, .$agi, year, status, marginal_rates), by = 'id') %>%
    # calculate child tax credit
    left_join(child_tax_credit(id, .$taxable_income, status, children), by = 'id') %>%
    # subtract child tax credit from tax liability
    mutate(tax_liability = tax_liability - child_credit) %>%
    # child tax credit is not refunded, so negative value is zero
    mutate(tax_liability = pmax(tax_liability, 0)) %>%
    # caclulate EITC
    left_join(eitc(id, .$taxable_income, status, children, year, eitc_rate), by = 'id') %>%
    # subtract EITC from tax liability
    # Eitc is refunded if negative, so keep negative values
    mutate(tax_liability = tax_liability - eitc) %>%
    select(id, tax_liability)
  
}

deduction_exemption <- function(id, taxable_income, status, children, year, 
                                std_deduct_table, personal_exempt_table) {
  
  # This function calculates the standard deduction and personal exemption for taxpayers
  # Input:
  #   taxable_income: vector of taxable incomes
  #   status & year : vector ost atus and year, in same order as taxable income
  #   std_deduct_table: table of standard deductions by year and status, in long form
  #                     must include columns titles 'year', 'status', and 'std_deduction'
  #   personal_exempt_table: table of personal exemptions by year;
  #                     must include columns titles 'year' and 'personal_exemption'
  
  # create dataframe of year, status, and income
  df <- data.frame(id = id,
                   year = year,
                   status = status,
                   children = children,
                   taxable_income = taxable_income) %>%
    # merge table of standard deductions onto income dataframe
    left_join(std_deduct_table, by = c('year', 'status')) %>%
    # merge table of personal exemptions onto income dataframe
    left_join(personal_exempt_table, by = 'year') %>%
    # amount of standard deduction will be the amount in the table, no other calculations needed
    # calculate personal exemption: 
    #     if married value is 2 (one for  each spouse) plus number of children times exemption;
    #     otherwise, value is 1 (for single person) plus number of children time exemption
    mutate(personal_exemption = ifelse(status == 'married', (children + 2) * personal_exemption, 
                                (children + 1) * personal_exemption)) %>%
    select(id, std_deduction, personal_exemption)
  
  return(df)
  
}

child_tax_credit <- function(id, taxable_income, status, children) {
  
  # This function calculates a taxpayer's child tax credit
  # For 2017 and prior, the credit is 1000 for each child,
  # and it phases out at a rate of 0.05 over a given threshhold
  
  married_threshhold <- 110000
  single_threshhold <- 75000 # also hoh threshhold
  
  # merge all values into dataframe
  df <- data.frame(id = id, taxable_income = taxable_income,
                   status = status, children = children)
    
  # if person gets full credit, signify with 0 to show that adjustment is not applicable
  # if threshold drawdown does apply, signify with 1
  # we will then multiply this number by adjustment value to ensure that those who adjustment
  # does not apply get full credit
  df <- df %>%
    mutate(gets_adj = ifelse(status == 'single' & taxable_income < !!single_threshhold, 0,
                        ifelse(status == 'married' & taxable_income < !!married_threshhold, 0, 1))) %>%
    # adjust credit based on income and filing status
    mutate(adj_credit = ifelse(status == 'single',
                               ( (taxable_income-!!single_threshhold)*0.05 ),
                            ifelse(status == 'married',
                                ( (taxable_income-!!married_threshhold) * 0.05), 999999)) * gets_adj) %>%
   # if adjustment is negative then there is no credit because person makes too much money
   # otherwise, credit is number of children times 1000 minus adjustment
   mutate(child_credit = ifelse(adj_credit < 0, 0,
                               (children * 1000) - adj_credit)) %>%
   # if negative, raise to 0
   mutate(child_credit = pmax(child_credit, 0)) %>% 
    select(id, child_credit)
  
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
    
    #print(yr)
  
    # filing status
    for (filing_status in unique(df$status)) {
     # print(filing_status)
      # then loop through number of children (0, 1, 2, and 3)
      # will be no more than three children due to correction above
      for(num_child in seq(0,3)) {
        #print(num_child)
        # filter eitc rate table for year, status, and number of children
        # will result in one row
        eitc_single <- eitc_rate %>%
          filter(year == yr,
                 children == num_child,
                 status == filing_status)
        #print(eitc_single)
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

taxes_due_year <- function(id, taxable_income, year, status, marginal_rates) {

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
  
  # create vector of unique years, to loop through
  unique_years <- unique(year)
  
  # create dataframe of all data
  df <- data.frame(id = id,
                   taxable_income = taxable_income,
                   year = year,
                   status = status)
  
  # initialize dataframe to store all year's results
  df_full <- data.frame()
  
  # calculate taxes for each year of data
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
  
  # only return id and tax liability
  #df_full <- df_full %>% select(id, tax_liability)
  
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
  
  # this function takes in table of rates and cleans / puts in long form
  
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

num_children <- function(df, age_limit) {
  
  # This function calculates the number of persons in a tax unit
  # at or below a given age
  #
  # Input: 
  #       df: dataframe with PUMS data grouped by household and tax unit
  #       age_limit: a vector of age limits
  # Output: vector of number of persons below age limit in household
  #         To be used ats output in mutate function
  
  df %>%
    # replace age number with number signifying whether person is below limit
    # 1 if person is below, 0 otherwise
    mutate(AGEP = ifelse(AGEP <= age_limit, 1, 0)) %>%
    # sum number this column per tax unit
    # result is number of children below age threshhold
    summarize(num_under = sum(AGEP)) %>%
    select(SERIALNO, tax_unit, num_under) %>%
    rename()
}

tax_format <- function(con, year) {
  
  # This function places data in the format needed to calculate taxes with total_taxes
  # con: database connection
  # year: year to pull pums
  
  # establish connection to tables
  
  # create table name by extracting last two digits from year
  # and placing after 'p_'
  yr <-str_extract(as.character(year), '[0-9][0-9]$')
  table_name <- paste0('p_', yr)
  
  # connect to database table
  population <- tbl(con, table_name)
  
  pop_vars <- c('SERIALNO', # serial number grouped by household 
                'SPORDER', # order of person in household
                'ST', # state (needed to calculate state income taxes)
                'PUMA', # Public use micro area
                # relationship to reference person; variable name changed in 2010
                ifelse(year < 2010, 'REL', 'RELP'),
                'AGEP', # age
                'WAGP', # wages or salary income
                'SEMP' # self employment income
  )
  
  # import population data
  pop <- population %>%
    select(!!pop_vars) %>%
    # need to collect now because cannot transform NA without collecting
    filter(ST == 37, PUMA == 1801) %>%
    collect()
  
  # prior to 2010, RELP column is called REL
  # prior to 2009, coding of relationships is different
  # from 2008 and forward, 0-4 are in the same tax unit
  # prior to 2008, 0-2 are in the same tax unit
  if (year < 2010) {
    
    # rename column
    pop <- rename(pop, RELP = REL)
    
    # recode values 
    if (year < 2008) {
      
      # if value is 3 or 4, change to 15
      # the end result is that there will be no 3 or 4 relationship values
      # and similair tax units will be 0-2
      # as a result, we can filter for individual tax units for all years by
      # filtering for 0-4
      pop <- pop %>%
        mutate(RELP = ifelse(RELP %in% c(3, 4), 15, RELP))
      
    }
    
  }
  
  
  pop <- pop %>%
    # replace income NA values with 0
    mutate_at(vars(WAGP, SEMP), funs(replace_na(., 0))) %>%
    mutate(taxable_income = WAGP + SEMP) %>%
    # no longer need subcomponents of income
    select(-WAGP, -SEMP) %>%
    # create tax units
    # relationship statuses of 0, 1, 2, 3 and 4 represent a tax unit (wife / husband and daugher / son)
    # the child in the house must be 22 or younger to qualify as part of the parents' tax unit
    # identiy people part of tax unit with the number 100
    mutate(tax_unit = ifelse(# married persons are in same unit
      RELP %in% c(0,1), 100,
      # children of reference person 19 and under are also part of tax unit
      ifelse(RELP %in% c(2,3,4) & AGEP <= 19, 100, RELP))) %>%
    group_by(SERIALNO, tax_unit) %>%
    # identify number of children below certain ages within tax groups
    # this allows us to calculate exemptions, EITC, and child tax credit
    #
    # tax units get exemptions for dependents for all children 22 and under
    left_join(num_children(., 19), by = c('SERIALNO', 'tax_unit')) %>%
    # EITC qualifying child must be under 19, and limit is three
    # since this is the same age limit as dependent children, we can just use it
    # but we need to cap the number of children at three
    mutate(eitc = ifelse(dep_exemptions > 3, 3, dep_exemptions)) %>%
    # tax units get child tax credit, and amount depends on number of children 16 or younger
    left_join(num_children(., 16), by = c('SERIALNO', 'tax_unit')) %>%
    # calculate tax unit filing status (single or married filing jointly)
    # all married couples are assumed to file jointly
    # in REL column 1 signifies spouse to reference person, so if there is a 1
    # in the tax unit we assume that unit is married filing jointly (number 2)
    # otherwise, unit is single (1)
    mutate(status = ifelse(1 %in% RELP, 2, 1)) %>%
    # since 2 signifies joint and 1 single, we can calculate total exemptions 
    # by adding filing status to number of dependent exemptions
    mutate(exemptions = status + dep_exemptions) %>%
    # only keep needed variables
    select(SERIALNO, SPORDER, tax_unit, ST, RELP, AGEP, taxable_income, SSP, EIC:XTOT) %>%
    ungroup()
  
  # children still in their parent's household often file their own tax returns,
  # but their parents claim them as a dependent; therefore the child does
  # not qualify for an exemption
  #
  # create dataset for children
  
  # children are those whithin household and 22 and under
  child <- pop %>%
    filter(RELP %in% c(2, 3, 4),   # children have status of 2, 3, or 4
           tax_unit == 100, # ensures filer is in household with parents
           taxable_income > 6000 # generally, children don't file taxes if income is under 6000
    ) %>%
    # children cannot take exemptions, and do not qualify for EITC and child tax credit
    # multuply column by 0 to convert to 0
    mutate_at(vars(EIC, n24, XTOT), funs(. * 0)) %>%
    # change column names to meet TAXSIM requirements
    rename(age_head = AGEP, e00200 = taxable_income) %>%
    # create variables for ages and incomes of other people, and set to 0 for most
    mutate(age_spouse = 0,
           e00200p = e00200, # set to unit taxable income
           e00200s = 0, # zero since filing single
           MARS = 1, # children filing single
           # number of children under 13 cannot be less than number of children under 17
           V1 = n24
    ) %>%
    select(-RELP, -tax_unit)
  
  # people in household, but not in the tax unit (not spoue or child) are assumed to file single
  # we will create this dataset and then bind with child and family datasets
  full <- pop %>%
    # people in tax unit 100 are in a family, so those not in this unit are singles
    filter(tax_unit != 100,
           # must have income above 1000, or single filer probably is not filing
           taxable_income > 1000) %>%
    # variable transformations are the same as those for child
    #
    # cannot take exemptions, and do not qualify for EITC and child tax credit
    # multuply column by 0 to convert to 0
    mutate_at(vars(EIC, n24, XTOT), funs(. * 0)) %>%
    # change column names to meet TAXSIM requirements
    rename(age_head = AGEP, e00200 = taxable_income) %>%
    # create variables for ages and incomes of other people, and set to 0 for most
    mutate(age_spouse = 0,
           e00200p = e00200, # set to unit taxable income
           e00200s = 0, # zero since filing single
           MARS = 1, # filing single
           V1 = n24 # dependents under 13 cannot be lower than dependents under 17
    ) %>%
    ungroup() %>%
    select(-RELP, -tax_unit) %>%
    bind_rows(., family, child) %>%
    # rename to convert to TAXSIM names
    #rename(RECID = SERIALNO) %>%
    # add year
    mutate(FLPDYR = year) %>%
    # add columns of zeros for empty columns
    bind_cols(as.data.frame(matrix(data = 0, nrow = nrow(.), ncol = 15))) %>%
    # reorder columns to match required order for online tax system
    select(SERIALNO, FLPDYR, fips, MARS, age_head, age_spouse, XTOT, V1, n24, EIC, e00200p, e00200s,
           V2:V8, SSP, V8:V15) %>%
    # convert NC fips code to its IRS code
    mutate(fips = 34) %>%
    # in 2017, serial IDs have leading '2017'; remove this
    mutate(SERIALNO = as.character(SERIALNO),
           SERIALNO = str_replace_all(SERIALNO, '^2017', ''),
           SERIALNO = as.integer(SERIALNO))
  
  return(full)
  
}
