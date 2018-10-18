###############################################################################
#
#   This script contains functions that creates datasets that can be run 
#   through the taxsim cli program
#
###############################################################################

library(tidyverse)
library(DBI)

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
    #group_by(SERIALNO, tax_unit) %>%
    # sum number this column per tax unit
    # result is number of children below age threshhold
    summarize(num_under = sum(AGEP)) %>%
    select(SERIALNO, tax_unit, num_under) %>%
    rename()
}

pop_taxes <- function(con, year) {
  
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
                'SEMP', # self employment income
                'SSP' # social security income
                )
  
  # import population data
  pop <- population %>%
    select(!!pop_vars) %>%
    # need to collect now because cannot transform NA without collecting
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
    mutate_at(vars(WAGP, SEMP, SSP), funs(replace_na(., 0))) %>%
    mutate(taxable_income = WAGP + SEMP) %>%
    # create tax units
    # relationship statuses of 0, 1, 2, 3 and 4 represent a tax unit (wife / husband and daugher / son)
    # the child in the house must be 22 or younger to qualify as part of the parents' tax unit
    # identiy people part of tax unit with the number 100
    mutate(tax_unit = ifelse(# married persons are in same unit
                             RELP %in% c(0,1), 100,
                             # children of reference person 22 and under are also part of tax unit
                             ifelse(RELP %in% c(2,3,4) & AGEP <= 22, 100, RELP))) %>%
    group_by(SERIALNO, tax_unit) %>%
    # identify number of children below certain ages within tax groups
    # this allows us to calculate exemptions, EITC, and child tax credit
    #
    # tax units get exemptions for dependents for all children 22 and under
    left_join(num_children(., 22), by = c('SERIALNO', 'tax_unit')) %>%
    rename(dep_exemptions = num_under,
           fips = ST # rename states to TAXSIM requirement while we are renaming things
           ) %>%
    # EITC qualifying child must be under 22, and limit is three
    # since this is the same age limit as dependent children, we can just use it
    # but we need to cap the number of children at three
    mutate(EIC = ifelse(dep_exemptions > 3, 3, dep_exemptions)) %>%
    # tax units get child tax credit, and amount depends on number of children 16 or younger
    left_join(num_children(., 16), by = c('SERIALNO', 'tax_unit')) %>%
    rename(n24 = num_under) %>%
    # calculate tax unit filing status (single or married filing jointly)
    # all married couples are assumed to file jointly
    # in REL column 1 signifies spouse to reference person, so if there is a 1
    # in the tax unit we assume that unit is married filing jointly (number 2 in taxsim)
    # otherwise, unit is single (1 in taxsim)
    mutate(MARS = ifelse(1 %in% RELP, 2, 1)) %>%
    # since 2 signifies joint and 1 single, we can calculate total exemptions 
    # by adding filing status to number of dependent exemptions
    mutate(XTOT = MARS + dep_exemptions) %>%
    # only keep needed variables
    select(SERIALNO, tax_unit, fips, RELP, AGEP, taxable_income, SSP, EIC:XTOT)

  
  # To run the data through TAXSIM, each spouse's income and age must
  # be on separate columns
  
  # Create two datasets, one for reference person and one for spouse
  # then merge the datasets
  
  # column needed for dataset with spouse
  cols_spouse <- c('SERIALNO', 'tax_unit', 'AGEP', 'SSP', 'taxable_income')
  
  # data set for reference person (first spouse)
  ref <- pop %>%
    filter(RELP == 0) %>%
    # rename reference person variables to match TAXSIM
    rename(age_head = AGEP, e00200p = taxable_income)
  
  # merge with dataset for second spouse
  family <- pop %>%
    # RELP of 1 signifies the spouse
    filter(RELP == 1) %>%
    select(!!cols_spouse) %>%
    rename(age_spouse = AGEP, e00200s = taxable_income) %>%
    left_join(ref, ., by = c('SERIALNO', 'tax_unit')) %>%
    select(-RELP)  %>%
    # NA values for spouse information (age, income) represents not having a spouse
    # these can be change to zero
    mutate_at(vars(age_spouse:e00200s), funs(replace_na(., 0))) %>%
    # compute filing unit income
    mutate(e00200 = e00200p + e00200s,
           # social security (SSP) is entered at the filing unit level
           # sum each spouse's social security
           SSP = SSP.x + SSP.y) %>%
    ungroup() %>%
    select(-SSP.x, -SSP.y, -tax_unit)
  
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
           MARS = 1 # children filing single
           ) %>%
    # the child ID numbers must be separate from the parents for TAXSIM
    # but, we need to link them back up after running the TAXSIM program
    # we can do this by adding three zeros to the end of the ID, 
    # then removing them after running the program
    # but, must ungroup because we are transforming grouping variable (SERIALNO)
    ungroup() %>%
    mutate(SERIALNO = SERIALNO * 1000) %>%
    select(-RELP, -tax_unit)
  
  # people in household, but not in the tax unit (not spoue or child) are assumed to file single
  # we will create this dataset and then bind with child and family datasets
  full <- pop %>%
    # people in tax unit 100 are in a family, so those not in this unit are singles
    filter(tax_unit != 100) %>%
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
           MARS = 1 # filing single
    ) %>%
    # the ID numbers must be separate from the household for TAXSIM
    # but, we need to link them back up after running the TAXSIM program
    # we can do this by adding six zeroes to the end of the ID, 
    # then removing them after running the program
    # but, must ungroup because we are transforming grouping variable (SERIALNO)
    ungroup() %>%
    mutate(SERIALNO = SERIALNO * 100000) %>%
    select(-RELP, -tax_unit) %>%
    bind_rows(., family, child) %>%
    # rename to convert to TAXSIM names
    rename(RECID = SERIALNO) %>%
    # add year
    mutate(FLPDYR = year) %>%
    # add columns of zeros for empty columns
    bind_cols(as.data.frame(matrix(data = 0, nrow = nrow(.), ncol = 15))) %>%
    # reorder columns to match required order for online tax system
    select(RECID, FLPDYR, fips, MARS, age_head, age_spouse, XTOT, V1, n24, EIC, e00200p, e00200s,
           V2:V8, SSP, V8:V15)
  
  return(full)

}