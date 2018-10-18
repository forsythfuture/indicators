library(tidyverse)
library(DBI)
library(data.table)

source('i_social_justice/palma_functions.R')


# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

household_incomes <- house_incomes(con, 2016)

#household_incomes <- household_incomes %>%
#  filter(ST == 37 & PUMA %in% c(1801, 1802, 1803))

# start incorporating replciate weights

# replicate weight variables
house_weights <- c('WGTP', paste0('wgtp', seq(1, 80))) 

# create connection with housing replicate weights
weights_tbl <- tbl(con, "h_16") %>%
  filter(#PUMA %in% c(1801, 1802, 1803), # Winston Salem
         ST == 37 # North Carolina (need both puma and state)
         ) %>%
  select(SERIALNO, !!house_weights)

# initialize list to store palma values
# each item in list will contain dataframe of Palma values for all grouped
# geographies and one replicate weight
palmas <- list()

# iterate through each replicate weight
for (weight in house_weights) {
  
  # bring into memory dataframe with serialno and just one weight column
  wgt <- weights_tbl %>%
    select(SERIALNO, !!weight) %>%
    collect() %>%
    # convert to data table
    as.data.table()
  
  # this provides dataframe for entire US with income, weights, and geography
  # it can then be filtered by geography
  household_incomes_wgt <- wgt[household_incomes, on = 'SERIALNO']
  
  # change name of column so that it is the same of each iteration
  setnames(household_incomes_wgt, weight, 'wgt')
  
  palmas[[weight]] <- household_incomes_wgt[
    # filter out replicate weight values less than 1
    wgt > 0][
    # add additional rows based on the number of replicate weights
    rep(seq(.N), wgt), !"wgt"][
    # order based on income
    order(income, PUMA)][
    # calculate palma for each group
    # use 'get' to convert string to object name
    ,.(palma_cal(income)), by = 'PUMA']

}

# create list of column names for dataframe taht contains all Palmas
col_names <- append('PUMA', house_weights)

# currently, each weight is in a different dataframe
# combine them all into one dataframe where each column is a replicate weight and each row is a geography
palmas_full <- reduce(palmas, left_join, by = 'PUMA')
# rename variables
colnames(palmas_full) <- col_names
# transpose so that rows are weights and columsn are geographies
palmas_t <- as.data.frame(t(palmas_full))
# the first row is the PUMA numeber; save as object and remove
pumas <- palmas_t[1,]
palmas_t <- palmas_t[2:nrow(palmas_t),]

# This sequence does the following:
#   calculate st error of palmas
#   convert list of standard errors to dataframe with PUMAs included
#   bind standard error dataframe to dataframe containing palma values
#
# calculate standard error
palma_complete <- sapply(palmas_t,  function(x) palma_st_error(as.vector(x))) %>%
  # bind with dataframe containing PUMAs
  bind_rows(pumas) %>%
  # transpose so that each row is a PUMA and each column is a SE
  t() %>%
  # convert back to dataframe
  as.data.frame() %>%
  # rename columns
  rename(se = V1, PUMA = V2) %>%
  # bind to dataframe containing PUMA values
  left_join(palmas_full, ., by = 'PUMA') %>%
  # remove replicate weight values
  select(PUMA, WGTP, se) %>%
  rename(palma = WGTP)