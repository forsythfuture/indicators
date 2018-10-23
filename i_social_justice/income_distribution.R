##################################################################
#
# This function imports income data and expands by weight
# so that entire distribution of incomes can be plotted
#
##################################################################

library(tidyverse)
library(DBI)
library(data.table)

source('i_social_justice/palma_functions.R')
source('functions/puma_functions.R')

# connect to PUMS database
con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")
state <- 37
area_code = c(1801, 1802, 1803)
year <- 2017

# vector that will return all states when filtered
all_states <- seq(1, 100)
# vector to return all PUMAs
all_pumas <- seq(1, 10000)

# create table names
yr <-str_extract(as.character(year), '[0-9][0-9]$')
income_table <- paste0('h_', yr)

# establish connection to tables
income <- tbl(con, income_table)

# import these PUMS variables
income_vars <- c('TYPE', 'PUMA', 'ST', 'HINCP', 'WGTP')

# import incomes
income <- income %>%
  select(!!income_vars) %>%
  filter(# state and PUMA filter
    # if state or PUMA is na, use vector containing all states and pumas for filtering
    ST == 37,
    PUMA %in% c(1801, 1802, 1803),
    TYPE == 1, # housing units only
    (!is.na(HINCP) & HINCP >= 0)) %>% # positive household income
  select(-TYPE) %>%
  select(HINCP, WGTP, PUMA) %>%
  collect() %>%
  as.data.table()

# use weights to repeat rows
income <- income[rep(seq(.N), WGTP), !"WGTP"]

# create transformations
income <- income %>%
  filter(HINCP >= 500) %>%
  as.tibble() %>%
  mutate(PUMA = as.factor(PUMA),
         PUMA = fct_recode(PUMA, `North Winston-Salem` = "1801", 
                           `South Winston-Salem` = "1802",
                           `Kernersville and Clemmons` = '1803'))

income %>%
  ggplot(aes(HINCP, colour = PUMA, fill = PUMA)) +
  stat_ecdf(pad = FALSE) +
  scale_y_continuous('Percentage of population under income level',
                     labels = scales::percent) +
  scale_x_continuous('Income',
                     trans = 'log', labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Distribution of incomes by PUMA in 2016',
    color = 'Area')

# create kde
income %>%
  ggplot(aes(HINCP, colour = PUMA, fill = PUMA)) +
    geom_density(alpha = 0.1, adjust = 2) +
  scale_x_continuous('Income',
                     trans = 'log', labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = 'Distribution of incomes by PUMA in 2017')
