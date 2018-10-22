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
year <- 2016

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

# create histogram
income %>%
  filter(HINCP >= 500) %>%
  as.tibble() %>%
  mutate(PUMA = as.factor(PUMA)) %>%
  ggplot(aes(HINCP, colour = PUMA, fill = PUMA)) +
    geom_density(alpha = 0.1) +
  scale_x_continuous(trans = 'log', 
                     breaks = c(1000, 2000, 5000, 10000, 20000, 50000, 100000, 200000, 500000),
                     labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
