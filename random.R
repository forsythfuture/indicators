library(tidyverse)
library(DBI)

con <- dbConnect(RSQLite::SQLite(), "puma_data/pums_db.db")

year <- 2017

housing <- tbl(con, 'h_17') %>%
  select(SERIALNO, ST, PUMA, HUPAC) %>%
  filter(ST == 37) %>%
  collect() %>%
  filter(PUMA %in% c(1801, 1802, 1803)) 

# replicate weight variables
house_weights <- c('WGTP', paste0('WGTP', seq(1, 80)))

population <- tbl(con, 'p_17') %>%
  select(SERIALNO, AGEP, ST, PUMA) %>%
  filter(ST == 37) %>%
  collect() %>%
  filter(PUMA %in% c(1801, 1802, 1803)) %>%
  select(-PUMA, -ST)

# combine household weights to dataset with household of kids
house <- tbl(con, 'h_17') %>%
  select(SERIALNO, ST, PUMA, !!house_weights) %>%
  filter(ST == 37) %>%
  collect() %>%
  filter(PUMA %in% c(1801, 1802, 1803)) %>%
  select(-PUMA, -ST)

# combine household weights to kids dataset
population <- left_join(population, house, by = 'SERIALNO')

# houses with children under 9
under9 <- population %>%
  filter(AGEP < 9) %>%
  select(-AGEP) %>%
  distinct(SERIALNO, .keep_all = TRUE)

total_under9 <- sum(under9$WGTP)

# replicate weights
under9_weights <- sapply(house_weights[-1], function(x) sum(under9[[x]])) %>%
  sapply(., function(x) (total_under9 - x)^2) %>%
  sum()

under9_se <- sqrt( under9_weights * (4/80) ) * 1.645


# houses with children under 6
under6 <- population %>%
  filter(AGEP < 6) %>%
  select(-AGEP) %>%
  distinct(SERIALNO, .keep_all = TRUE)

total_under6 <- sum(under6$WGTP)

# replicate weights
under6_weights <- sapply(house_weights[-1], function(x) sum(under6[[x]])) %>%
  sapply(., function(x) (total_under6 - x)^2) %>%
  sum()

under6_se <- sqrt( under6_weights * (4/80) ) * 1.645


