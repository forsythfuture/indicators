# set as working directory because this script is not part of the project yet
setwd("~/work_projects/indicators")

library(tidyverse)
library(survey)

### Create housing variables ###

house_vars <- c('RT', 'SERIALNO', 'DIVISION', 'PUMA', 'ST', 'ADJINC', 'FINCP', 'HINCP',  
                'NP', 'WKEXREL', 'WORKSTAT', 'WGTP')
                
# inivtialize vector to store replicate weight columns
house_rep_weights <- c()
# create housing replicate weight column names    
for (i in 1:80) {
  value <- paste0('wgtp', as.character(i))
  house_rep_weights <- append(house_rep_weights, value)
}

# merge houing replicate weight column names with other colums names
house_vars <- append(house_vars, house_rep_weights)
                
### Create population variables ###

pop_vars <- c('RT', 'SERIALNO', 'SPORDER', 'PUMA', 'ST', 'ADJINC', 'PERNP', 'PINCP', 'PWGTP', 
              'AGEP', 'NWAV', 'ESR', 'RELP', 'SEX', 'WKHP', 'WKL', 'WKW', 'RAC1P', 'FPINCP')

# inivtialize vector to store replicate weight columns
pop_rep_weights <- c()
# create housing replicate weight column names    
for (i in 1:80) {
  value <- paste0('pwgtp', as.character(i))
  pop_rep_weights <- append(pop_rep_weights, value)
}

# merge population replicate weight column names with other colums names
pop_vars <- append(pop_vars, pop_rep_weights)

### import data ###
housing <- read_csv('pums_data/nc_pums_house_16.csv') %>%
  select(one_of(house_vars))

pop <- read_csv('pums_data/nc_pums_pop_16.csv') %>%
  select(one_of(pop_vars))

### create top 10% and 40% income quantiles ###
design <- svydesign(ids=~0, weights=~housing$WGTP, data=housing)

