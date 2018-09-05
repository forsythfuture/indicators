library(tidyverse)
library(data.table)
library(pryr)



# total number of households; used to find percentiles
n_income <- length(yearly_income)

# total earnings of bottom 40% and top 10%
bottom <- yearly_income[1:floor(n_income*.40)] %>% sum()
top <- yearly_income[floor(n_income*.90):n_income] %>% sum()

palma <- top / bottom

source('i_social_justice/palma_functions.R')

a <- income_vec_year('14', 'pums')

# calculate palma for each replicate weight of household incomes

lapply(a, palma_cal) %>% unlist(use.names = FALSE)


palma_cal(a[[2]])

a <- fread('pums/ss14husa.csv', select = c(house_vars, house_weights)) %>%
             filter(wgtp2 <= 0,
                    TYPE == 1,
                    RT == 'H')

