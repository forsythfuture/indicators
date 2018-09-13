# This script randomly generates Z scores and tests them for significance

source('functions/acs/acs_functions_test.R')

# Size reflects the length and width of the grid and this value can be changed.
size <- 20

matrix(data = rnorm(size*size,mean = 1.96, sd = .5), ncol=size, nrow=size) %>%
  as.data.frame() %>%
  ff_acs_zplot()
