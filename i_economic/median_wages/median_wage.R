library(tidyverse)
library(data.table)
library(DBI)

source('i_economic/median_wages/median_wages_functions.R')

con <- dbConnect(RSQLite::SQLite(), "../pums_db.db")

# set geographic unit, either 'county', or 'state'
geo_unit <- 'county'

pop_vars <- c('SERIALNO', 'SPORDER', 'PUMA', 'ST', 'AGEP', 'SEX', 'RAC1P', 'HISP', 'PERNP', 'PINCP', 'WKW', 'WKHP', 'SCH')

years <- seq(2008, 2017, 1)
state <- 37

# initialize dataframe to store all median wage values from all years
median_wages_master <- data.frame()
#yr <- 2017
# for each year
for (yr in years) {
  
  print(yr)

  # create table name
  tbl_name <- as.character(yr) %>%
    str_extract(., '[0-9][0-9]$') %>%
    paste0('p_', .)
  
  tbl <- tbl(con, tbl_name) %>%
    select(!!pop_vars) %>%
    filter(ST == !!state,
           #PUMA %in% c(1800, 1801),
           # filter out people 65 and over
           AGEP < 65,
           # filter out people udner 18, since there are so few of them not in school
           AGEP > 15,
           # filter out people 
           # filter out those currently in school
           # 1 is those not in school
           SCH == 1) %>%
    collect() %>%
    # recode race and create age bins
    clean_demographics(.) %>%
    # add 'total' column, so that when iterating through demographic this column can be added;
    # and we do not return an error due to a lack of the demographic columns
    mutate(total = 100) %>%
    # add up total household income and wages, 
    # and remove people who have more than half of their income from non-wage sources
    group_by(SERIALNO) %>%
    mutate(total_income = sum(PINCP),
           total_wages = sum(PERNP),
           perc_wages = total_wages / total_income) %>%
    ungroup() %>%
    filter(perc_wages > 0.5) %>%
    select(-total_income:-perc_wages)
  
  
  # recode weeks worked from integer category to actual number
  hours_recode <- list(`1` = 51,
                       `2` = 48.5,
                       `3` = 43.5,
                       `4` = 33,
                       `5` = 20,
                       `6` = 7)
  
  # create lookup table of counties and PUMAs, so that county names can be added
  counties <- create_counties(yr)
  
  tbl <- tbl %>%
    # recode weeks wored during year
    # 2007 already shows number so don't recode
    mutate(weeks_worked = if (yr != 2007) recode(.$WKW, !!!hours_recode) else .$WKW,
           # calcualte hourly rate by dividing wages by total hours worked
           # total hours worked is weeks worked times hours per week
           wage = PERNP / (weeks_worked * WKHP)) %>%
    # eliminate values 0 or less as this likely represent self-employed people with odd circumstances
    # this also eliminates null values
    filter(wage > 0) %>%
    # drop unneeded variables
    select(-PERNP:-SCH, -weeks_worked) %>%
    # join in county names based on PUMA
    left_join(counties, by = 'PUMA')
  
  # only need select counties of geographic unit is countis
  tbl <- if (geo_unit == 'county') filter(tbl, cntyname %in% c('Forsyth', 'Guilford', 'Durham')) else tbl
  
  ### import weights table
  
  # must define weight variable names within year loop because names change dpending on year
  # replciate weight variable names are lower case until 2017 and upper case starting in 2017
  weight_names <- ifelse(yr >= 2017, 'PWGTP', 'pwgtp')
  
  replicate_weights <- c('PWGTP', paste0(weight_names, seq(1, 80)))
  
  # replicate weight variables
  pop_weights <- c('SERIALNO', 'SPORDER', 'PWGTP', replicate_weights)
  
  # import all table weights
  wgt_tbl <- tbl(con, tbl_name) %>%
    filter(ST == !!state,
           #PUMA %in% c(1800, 1801),
           # filter out people 65 and over
           AGEP < 65,
           # filter out people udner 18, since there are so few of them not in school
           AGEP > 15,
           # filter out those currently in school
           # 1 is those not in school
           SCH == 1) %>%
    select(!!pop_weights) %>%
    collect() 
  
  # iterate through whole state and county
  for (geo_unit in c('state', 'county')) {
  
    ##### calculate median wages
    
    # demographics to iterate through
    demographics <- c('AGEP', 'SEX', 'RAC1P', 'total')
    #demo <- 'AGEP'
    ## loop though each demographic
    for (demo in demographics) {  
      
      print(demo)
      
      # create columns to group on,
      # group by county name and demographic if geo_unit is county,
      # only group by  state if geographic unit is state
      
      group_cols <- if (geo_unit == 'county') c(demo, 'cntyname') else demo
      
      # attache weight column to dataset containing wages
      median_wage <- tbl %>%
        # join wage data
        left_join(wgt_tbl, by = c('SERIALNO', 'SPORDER')) %>%
        # group by required columns, based on demographic
        group_by_at(group_cols)
      
      # find median wage by demographic for all replciate weights
      median_demo <- lapply(replicate_weights, 
                            function(x) find_median(median_wage, demo, x, geo_unit))
      
      # calcualte standard error
      median_se <- find_se(median_demo)
      
      # add standard errors to dataset containing median wage values
      # first dataframe in list contains median vage values from primary weight
      median_demo <- median_demo[[1]] %>%
        ungroup() %>%
        mutate(se = median_se[[1]],
               # adde column for demographic
               type = demo,
               year = yr)
      
      colnames(median_demo) <- c('subtype', 'geo_area', 'estimate', 'se', 'type', 'year')
      
      median_wages_master <- bind_rows(median_wages_master, median_demo)
      
    }
      
  }
    
}

# inflation adjust median wages to most recent year
median_wages_master_inf <- inflation_adjust(median_wages_master, estimate, se, 
                                 max(median_wages_master$year), error = TRUE)

# write out county estimates
write_csv(median_wages_master_inf, 'i_economic/median_wages/median_wages.csv')

# import county estimates
median_wages <- read_csv('i_economic/median_wages/median_wages.csv') %>%
  # remove non-inflation adjusted values
  select(-estimate, -se) %>%
  # rename inflation adjusted values to remove inf suffix
  rename(geo_description = geo_area, estimate = estimate_adj, 
         se = se_adj, moe = moe_adj, cv = cv_adj) %>%
  # RAC1P values of 4 is other, and we do not need
  filter(!(type == 'RAC1P' & subtype == 4))

# recode demographic labels to be more descriptive

race_labels <- seq(1, 3)
race_recode <- c('White, non-Hispanic', 'African American', 'Hispanic / Latino')

sex_labels <- c(1, 2)
sex_recode <- c('Male', 'Female')

age_labels <- c(21, 29, 44, 64)
age_recode <- c('16 to 21', '22 to 29', '30 to 44', '45 to 64')

# map recoding of sub demographics
median_wages$subtype <- ifelse(median_wages$type == 'RAC1P', 
                                     plyr::mapvalues(median_wages$subtype, race_labels, race_recode),
                                     ifelse(median_wages$type == 'SEX', 
                                            plyr::mapvalues(median_wages$subtype, sex_labels, sex_recode),
                                            ifelse(median_wages$type == 'AGEP', 
                                                   plyr::mapvalues(median_wages$subtype, age_labels, age_recode),
                                                   'Total')))

# recode demographic names
median_wages$type <- recode(median_wages$type, 
                                 RAC1P = 'Race / Ethnicity', SEX = 'Gender', 
                                 AGEP = 'Age', total = 'Comparison Community')

# write out to shiny app
write_csv(median_wages, 'i_economic/median_wages/shiny_median_wages/median_wages_shiny.csv')
