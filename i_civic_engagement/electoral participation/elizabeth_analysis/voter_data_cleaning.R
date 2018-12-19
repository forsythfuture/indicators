### Install packages 

#install.packages("dplyr")
library(dplyr)

#install.packages("gmodels")
library(gmodels)

#install.packages("tidycensus")
library(tidycensus)

### Set Working Directory 

#setwd("C:/Users/futures/OneDrive - Forsyth Futures/Shared with Everyone/2017 Indicator Expansion/Electoral Participation/Raw Data/NCState")

### Open files and create history and voter objects 

unzip("ncvoter_Statewide.zip")
unzip("ncvhis_Statewide.zip")

history <- read.delim("ncvhis_Statewide.txt")
voter <- read.delim("ncvoter_Statewide.txt")

###  Select needed columns 

history <- select(history, voter_reg_num, election_lbl, election_desc, voted_county_desc, 
                  voted_county_id)
voter <- select(voter, voter_reg_num, status_cd, res_street_address,
                res_city_desc, state_cd, zip_code, race_code, ethnic_code, 
                gender_code, birth_age, birth_year, registr_dt)

###   Exloring voter file for data cleaning

###     making lists for summary and table views 

summarylist <- list(voter$birth_age, voter$birth_year, voter$registr_dt)
tablelist <- list(voter$status_cd, voter$res_city_desc, voter$state_cd, 
                  voter$zip_code, voter$race_code, voter$ethnic_code, 
                  voter$gender_code, history$election_lbl, history$election_desc,
                  history$voted_county_desc)

###     making function for making tables 

frequency <- function(data){
  print(table(data))
  print(prop.table(table(data)))
}

###     printing out summary information for cleaning 

cleaninfo <- list(sapply(summarylist, summary), sapply(tablelist, frequency))

###     checking number and percent missing for age, birth year, and registration year  

table(voter$birth_age)
prop.table(table(voter$birth_age))

table(voter$birth_year)
prop.table(table(voter$birth_year))

###     creating and checking variable to compare birth age and birth year

voter$agecheck <- (2018 - voter$birth_year) - voter$birth_age
table(voter$agecheck)

###     creating varaible for registration yearchecking it

voter$registr_dt <- as.Date(voter$registr_dt, format = "%m/%d/%Y")
voter$registr_year <- lubridate::year(voter$registr_dt)
table(voter$registr_year)

###     checking age at date of registration 

###       creating a variable

voter$age_reg <- voter$registr_year - voter$birth_year

###       creating a variable to flag problems

voter$age_reg_flag <- 0 
voter$age_reg_flag[voter$age_reg < 16] <- 1
table(voter$age_reg_flag)

###     checking voted date against description 

date_desc_table <- as.data.frame(table(history$election_lbl, history$election_desc))

###     checking to see if ethnicity is missing at random 

###         Flagging ethnicity as missing 

voter$ethnic_code_flag <-0
voter$ethnic_code_flag[voter$ethnic_code == "UN"] <- 1

###         chekcing missing ethnicity by registration year 

CrossTable(voter$registr_year, voter$ethnic_code_flag, 
           prop.t = FALSE,
           prop.chisq = FALSE
)

#######################################################################
########################Begin Analysis Here############################

### Select Needed Variables for voter (history has needed variables)

voter <- select(voter, voter_reg_num, res_street_address, res_city_desc,
                state_cd, zip_code, race_code, ethnic_code, gender_code, birth_age,
                age_reg_flag)

### merging the voter registration and history files 

data <- merge(voter, history, by = "voter_reg_num")

### Calculating age at election 

###     marking improbable registration ages and missing age data as missing 

data$birth_age[data$age_reg_flag == 1] <- NaN
data$birth_age[data$birth_age == 118] <- NaN

###     calculating election year 

data$election_year <- as.Date(data$election_lbl, format= "%m/%d/%Y")
data$election_year <-lubridate::year(data$election_year)

###     calculating age at election 

data$election_age <- data$birth_age - (2018 - data$election_year)

###     marking age at electino missing if under 18 or over 105

data$election_age[data$election_age < 18] <- NaN
data$election_age[data$election_age > 105] <- NaN

###     creating age groups 

data$election_age_group <- " " 
data$election_age_group[data$election_age < 26] <- "18 to 25"
data$election_age_group[data$election_age >= 26 & data$election_age <= 29] <- "26 to 29"
data$election_age_group[data$election_age >= 30 & data$election_age <= 49] <- "30 to 49"
data$election_age_group[data$election_age >= 50 & data$election_age <= 64] <- "50 to 64"
data$election_age_group[data$election_age >= 65 & data$election_age <= 84] <- "65 to 84"
data$election_age_group[data$election_age >= 85] <- "85 and Older"

### recoding race

data$race_code_rcd <- data$race_code
ifelse(as.character(data$ethnic_code == "HL"), 'HL', as.character(data$race_code_rcd))

### calculating votes per presidential election 

###     summarzing number of votes per election  

vote_count_table <- as.data.frame(table(data$election_desc, data$voted_county_desc, 
                                        data$election_age_group, data$race_code_rcd, 
                                        data$gender_code, 
                                        dnn = list("election_desc", "voted_county_desc", 
                                                   "election_age_group", "race_code",
                                                   "gender_code")))