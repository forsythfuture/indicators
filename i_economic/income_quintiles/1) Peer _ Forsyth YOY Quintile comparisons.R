library(tidyverse)
library(xlsx)

# store name of subdirectory as an object
# now, we can refer to files in the income_quintiles folder by simply refering to subdirectory and file
sub_directory <- 'i_economic/income_quintiles/'

#read in files
CPI <- read.csv(paste0(sub_directory, "CPI.csv"))
Quintiles <- read.csv(paste0(sub_directory, "Income_Quintiles.csv"))

#create condensed dataframe with just 4 columns: Year, County, Quintile and Threshold
Quintile_inc <- Quintiles %>%
        # select the needed variables 
        select(Year, County, Q1_max,Q2_max,Q3_max,Q4_max, Q5_min95) %>%
        # take the columns referenced below and pivot them so they now each appear as a separate row
        gather('Q1_max','Q2_max','Q3_max','Q4_max', 'Q5_min95', key = "Quintile", value = "Threshold" ) %>%
        # take the names of the quarters ('Q1_max', 'Q2_max', etc) and remove the '_max' section
        mutate(Quintile = substr(Quintile, start = 1,stop = 2))

#create condensed dataframe with just 4 columns: Year, County, Quintile and MOE (@90%)
# this section does the same transformations as the previous section, except it does the 
# transformation to the quintile MOE instead of the Quintile point estimate
Quintile_moe <- Quintiles %>%
        select(Year, County, Q1_moe,Q2_moe,Q3_moe,Q4_moe, Q5_moe) %>%
        gather('Q1_moe','Q2_moe','Q3_moe','Q4_moe', 'Q5_moe', key = "Quintile", value = "MOE_90") %>%
        mutate(Quintile = substr(Quintile, start = 1,stop = 2))

#Merge the two condensed dataframes (quintile point estimates and quintile MOEs)
Tmp <- merge(Quintile_inc,Quintile_moe,by = c("Year", "County", "Quintile"))

#Merge new dataframe with the CPI dataframe
Peer_Trends <- merge(Tmp, CPI, by = "Year")

#assign 2016 CPI to variable 
cpi2016 <- CPI$CPI[CPI$Year == 2016]

#Add columns to calculate the Inflation ratio, the (2016 inflation) Adjusted income and MOE for each threshold, 
#the MOE at 95%, the SE and the CV for every row
Peer_Trends <- Peer_Trends  %>% 
  mutate(infl_ratio = cpi2016/CPI, 
         Adj_income = infl_ratio*Threshold, 
         Adj_MOE_90 = infl_ratio*MOE_90,
         MOE_95 = Adj_MOE_90 * (1.96/1.645),
         SE = MOE_95/1.96, # original script had 1.645, should be 1.96
         CV = SE/Adj_income * 100) 

#add a comparison to each county
for (county in c('Forsyth NC','Guilford NC','Pulaski AR','Lafayette Parish LA','Jackson MO')){
        #get data for just that county
        county_comparison <- Peer_Trends[Peer_Trends$County==county,c("Year","Quintile","Adj_income","SE")]
        #rename the variables to name the county.  
        
        colnames(county_comparison)[colnames(county_comparison) == 'Adj_income'] <- paste(county,"Adj_income", sep = "_")
        colnames(county_comparison)[colnames(county_comparison) == 'SE'] <- paste(county,"SE", sep = "_")
        
        #add the column.  Make sure the number of records in df doesn't change
        #all.x means that every record in df is preserved.  If there isn't a matching record it will create null values in the new columns
        Peer_Trends <- merge(Peer_Trends,county_comparison,by=c("Year","Quintile"),all.x=TRUE)
        
        #z score calculation
        Peer_Trends[[paste0(county,"_zdiff")]]<-abs((Peer_Trends[[paste(county,"Adj_income", sep = "_")]] - Peer_Trends$Adj_income)) / sqrt(Peer_Trends[[paste(county,"SE", sep = "_")]]^2 + Peer_Trends$SE^2)
}

#add Year over Year comparison for Forsyth County
#Filter out forsyth county
YOY_FC <- Peer_Trends %>%
        filter(County == "Forsyth NC")
for (year in c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)){
        #get data for just that county
        fc_year_comparison <- YOY_FC[YOY_FC$Year==year,c("Quintile","Adj_income","SE")]
        #rename the variables to name the county.  I don't remember the function to use, so google it
        
        colnames(fc_year_comparison)[colnames(fc_year_comparison) == 'Adj_income'] <- paste(year,"Adj_income", sep = "_")
        colnames(fc_year_comparison)[colnames(fc_year_comparison) == 'SE'] <- paste(year,"SE", sep = "_")
        
        #add the column.  Make sure the number of records in df doesn't change
        #all.x means that every record in df is preserved.  If there isn't a matching record it will create null values in the new columns
        YOY_FC <- merge(YOY_FC,fc_year_comparison,by=c("Quintile"),all.x=TRUE)
        
        #z score calculation
        YOY_FC[[paste0(year,"_zdiff")]]<-abs((YOY_FC[[paste(year,"Adj_income", sep = "_")]] - YOY_FC$Adj_income)) / sqrt(YOY_FC[[paste(year,"SE", sep = "_")]]^2 + YOY_FC$SE^2)
}

Peer_Trends_backup <- Peer_Trends

#Create dataframe of relevant columns
Peer_comparisons <- select(Peer_Trends, 1, 2, 3, 8, 11, 12, ends_with("_Adj_income"), ends_with("_SE"), ends_with("_zdiff"))

#Use "arrange" to sort the order
Peer_comparisons <- arrange(Peer_comparisons, Year, Quintile)

YOY_backup <- YOY_FC

#Create dataframe of relevant columns
Year_comparisons <- select(YOY_FC, 2, 1, 3, 8, 11, 12, ends_with("_Adj_income"), ends_with("_SE"), ends_with("_zdiff"))

#Use "arrange" to sort the order
Year_comparisons <- arrange(Year_comparisons, Year, Quintile)

#write.xlsx(Peer_comparisons, file = paste0(sub_directory, "IncomeQuintiles.xlsx"), sheetName = "Peer_comps_all_data", append = TRUE)
#write.xlsx(Year_comparisons, file = paste0(sub_directory, "IncomeQuintiles.xlsx"), sheetName = "Year_comparisons", append = TRUE)


#Add Poverty thresholds and run statistcal significance against Q1
Fed_Poverty_threshold <- read.xlsx(paste0(sub_directory, "Federal Poverty Thresholds.xls"), 1)
FC_Q1_Incomes <- Year_comparisons %>%
        filter(Quintile == "Q1")%>%
        select(1:5)

Q1_and_Fed_Poverty_comparison <- merge(FC_Q1_Incomes,Fed_Poverty_threshold, by = "Year")

#z score calculation
Q1_and_Fed_Poverty_comparison <- Q1_and_Fed_Poverty_comparison %>%
        mutate(Zdiff = abs(Adj_income - Fed_Poverty_4)/SE)


              