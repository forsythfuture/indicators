df_backup<-df

#add a comparison to each county
for (county in c('Forsyth NC','Guilford NC','Pulaski AR','Lafayette Parish LA','Jackson MO')){
  #get data for just that county
  county_comparison <- df[df$County==county,c("Sample","Year","Quintile","Demographic","proportion","SE")]
  #rename the variables to name the county.  I don't remember the function to use, so google it
  
  colnames(county_comparison)[colnames(county_comparison) == 'proportion'] <- paste(county,"proportion", sep = "_")
  colnames(county_comparison)[colnames(county_comparison) == 'SE'] <- paste(county,"SE", sep = "_")
 
  #add the column.  Make sure the number of records in df doesn't change
  #all.x means that every record in df is preserved.  If there isn't a matching record it will create null values in the new columns
  df <- merge(df,county_comparison,by=c("Sample","Year","Quintile","Demographic"),all.x=TRUE)
  
  #z score calculation
  df[[paste0(county,"_zdiff")]]<-abs((df[[paste(county,"proportion", sep = "_")]] - df$proportion)) / sqrt(df[[paste(county,"SE", 
        sep = "_")]]^2 + df$SE^2)
}


#add a comparison for each demographic

#list of lists defining what categories to look at
race_eth_list <- list('Black','Hispanic','Whitenh')
age_list <- list('Age18_44','Age45_64','Age65_plus')


demographics <- list(race_eth_list,age_list)

#add the values / SEs for each demographic
for (demographic in list('Black','Hispanic','Whitenh','Age18_44','Age45_64','Age65_plus')){
  #make a table that just has records for this demographic
  #case sensitive, check against real variables
  demo_df <- df[df$Demographic==demographic,c("Sample","Year","County","Quintile","proportion","SE")]

  #rename the variables to name the county.  I don't remember the function to use, so google it
  colnames(demo_df)[colnames(demo_df) == 'proportion'] <- paste(demographic,"proportion", sep = "_")
  colnames(demo_df)[colnames(demo_df) == 'SE'] <- paste(demographic,"SE", sep = "_")
  
  #add columns to df with the proportion / se for this demographic in the same county and year
  df <- merge(df,demo_df,by=c("Sample","Year","County", "Quintile"),all.x=TRUE)
  
  #z score calculation comparing the current record to the demographic
  df[[paste0(demographic,"_zdiff")]]<-abs((df[[paste(demographic,"proportion", sep = "_")]] - df$proportion)) / sqrt(df[[paste(demographic,"SE", sep = "_")]]^2 + df$SE^2)
}


#df should now have every proportion and record you want, and more!
#you can create one version of the table for each analysis with fewer columns.
#You could just keep the proportion and z variables relevant to the analysis.  
#SEs could be useful for error checking, but you don't ultimately care about them

raw_data <- df
df <- raw_data
Forsyth_comparisons <- df %>%
        filter(County == "Forsyth NC")

Forsyth_comparisons_backup <- Forsyth_comparisons
Forsyth_comparisons <- Forsyth_comparisons_backup

#Create dataframe of Age comparisons
Age_comparisons <- Forsyth_comparisons %>%
        filter(substr(Demographic,1,3) =="Age") %>%
        select(1:8, starts_with("Age")) %>%
        arrange(Demographic, Quintile)

#YOY comparisons by Age
Tmp <- Age_comparisons
for (year in c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)){
        for (demographic in list('Age18_44','Age45_64','Age65_plus')){
                #make a table that just has records for this demographic
                #case sensitive, check against real variables
                demo_df <-Tmp[Tmp$Year==year & Tmp$Demographic==demographic,c("Quintile","proportion","SE")]
                
                #rename the variables to name the county.  I don't remember the function to use, so google it
                colnames(demo_df)[colnames(demo_df) == 'proportion'] <- paste(year, demographic,"proportion", sep = "_")
                colnames(demo_df)[colnames(demo_df) == 'SE'] <- paste(year, demographic,"SE", sep = "_")
                
                #add columns to df with the proportion / se for this demographic in the same county and year
                Tmp <- merge(Tmp,demo_df,by=c("Quintile"),all.x=TRUE)
                
                #z score calculation comparing the current record to the demographic
                Tmp[[paste(year, demographic,"zdiff", sep = "_")]]<-abs((Tmp[[paste(year, demographic,"proportion", sep = "_")]] - Tmp$proportion)) / sqrt(Tmp[[paste(year, demographic,"SE", sep = "_")]]^2 + Tmp$SE^2)
        }
}

#Create all Age 18-44 df
YOY_Age18_44_comparisons <- Tmp %>%
        filter(Demographic == "Age18_44") %>%
        select(2,3,1,4:8, ends_with("_Age18_44_proportion"), ends_with("_Age18_44_zdiff"), ends_with("_Age18_44_SE")) %>%
        arrange(Year, Quintile)

#Create all Age 45-64 df
YOY_Age45_64_comparisons <- Tmp %>%
        filter(Demographic == "Age45_64") %>%
        select(2,3,1,4:8, ends_with("_Age45_64_proportion"), ends_with("_Age45_64_zdiff"), ends_with("_Age45_64_SE")) %>%
        arrange(Year, Quintile)

#Create all Age 65+ df
YOY_Age65_plus_comparisons <- Tmp %>%
        filter(Demographic == "Age65_plus") %>%
        select(2,3,1,4:8, ends_with("_Age65_plus_proportion"), ends_with("_Age65_plus_zdiff"), ends_with("_Age65_plus_SE")) %>%
        arrange(Year, Quintile)

#Create dataframe of race comparisons
Forsyth_comparisons <- Forsyth_comparisons_backup

race <- c('Black','Hispanic','Whitenh')

Race_comparisons <- Forsyth_comparisons %>%
        filter(Demographic %in% race) %>%
        select(1:8, starts_with("Black"), starts_with("Hispanic"), starts_with("Whitenh")) %>%
        arrange(Demographic, Quintile)

#YOY comparisons by race
Tmp <- Race_comparisons
for (year in c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)){
        for (demographic in list('Black','Hispanic','Whitenh')){
                #make a table that just has records for this demographic
                #case sensitive, check against real variables
                demo_df <-Tmp[Tmp$Year==year & Tmp$Demographic==demographic,c("Quintile","proportion","SE")]
                
                #rename the variables to name the county.  I don't remember the function to use, so google it
                colnames(demo_df)[colnames(demo_df) == 'proportion'] <- paste(year, demographic,"proportion", sep = "_")
                colnames(demo_df)[colnames(demo_df) == 'SE'] <- paste(year, demographic,"SE", sep = "_")
                
                #add columns to df with the proportion / se for this demographic in the same county and year
                Tmp <- merge(Tmp,demo_df,by=c("Quintile"),all.x=TRUE)
                
                #z score calculation comparing the current record to the demographic
                Tmp[[paste(year, demographic,"zdiff", sep = "_")]]<-abs((Tmp[[paste(year, demographic,"proportion", sep = "_")]] - Tmp$proportion)) / sqrt(Tmp[[paste(year, demographic,"SE", sep = "_")]]^2 + Tmp$SE^2)
        }
}

#Create all Blacks df
YOY_Black_comparisons <- Tmp %>%
        filter(Demographic == "Black") %>%
        select(2,3,1,4:8, ends_with("_Black_proportion"), ends_with("_Black_zdiff"), ends_with("_Black_SE")) %>%
        arrange(Year, Quintile)

#Create all hispanics df
YOY_Hispanic_comparisons <- Tmp %>%
        filter(Demographic == "Hispanic") %>%
        select(2,3,1,4:8, ends_with("_Hispanic_proportion"), ends_with("_Hispanic_zdiff"), ends_with("_Hispanic_SE")) %>%
        arrange(Year, Quintile)

#Create all whites df
YOY_White_comparisons <- Tmp %>%
        filter(Demographic == "Whitenh") %>%
        select(2,3,1,4:8, ends_with("_Whitenh_proportion"), ends_with("_Whitenh_zdiff"), ends_with("_Whitenh_SE")) %>%
        arrange(Year, Quintile)

library(xlsx)
#received error when trying to add multiple tabs:"Error in .jcall("RJavaTools", "Ljava/lang/Object;", "invokeMethod", cl, : java.lang.OutOfMemoryError: Java heap space"
#solution was to increase java parameters, but had to unload dependent package first
unloadNamespace("xlsx")
options(java.parameters = "-Xmx1000m")
install.packages("xlsx")
library("xlsx")

write.xlsx(raw_data, file = "IncomeQuintiles.xlsx", sheetName = "Raw_data", append = TRUE)
write.xlsx(YOY_Age18_44_comparisons, file = "IncomeQuintiles.xlsx", sheetName = "YOY_Age18_44", append = TRUE)
write.xlsx(YOY_Age45_64_comparisons, file = "IncomeQuintiles.xlsx", sheetName = "YOY_Age45_64", append = TRUE)
write.xlsx(YOY_Age65_plus_comparisons, file = "IncomeQuintiles.xlsx", sheetName = "YOY_Age65+", append = TRUE)
write.xlsx(YOY_Black_comparisons, file = "IncomeQuintiles.xlsx", sheetName = "YOY_Black", append = TRUE)
write.xlsx(YOY_Hispanic_comparisons, file = "IncomeQuintiles.xlsx", sheetName = "YOY_Hispanic", append = TRUE)
write.xlsx(YOY_White_comparisons, file = "IncomeQuintiles.xlsx", sheetName = "YOY_White", append = TRUE)

