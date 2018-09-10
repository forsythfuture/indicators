############ begin code for replicate calculations.  You can run this section without reading it   ################################

#there are two important things to know about PUMS analysis, whether or not you go over the code below.
#One is that there are separate weights for people and households.  The functions assume that you are measuring people,
#but for household measures you must specify type='h' in the functions

#One is that SEs for measures are estimated by lookin at how the measure varies when it is artificially resampled,
#and this estimate isn't always accurate for extreme values or medians.
#You have a true weight (WGTP/PWGTP) which reflects the number of households/people, and each of those has 
#80 replicate weights (WGTP1-80 and PWGTP1-80).  You the replicate_calculation function takes 81 measurements
#based on the different weights and estimates a SE based on how much they differ.

#HOWEVER, if you are measuring a rare occurance / using a small population, 
#it is possible thatevery record has the same value and the SE will be 0.
#similarly, it is possible that a median will not change mcuh in response to reweighting.
#you should only calculate medians with PUMS data when using continuous data that doesn't have clusters 



replicate_calculation <- function(observed,reweighted_calculations){
        #calculate a SE based on the square difference between the observed value and 80 other values based on different weights
        #REMEMBER TO USE PWGTP FOR PERSON LEVEL ANALYSIS AND WGTP FOR HOUSEHOLD LEVEL ANALYSIS.
        #reweighted_calculations must be an 80 item list
        if (!length(reweighted_calculations)==80){stop("The second argument did not have an 80 item list")}
        
        #this doesn't work well when calculating medians / quantiles
        #and can return a SE of 0 if there are few enough records that they are all the same
        #(e.g. if  there is only one record that is a hispanic citizen over 65 in Forsyth, the unemployment rate of this group value would be 0 or 100 no matter how you reweight the calculation, causing the SE to be 0)
        
        
        #calculate the sum of squared differences
        #this formula can be found in https://usa.ipums.org/usa/repwt.shtml among other places
        sum_of_squared_diff <-0
        for (reweighted_calculation in reweighted_calculations){
                
                diff <- observed-reweighted_calculation
                sqdiff <- diff^2
                #add the squared diff to a running total
                sum_of_squared_diff <- sum_of_squared_diff+sqdiff
                
        }
        SE <- sqrt(sum_of_squared_diff/20)
        
        return(SE)
        
}

#calculate the percent of a group that has certain characteristics
replicate_proportion <- function(df,numerator_var,numerator_value,denominator_var="Allrecords",denominator_value=1,type='p',
                                 year=2016,county="Forsyth NC",samplespan=1){
        
        #define weights as either the person/household weights
        if (type=='p'){
                weight_type_r <- "pwgtp"
                weight_type <- "PWGTP"
        } else {
                weight_type_r <- "wgtp"
                weight_type <- "WGTP"
                
                #if this is a calculation of household characteristics, just look at one record per household 
                #this also means that any demographics referenced in the function will be just for the householder
                df <- df[df[["RELP"]]==0,]
        }
        
        #filter the base dataframe
        #filter to one county/year/sample
        df <-  df[df$year==year & df$sample==samplespan & df$county==county,]
        
        #custom filter
        df <-  df[df[[denominator_var]]==denominator_value,]
        dfn <- df[df[[numerator_var]]==numerator_value,]
        
        
        #get population counts
        denominator_population <- sum(df[[weight_type]])
        numerator_population <- sum(dfn[[weight_type]])
        proportion <- numerator_population/denominator_population
        
        #a blank vector to paste values into
        reweighted_results <- vector("list", 80)
        reweighted_num <- vector("list", 80)
        reweighted_denom <- vector("list", 80)
        
        #this makes WGTP/PWGTP 1-80
        for (weightnum in 1:80){
                weightvar <- paste(weight_type_r,as.character(weightnum),sep="")
                #reweighted values
                denominator_population_tmp <- sum(df[[weightvar]])
                numerator_population_tmp <- sum(dfn[[weightvar]])    
                proportion_tmp <- numerator_population_tmp/denominator_population_tmp
                
                #save the reweighted proportion
                reweighted_results[weightnum] <- proportion_tmp
                reweighted_num[weightnum] <- numerator_population_tmp
                reweighted_denom[weightnum] <- denominator_population_tmp
                
                
        }
        SE <- replicate_calculation(proportion,reweighted_results)
        SE_num <- replicate_calculation(numerator_population,reweighted_num)
        SE_denom <- replicate_calculation(denominator_population,reweighted_denom)
        CV=100*SE/proportion
        
        if(type=='p'){type_name<-"people"}else{type_name<-"households"}
        
        print(paste(proportion,'of',type_name,'where',denominator_var,"is",denominator_value,'have a',numerator_var,"of",numerator_value,"+/-",SE*1.96))
        
        return(c(proportion,SE,CV,numerator_population,SE_num,denominator_population,SE_denom))
}





#calculate population a group that has certain characteristics
replicate_count <- function(df,var,value,type='p',filter_var="Allrecords",filter_value=1,
                            year=2016,county="Forsyth NC",samplespan=1){
        
        #filter to one county/year/sample
        df <-  df[df$year==year & df$sample==samplespan & df$county==county,]
        
        #custom filter
        df <- df[df[[filter_var]]==filter_value,]
        
        
        #define weights as either the person/household weights
        if (type=='p'){
                weight_type_r <- "pwgtp"
                weight_type <- "PWGTP"
        } else {
                weight_type_r <- "wgtp"
                weight_type <- "WGTP"
                
                #if this is a calculation of household characteristics, just look at one record per household (RELP=0)
                #this also means that any demographics referenced in the function will be just for the householder
                df <- df[df[["RELP"]]==0,]
        }
        
        #filter the base dataframe
        df <-  df[df[[var]]==value,]
        
        
        
        #get population counts
        population <- sum(df[[weight_type]])
        
        
        
        #a blank vector to paste values into
        reweighted_results <- vector("list", 80)
        
        
        
        #this makes WGTP/PWGTP 1-80
        for (weightnum in 1:80){
                weightvar <- paste(weight_type_r,as.character(weightnum),sep="")
                #reweighted values
                population_tmp <- sum(df[[weightvar]])
                
                #save the reweighted proportion
                reweighted_results[weightnum] <- population_tmp
                
                
        }
        SE <- replicate_calculation(population,reweighted_results)
        CV=100*SE/population
        
        if(type=='p'){type_name<-"people"}else{type_name<-"households"}
        
        
        print(paste("there are ",population,type_name,"+/-",SE*1.96,"where",var,"is",value))
        
        return(c(population,SE,CV))
}


############### end PUMS functions #########################











#############   PUMS Income by Quintiles #####################

#replace this with wherever the pums data is

data_directory <- "C:/Users/staff/Documents/Income by quintiles"
pums <- readRDS(file=paste(data_directory,"./all_pums_data.rds",sep="")) 

#added ./ since I threw this error, "Error in gzfile(file, "rb") : cannot open the connection In addition: 
#Warning message: In gzfile(file, "rb") : cannot open compressed file 'C:/Users/staff/Documents/Income by 
#quintilesall_pums_data.rds', probable reason 'No such file or directory'


#each analysis should only deal to one sampleframe, one county, and one year.


#filter the alldata to just the desired records (in this case 1 year samples)

pums <- pums[(pums$sample == 1 & pums$TYPE == 1 & pums$county != "Roanoke city VA"),]


#for this analysis you will be comparing household income quintiles imported from a spreadsheet to actual household income (HINCP)

#add quintile variables to check against income.  There are lots of ways we could do this.
#I will suggest that we make one threshold variable for each quintile, and then use that to make an indicator for each quintile.

#to get the quintiles, we could have one line of code for each quintile, likealldata[alldata$county == 'Forsyth NC' & alldata$year == 2016,'Q1_thresh'] <- 20257
#we could also say quintile_check <- function(HINCP, county, year, quintile){....} and then use mapply. 
#That would leave us with fewer columns, which could make the files smaller, but I would prefer to leave the thresholds in the dataframe for reference
#I don't want to do either of these because there would be 10 years * 5 counties * 4 thresholds = 200 lines of code defining thresholds

#instead I suggest you a table of thresholds in excel that will be imported and merged to the data file
#this makes the code shorter, and more importantly allows us to copy and paste from excel, which reduces potential errors.

#thresholds need to be found in B19080: HOUSEHOLD INCOME QUINTILE UPPER LIMITS.  Be sure to use the 1 year file
#actually I have downloaded the files for you and created a template for creating the table to import


thresholds <- read.table(paste0(data_directory,"./Quintile thresholds.csv"), 
                         header = TRUE,
                         sep = ",")
summary <- pums %>%
        group_by(county, sample, year) %>%     # multiple group columns
        summarise(total_pop = sum(PWGTP))  # multiple summary columns

print(summary)

relp_adjust <- function(samplespan,year,relp,rel){
        if (samplespan==-1){return(relp)
        }else if (year>2009){return(relp)
        }else if (year<=2007){
                if (rel<=2){return(rel)
                }else if (rel<=5 & rel >=3){return(rel+2)
                }else{return(rel+3)}
        }else{return(rel)}
}




pums$RELP <- mapply(relp_adjust, pums$sample,pums$year,pums$RELP,pums$REL)

#merge this to the pums data by year and county
pums <- merge(pums,thresholds,by = c("county","year"))

#convert incomes & thresholds (& MOEs?) to 2016 inflation adjusted dollars

pums[pums$year == 2016, "inflation_adj"] <- 1
pums[pums$year == 2015, "inflation_adj"] <- 1.013
pums[pums$year == 2014, "inflation_adj"] <- 1.014
pums[pums$year == 2013, "inflation_adj"] <- 1.030
pums[pums$year == 2012, "inflation_adj"] <- 1.045
pums[pums$year == 2011, "inflation_adj"] <- 1.067
pums[pums$year == 2010, "inflation_adj"] <- 1.101
pums[pums$year == 2009, "inflation_adj"] <- 1.119
pums[pums$year == 2008, "inflation_adj"] <- 1.115
pums[pums$year == 2007, "inflation_adj"] <- 1.158
pums[pums$year == 2006, "inflation_adj"] <- 1.191

pums$HINCP <- pums$HINCP * pums$inflation_adj

pums$Q1_max <- pums$Q1_max * pums$inflation_adj
pums$Q2_max <- pums$Q2_max * pums$inflation_adj
pums$Q3_max <- pums$Q3_max * pums$inflation_adj
pums$Q4_max <- pums$Q4_max * pums$inflation_adj
pums$Q5_min95 <- pums$Q5_min95 * pums$inflation_adj

#compare HINCP to thresholds to create indicator variables (1 means above the threshold)
pums$Q1 <- as.numeric(pums$HINCP <= pums$Q1_max)
pums$Q2 <- as.numeric(pums$HINCP <= pums$Q2_max & pums$HINCP > pums$Q1_max)
pums$Q3 <- as.numeric(pums$HINCP <= pums$Q3_max & pums$HINCP > pums$Q2_max)
pums$Q4 <- as.numeric(pums$HINCP <= pums$Q4_max & pums$HINCP > pums$Q3_max)
pums$Q5 <- as.numeric(pums$HINCP > pums$Q4_max)

#create the demographic variables we want.  
#Since we are measuring household income we must measure demographics as the household level.
#So far, we have summarized demographics to the household level by 
#looking at the demographics of the reference person (the person who filled out the survey), even if that is somewhat arbitrary.
#this is done in the function automatically with (type='h') 

#there are already some indicators for race, but do not use them!  They are for [race] alone or in combination with others,
#but we just want [race] alone because that is how the racial AFF tables are made

pums$Black <- as.numeric(pums$RAC1P == 2) 
pums$Hispanic <- as.numeric(pums$HISP > 1)  
pums$Whitenh <-  as.numeric(pums$RAC1P == 1) # white and not hispanic

#you can choose whatever age groups you want.  3 might be fine here.  We have several sets of age groups that we use
#we often try to expand / collapse age groups until we reliably get significant differences
#AGEP is age
pums$Age18_44 <- as.numeric(pums$AGEP >= 18 & pums$AGEP < 45)
pums$Age45_64 <- as.numeric(pums$AGEP >= 45 & pums$AGEP < 65)
pums$Age65_plus <- as.numeric(pums$AGEP >= 65)


#we don't do analyses by gender for household level analysis 
#because the gender of the reference person is random / not useful information

#now we can calculate the proportion of HOUSEHOLDS (type='h') with household income above the threshold

#here is an example of that calculation that calculates the percent of residents in Forsyth in 2016 that are 18 years old
#the function assumes you are looking at Forsyth in 2016 unless told otherwise
#run this to get a sense of the output of this function

x <- replicate_proportion(pums,"AGEP",18)



#the arguments are defined as: replicate_proportion <- function(
#                 df,numerator_var,numerator_value,denominator_var="Allrecords",denominator_value=1,type='p',
#                 year=2016,county="Forsyth NC",samplespan=1)

#replicate_proportion()'s return function is: return(c(proportion,SE,CV,numerator_population,SE_num,denominator_population,SE_denom))
#so if x <- replicate_proportion(...), then x[1] is the proportion, x[2] is the SE, etc.


#you want to run this function once for each year/county/sample combination, then for each year/county/sample/demographic combination, 
#and put it in a dataframe
#I have uploaded an example output from python that may be helpful
#ideally this dataframe would have the following columns:
#year, county, sample, demographic1, demographic2, demographic3, 
#if you are looking at the overall rate, demographic1-3 would be blank.  
#If you are looking at African Americans, demographic1 would be African American and demographic2-3 would be blank
#If you are looking at Employed African Americans, demographic1 would be African American, demographic2 would be Employed, and demographic3 would be blank
#This is roughly how my PUMS output in python was formatted, so you can look at my old analyses for reference.


#there are again many ways to do this, and you should feel proud of yourself if you can get this to work with mapply or dplyr or something like that
#here however I will outline how to create the dataframe by hand line by line.  This may involve more lines of code and be harder to read, 
#but it should require less experimentation

#I will iterate over all the calculations that are desired, and add rows one by one


#the overall rates should be meaningless (20% each), but this can be used for verification.  It should not be exactly 20% because it isn't the full ACS dataset,
#but for instance if there are far more households in the bottom quintile than expected then this could be a sign that 
#the wrong value was entered, the files didn't merge properly, vacant houses have been somehow included, etc.



#create a blank dataframe
colClasses = c("integer", "character", "integer","character","character", "numeric","numeric","numeric","numeric","numeric","numeric","numeric")
col.names = c("Sample", "County", "Year","Demographic","Quintile", "proportion","SE","CV","numerator_population","SE_num","denominator_population","SE_denom")

df <- read.table(text = "",
                 colClasses = colClasses,
                 col.names = col.names)

#this keeps track of the row you are writing to
i <- 0

for (year in c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016)){
        for (county in c('Forsyth NC','Guilford NC','Pulaski AR','Lafayette Parish LA','Jackson MO')){
                for (demographic in c("Allrecords",'Black','Hispanic','Whitenh','Age18_44','Age45_64','Age65_plus')){
                        for (q in c("Q1","Q2","Q3","Q4","Q5")){
                                i<-i+1
                                
                                #run the analysis and store the results
                                x<-replicate_proportion(pums,q,1,denominator_var=demographic,denominator_value=1,type='h',
                                                        year=year,county=county,samplespan=1)
                                #assign column values
                                df[i,"Sample"]<-1
                                df[i,"County"]<-county
                                df[i,"Year"]<-year
                                df[i,"Demographic"]<-demographic
                                df[i,"Quintile"]<-q
                                df[i,"proportion"]<-x[1]
                                df[i,"SE"]<-x[2]
                                df[i,"CV"]<-x[3]
                                df[i,"numerator_population"]<-x[4]
                                df[i,"SE_num"]<-x[5]
                                df[i,"denominator_population"]<-x[6]
                                df[i,"SE_denom"]<-x[7]
                                
                        }       
                }
        }
}

#Add thresholds to each row according to the quintile 

df <- merge(x = df[,,], y = Peer_Trends[ , c("Year", "County", "Quintile", "Adj_income")])
colnames(df)[colnames(df) == 'Adj_income'] <- 'Thresholds'

