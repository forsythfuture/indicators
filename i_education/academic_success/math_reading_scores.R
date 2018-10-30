edu <- read_csv('i_education/academic_success/data/math_reading_scores.csv')


chisq_2way <- function(measure,year1,county1,group1,year2,county2,group2){
        #takes a two by two matrix and states whether the chisquared is significant
        
        #variable names are short for yes1, no1, yes2, no2
        
        #only continue if the parameters are different in some way
        #if they are the same the function does not do anytying
        if(year1!=year2 | county1!=county2 | group1!=group2){
                
                y1 <- as.integer(edu[edu$measure==measure & edu$year==year1 & edu$group==group1 & edu$county==county1,"passed"])
                n1 <- as.integer(edu[edu$measure==measure & edu$year==year1 & edu$group==group1 & edu$county==county1,"not_passed"])
                y2 <- as.integer(edu[edu$measure==measure & edu$year==year2 & edu$group==group2 & edu$county==county2,"passed"])
                n2 <- as.integer(edu[edu$measure==measure & edu$year==year2 & edu$group==group2 & edu$county==county2,"not_passed"])
                
        #check if the values are NA.  If so, this means one year/group/measure/county combination was invalid
        invalid_comparison <- 0
                
    
    if(is.na(y1)|is.na(n1)){
      print(paste("No cell found with a ",measure,"measurement for",year1,county1,group1))
      invalid_comparison <- 1
    }
    if(is.na(y2)|is.na(n2)){
      print(paste("No cell found with a ",measure,"measurement for",year2,county2,group2))
      invalid_comparison <- 1
    }       
    
    #proceed if there are values to work with
    if(invalid_comparison == 0){
      #make the dataframe
      test1 <- matrix(c(y1,y2,n1,n2),ncol=2,byrow=TRUE)
      
      #the rates (used only for summary text)
      p1 <- y1/(n1+y1)
      p2 <- y2/(n2+y2)
      
      #chi squared results
      chisq_result <- chisq.test(test1)
      #just the p value
      testp <- chisq_result$p.value
      
      #evaluate the rates and p value
      if(testp>.05) {
        result_text <- (paste(group1,"in",county1,year1,"has a similar",measure,"compared to",group2,"in",county2,year2))
      } else if(p1>p2) {
        result_text <- (paste(group1,"in",county1,year1,"has a higher",measure,"than",group2,"in",county2,year2))
        
      } else{result_text <- (paste(group1,"in",county1,year1,"has a lower",measure,"than",group2,"in",county2,year2))}
      
      #return a row  to be appended to the results
      data <- list(year1,county1,group1,year2,county2,group2,y1,n1,p1,y2,n2,p2,testp<=.05,result_text)
      output_df <- as.data.frame(data,col.names=colnames(comparisons))
      
      return(output_df)
      
      #also print the table and full results for double checking if you wish.
      #print(test1)
      #print(chisq_result)
    }
  }
}


#blank data frame for results
comparisons <- data.frame(
  year1=integer(),
  county1=character(),
  group1=character(),
  year2=integer(),
  county2=character(),
  group2=character(),                 
  Yes1=integer(),
  No1=integer(),
  Rate1=double(),
  Yes2=integer(),
  No2=integer(),
  Rate2=double(),
  Significant=logical(),
  Narrative=character(),
  stringsAsFactors=FALSE)

#used to describe results
measure <- "3rd Grade Reading Score"

#list of lists defining what categories to look at
all <- list("ALL")
race_eth <- list('black',"white","hispanic")
gender <- list('male',"female")
EDS <- list('eds',"not_eds")

#currently I have data going back to 2013-14

#do the same set of analyses for every demographic group/year combination
demographics <- list(race_eth,gender,EDS,all)
for (year1 in c("2017-18","2016-17","2015-16","2014-15","2013-14")){
  for (demographic_cat in demographics){
    for (group1 in demographic_cat){
      
      
      #there are three different types of analysis
      
        for (year2 in c("2017-18","2016-17","2015-16","2014-15","2013-14")){
                    #Compare this demographic group to itself across years within Forsyth
                    comparisons <- rbind(comparisons,chisq_2way(measure,year1,"Forsyth",group1,year2,"Forsyth",group1))
        }
      
      for (group2 in demographic_cat){
        #Compare this group to every other group in the same demographic cat in this year
        comparisons <- rbind(comparisons,chisq_2way(measure,year1,"Forsyth",group1,year1,"Forsyth",group2))
      }
      
      for (peer in c("Durham","Guilford")){
      #Compare this group to peers in this year 
        comparisons <- rbind(comparisons,chisq_2way(measure,year1,"Forsyth",group1,year1,peer,group1))
      }
    }
  }
}


for (measure in c("8th Grade Math Score")){
        
        #currently I only have FC demographic data going back to 2014-15 and any dat back to 2013-2014
        
        #do the same set of analyses for every demographic group/year combination
        demographics <- list(race_eth,gender,EDS,all)
        for (year1 in c("2017-18","2016-17","2015-16","2014-15","2013-14")){
                for (demographic_cat in demographics){
                        for (group1 in demographic_cat){
                                
                                
                                #there are three different types of analysis
                                
                                for (year2 in c("2017-18", "2016-17","2015-16","2014-15","2013-14")){
                                        #Compare this demographic group to itself across years within Forsyth
                                        
                                        #don't make demographic comparisons for 2013-14
                                        if (year1 != "2013-14" & year2 != "2013-14"){
                                                comparisons <- rbind(comparisons,chisq_2way(measure,year1,"Forsyth",group1,year2,"Forsyth",group1))
                                        }
                                }
                                
                                for (group2 in demographic_cat){
                                        #Compare this group to every other group in the same demographic cat in this year
                                        
                                        #don't make demographic comparisons for 2013-14
                                        if (year1 != "2013-14"){
                                                comparisons <- rbind(comparisons,chisq_2way(measure,year1,"Forsyth",group1,year1,"Forsyth",group2))
                                        }
                                }
                                
                                for (peer in c("Durham","Guilford")){
                                        
                                        #don't make peer comparisons for 2013-14 except when using overall rates
                                        if (year1 != "2013-14" | group1 == "ALL"){
                                                #Compare this group to peers in this year 
                                                comparisons <- rbind(comparisons,chisq_2way(measure,year1,"Forsyth",group1,year1,peer,group1))
                                        }
                                }
                                
                        }
                }
        }
}


write.csv(comparisons, file ="math_reading_chi_squared_comparisons.csv")


