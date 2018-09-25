install.packages("tidycensus")
install.packages("tidyverse")
install.packages("rgdal")
library(tidycensus)
library(tidyverse)


#log file in case this gets too long for the window
now <- Sys.time()
file_name <- paste0(format(now, "%Y%m%d_%H%M%S_"), "education_log.txt")
sink(file_name, type=c("output", "message"),split=TRUE)

variable_list <- load_variables(2017, "acs1", cache = TRUE) 


census_api_key("3d94584bfd87fb43977be01dbddfc797af127c5c") 


counties <- c("Forsyth","Guilford","Durham")
states <- "NC"
year <- 2017
acs_var<-"B01001_001"
varname<-"total"

df <- edu
acs_var <- "B15002_001"
varname <- "Pop"
age1 <- "25"
age2 <- "99"
year <- '2011'
survey <- 'acs1'
get_NC <- 1
get_US <- 1
counties <- c("Forsyth","Guilford","Durham")
print_labels=FALSE
states <- 'NC'
MF<-"A"
race_eth<-"A"


acs_getvar <- function(acs_var,varname,survey = "acs1",year = 2017,
                       get_NC=1,
                       get_US=1,
                       counties=c("Forsyth","Guilford","Durham"),
                       states="NC",
                       age1="00",age2="99",MF="A",race_eth="A",
                       print_labels=TRUE
                       
){
        
        #return a dataframe with 1 row for each county (and NC), along with a measurement and SE for ONE measure.
        
        #get data for each county             
        for (i in 1:length(counties)){
                
                if (i==1){
                        df <- suppressMessages(as.data.frame(get_acs(geography = "county", 
                                                                     variables=(acs_var),
                                                                     #get all vars in tables
                                                                     year = year, #this year
                                                                     state = states, 
                                                                     county = counties[i],
                                                                     geometry = F, # no mapping geometry
                                                                     survey = "acs1",
                                                                     cache_table=T)) )
                }else{
                        tmp <- suppressMessages(as.data.frame(get_acs(geography = "county", 
                                                                      variables=(acs_var),
                                                                      #get all vars in tables
                                                                      year = year, #this year
                                                                      state = states, 
                                                                      county = counties[i],
                                                                      geometry = F, # no mapping geometry
                                                                      survey = "acs1",
                                                                      cache_table=T)))     
                        df[i,] <- tmp[1,]
                        
                }
        }
        
        #add NC data
        
        if(get_NC==1){
                tmp <- suppressMessages(as.data.frame(get_acs(geography = "state", 
                                                              variables=(acs_var),
                                                              #get all vars in tables
                                                              year = year, 
                                                              state = "NC",
                                                              geometry = F, # no mapping geometry
                                                              survey = "acs1",
                                                              cache_table=T))  )   
                df[i+1,] <- tmp[1,]            
        }
        
        #add NC data
        
        if(get_US==1){
                tmp <- suppressMessages(as.data.frame(get_acs(geography = "us", 
                                                              variables=(acs_var),
                                                              #get all vars in tables
                                                              year = year, 
                                                              geometry = F, # no mapping geometry
                                                              survey = "acs1",
                                                              cache_table=T))  )   
                df[i+2,] <- tmp[1,]            
        }
        
        
        #replace missing moes with 0
        #how do other moe errors (e.g. small n) appear?  there needs to be a check for that
        df$moe[is.na(df$moe)] <- 0
        
        #replace moe with SE
        df$SE <- df$moe/1.645
        
        #standardized suffixes
        yr2 <- substr(year,3,4)
        suffix <- paste0(age1,age2,MF,race_eth,yr2)
        
        varname_out <- paste0(varname,suffix)
        varname_out_SE <- paste0(varname,suffix,"_SE")
        
        #rename the resulting variables
        names(df)[names(df) == 'estimate'] <- varname_out
        names(df)[names(df) == 'SE'] <- varname_out_SE
        
        
        
        #remove moe and variable name
        df <- df[c('GEOID','NAME',varname_out,varname_out_SE)]
        
        #verify that you have the right variable
        varlist <- load_variables(year, survey, cache = TRUE)
        varlist17 <- load_variables(2017, survey, cache = TRUE)
        
        fullvarname <- varlist[varlist$name==paste0(acs_var,"E"),c('concept','label')]
        fullvarname17 <- varlist17[varlist17$name==paste0(acs_var,"E"),c('concept','label')]
        
        if(print_labels){
                if(year<2017){
                        print(paste(varname_out,"created based on",toString(fullvarname[1]),toString(fullvarname[2]),"which in 2017 was",toString(fullvarname17[2])))
                }else{print(paste(varname_out,"created based on",toString(fullvarname[1]),toString(fullvarname[2])))}
        }
        
        return(df)
        
        
}

import_acs <- function(acs_var,varname,survey = "acs1",year = 2011,
                       get_NC=1,
                       get_US=1,
                       counties=c("Forsyth","Guilford","Durham"),
                       states="NC",
                       age1="00",age2="99",MF="A",race_eth="A",
                       print_labels=TRUE
){
        #the equivalent to acs_getvar using downloaded excel files for 2011 and prior
        
        
        yr2 <- substr(year,3,4)
        if (nchar(yr2)==1){yr2<-paste0("0",yr2)}
        
        #check whether the data file has been downloaded
        #actually do it differently if it is before 2007 (EST format)
        tablename <- substr(acs_var,1,regexpr("_",acs_var)[1]-1)
        variablename <-substr(acs_var,regexpr("_",acs_var)[1]+1,nchar(acs_var))
        #two digit variable name
        short_variablename <- substr(variablename,2,3)
        
        if (year<=2006){
                filename <- paste0('ACS_',yr2,'_EST_',tablename,'_with_ann.csv')
        }else{filename <- paste0('ACS_',yr2,'_',substr(survey,4,4),'YR_',tablename,'_with_ann.csv')}
        if(file.exists(filename)){
                
                #import the csv file
                tmp <- read.csv(filename,stringsAsFactors = FALSE)
                
                varname_estimate <- paste0("HD01_VD",short_variablename)
                varname_moe <- paste0("HD02_VD",short_variablename)
                
                #get the label
                label <- t(tmp)[varname_estimate,1]
                
                
                #remove the first row which has the variable labels
                tmp <- tmp[-1,]
                
                #just the columns requested
                tmp <- tmp[,c("GEO.id2","GEO.display.label",varname_estimate,varname_moe)]
                
                
                
                #standardized suffixes
                suffix <- paste0(age1,age2,MF,race_eth,yr2)
                
                varname_out <- paste0(varname,suffix)
                varname_out_SE <- paste0(varname,suffix,"_SE")
                
                #rename to be just like the output of acs_getvar
                colnames(tmp) <- c("GEOID","NAME",varname_out,varname_out_SE) 
                
                
                #change the MOE to a SE and reformat variables as numeric
                #replace NA MOEs with 0 (statistically controlled)
                
                tmp[varname_out_SE][is.na(tmp[varname_out_SE])] <- 0
                tmp[varname_out_SE] <- as.numeric(unlist(tmp[varname_out_SE]))/1.645
                
                tmp[varname_out] <- as.numeric(unlist(tmp[varname_out]))
                
                #verify that you have the right variable
                varlist <- load_variables(2017, survey, cache = TRUE)
                fullvarname <- varlist[varlist$name==paste0(acs_var,"E"),c('concept','label')]
                
                if(print_labels){
                        print(paste(varname_out,"created based on",label,"from",filename,"which in 2017 was",fullvarname[2]))
                }
                
                return(tmp)
                
                
        }else{print(paste("The file ",filename,"does not exist."))}
        
        
}




acs_newvar <- function(df,acs_var,varname,survey = "acs1",year = 2017,get_NC=1,get_US=1,
                       counties=c("Forsyth","Guilford","Durham"),
                       states="NC",
                       age1="00",age2="99",MF="A",race_eth="A"){
        #add an ACS variable to an existing df, or create a df with an ACS variable
        
        
        #get population counts if df is not a dataframe
        if (!is.data.frame(df)){
                
                
                df <- acs_getvar("B01001_001","Total Population",survey = survey,year = 2011,
                                 get_NC=get_NC,
                                 get_US=get_US,
                                 counties=counties,
                                 states=states,
                                 print_labels=FALSE
                )
        }
        
        
        #pull variable
        #get_acs doesn't support 2011 and prior, so check whether it can be used
        if(year>=2012){
                
                tmp  <- acs_getvar(acs_var,varname,survey = survey,year = year,
                                   get_NC=get_NC,
                                   get_US=get_US,
                                   counties=counties,
                                   states=states,
                                   age1=age1,age2=age2,MF=MF,race_eth=race_eth
                )
        }else{
                tmp <- import_acs(acs_var,varname,survey = survey,year = year,
                                  get_NC=get_NC,
                                  get_US=get_US,
                                  counties=counties,
                                  states=states,
                                  age1=age1,age2=age2,MF=MF,race_eth=race_eth
                )
                
        }
        
        
        #merge to the base df
        df_out <- merge(df,tmp, by=c('GEOID','NAME'),all.x=TRUE)
        
        
        
        return(df_out)
}





acs_subract <- function(df,var1,var2,varname,suffix="0099AA17"){
        #subtract two variables
        for(var_to_check in c(var1,var2)){
                if(!var_to_check %in% colnames(df))
                {
                        print(paste(var_to_check,"is not in the dataframe.  The columns are:"))
                        print(colnames(df))
                }}
        
        values <- df[[var1]]-df[[var2]]
        SEs <- sqrt(df[[paste(var1,"_SE",sep="")]]^2+df[[paste(var2,"_SE",sep="")]]^2)
        
        df[[varname]] <- values
        df[[paste0(varname,"_SE")]] <- SEs
        
        print(paste(varname,"created by subtracting",var2,"from",var1))
        
        return(df)
}


acs_add <- function(df,var1,var2,varname){
        #adds two variables together
        #I should adapt this to accept a list of variables rather than two variables
        #This should be mostly replaced with acs_add_sumofvars which takes a list of ACS variables to import and sum
        
        #check that the data frames contain the neccessary columns
        for(var_to_check in c(var1,var2)){
                if(!var_to_check %in% colnames(df))
                {
                        print(paste(var_to_check,"is not in the dataframe.  The columns are:"))
                        print(colnames(df))
                }}
        
        values <- df[[var1]]+df[[var2]]
        SEs <- sqrt(df[[paste(var1,"_SE",sep="")]]^2+df[[paste(var2,"_SE",sep="")]]^2)
        
        df[[varname]] <- values
        df[[paste0(varname,"_SE")]] <- SEs
        
        print(paste(varname,"created by adding",var2,"and",var1))
        
        return(df)
}

acs_add_sumofvars <- function(df,varlist,varname,
                              survey = "acs1",year = 2017,get_NC=1,get_US=1,
                              counties=c("Forsyth","Guilford","Durham"),
                              states="NC",
                              age1="00",age2="99",MF="A",race_eth="A"
){
        #adds several variables together; this way none of the intermediary tables appear in the df
        
        #standardized suffixes
        yr2 <- substr(year,3,4)
        suffix <- paste0(age1,age2,MF,race_eth,yr2)
        
        varname_out <- paste0(varname,suffix)
        varname_out_SE <- paste0(varname,suffix,"_SE")
        
        
        #create a temporary df with every variable in varlist
        tmp <- 1
        
        #add the vars
        for (var in varlist){
                tmp <- acs_newvar(tmp,var,var,
                                  survey = survey,year = year,
                                  get_NC=get_NC,
                                  get_US=get_US,
                                  counties=counties,
                                  states=states,
                                  age1=age1,age2=age2,MF=MF,race_eth=race_eth)
        }
        tmp$sum <- 0
        tmp$SE_tmp <-0
        
        #sum values and squared SEs
        for (var in varlist){
                tmp$sum <- tmp$sum+tmp[[paste0(var,suffix)]]
                tmp$SE_tmp <- tmp$SE_tmp+tmp[[paste0(var,suffix,"_SE")]]^2
        }
        
        
        
        #calculate the SE
        tmp[[varname_out_SE]] <- sqrt(tmp$SE_tmp)
        
        tmp[[varname_out]]<-tmp$sum
        
        
        #remove temporary variables
        tmp <- tmp[c('GEOID','NAME',varname_out,varname_out_SE)]
        
        print(paste("The above variables were summed to create",varname_out))
        df_out <- merge(df,tmp, by=c('GEOID','NAME'),all.x=TRUE)
}




acs_proportion <- function(df,var_num,var_denom,varname){
        #subtract two variables
        
        #check that the data frames contain the neccessary columns
        for(var_to_check in c(var_num,var_denom)){
                if(!var_to_check %in% colnames(df))
                {
                        print(paste(var_to_check,"is not in the dataframe.  The columns are:"))
                        print(colnames(df))
                }}
        
        a <- df[[var_num]]
        b <- df[[var_denom]]
        p <- a/b
        
        SEa <- df[[paste0(var_num,"_SE",sep="")]]
        SEb <- df[[paste0(var_denom,"_SE",sep="")]]
        
        
        if((SEa^2+(p^2*SEb^2)) > 0 ){SEs <- (1/b)*sqrt(SEa^2-(p^2*SEb^2))
        }else{SEs <- (1/b)*sqrt(SEa^2+(p^2*SEb^2))}
        #SEs <- (1/b)*sqrt(SEa^2+(p^2*SEb^2))
        
        
        df[[paste0(varname)]] <- p
        df[[paste0(varname,"_SE")]] <- SEs
        
        print(paste(varname,"created by dividing",var_num,"by",var_denom))
        
        return(df)
        
}


edu<-1

#ACS and more intuitive race/ethnic labels
race_eth_label <- c("W","B","H")
race_eth_acs <- c("H","B","I")


for (year in c(2012,2010,2011)){
#for (year in c(2006)){
        
        
        yr2 <- substr(year,3,4)
        
        
        #get totals and gender from B15002
        edu <- acs_newvar(edu,"B15002_001","Pop",age1="25",year=year)
        edu <- acs_newvar(edu,"B15002_002","Pop",age1="25",MF="M",year=year)
        edu <- acs_newvar(edu,"B15002_019","Pop",age1="25",MF="F",year=year)
        edu <- acs_add_sumofvars(edu,c("B15002_014","B15002_015","B15002_016","B15002_017","B15002_018"),"Assoc_p",age1="25",MF="M",year=year)
        edu <- acs_add_sumofvars(edu,c("B15002_031","B15002_032","B15002_033","B15002_034","B15002_035"),"Assoc_p",age1="25",MF="F",year=year)
        
        edu <- acs_add(edu,paste0("Assoc_p2599FA",yr2),paste0("Assoc_p2599MA",yr2),paste0("Assoc_p2599AA",yr2))
        
        edu <- acs_proportion(edu,paste0("Assoc_p2599AA",yr2),paste0("Pop2599AA",yr2),paste0("Pct_Assoc_p2599AA",yr2))
        edu <- acs_proportion(edu,paste0("Assoc_p2599MA",yr2),paste0("Pop2599MA",yr2),paste0("Pct_Assoc_p2599MA",yr2))
        edu <- acs_proportion(edu,paste0("Assoc_p2599FA",yr2),paste0("Pop2599FA",yr2),paste0("Pct_Assoc_p2599FA",yr2))
        
}    
        #race/ethnicity
        
        #in 2008 a separate category for GED was added
        for (i in 1:3){
                edu <- acs_newvar(edu,paste0("B15002",race_eth_acs[i],"_001"),"Pop",age1="25",year=year,race_eth=race_eth_label[i])
                
                if(year>2007){
                        
                        edu <- acs_add_sumofvars(edu,c(paste0("B15002",race_eth_acs[i],"_008"),paste0("B15002",race_eth_acs[i],"_009"),paste0("B15002",race_eth_acs[i],"_010"),paste0("B15002",race_eth_acs[i],"_017"),paste0("B15002",race_eth_acs[i],"_018"),paste0("B15002",race_eth_acs[i],"_019")),"Assoc_p",age1="25",race_eth=race_eth_label[i],year=year)
                }else{
                        edu <- acs_add_sumofvars(edu,c(paste0("B15002",race_eth_acs[i],"_007"),paste0("B15002",race_eth_acs[i],"_008"),paste0("B15002",race_eth_acs[i],"_009"),paste0("B15002",race_eth_acs[i],"_015"),paste0("B15002",race_eth_acs[i],"_016"),paste0("B15002",race_eth_acs[i],"_017")),"Assoc_p",age1="25",race_eth=race_eth_label[i],year=year)
                }
                edu <- acs_proportion(edu,paste0("Assoc_p2599A",race_eth_label[i],yr2),paste0("Pop2599A",race_eth_label[i],yr2),paste0("Pct_Assoc_p2599A",race_eth_label[i],yr2))
        }
        
        
        
        #age
        edu <- acs_add_sumofvars(edu,c("B15001_011","B15001_052"),"Pop",age1="25",age2="34",year=year)
        edu <- acs_add_sumofvars(edu,c("B15001_019","B15001_060"),"Pop",age1="35",age2="44",year=year)
        edu <- acs_add_sumofvars(edu,c("B15001_027","B15001_068"),"Pop",age1="45",age2="64",year=year)
        edu <- acs_add_sumofvars(edu,c("B15001_035","B15001_076"),"Pop",age1="65",age2="99",year=year)
        
        edu <- acs_add_sumofvars(edu,c("B15001_016","B15001_017","B15001_018","B15001_057","B15001_058","B15001_059"),"Assoc_p",age1="25",age2="34",year=year)
        edu <- acs_add_sumofvars(edu,c("B15001_024","B15001_025","B15001_026","B15001_065","B15001_066","B15001_067"),"Assoc_p",age1="35",age2="44",year=year)
        edu <- acs_add_sumofvars(edu,c("B15001_032","B15001_033","B15001_034","B15001_073","B15001_074","B15001_075"),"Assoc_p",age1="45",age2="64",year=year)
        edu <- acs_add_sumofvars(edu,c("B15001_040","B15001_041","B15001_042","B15001_081","B15001_082","B15001_083"),"Assoc_p",age1="65",age2="99",year=year)
        
        edu <- acs_proportion(edu,paste0("Assoc_p2534AA",yr2),paste0("Pop2534AA",yr2),paste0("Pct_Assoc_p2534AA",yr2))
        edu <- acs_proportion(edu,paste0("Assoc_p3544AA",yr2),paste0("Pop3544AA",yr2),paste0("Pct_Assoc_p3544AA",yr2))
        edu <- acs_proportion(edu,paste0("Assoc_p4564AA",yr2),paste0("Pop4564AA",yr2),paste0("Pct_Assoc_p4564AA",yr2))
        edu <- acs_proportion(edu,paste0("Assoc_p6599AA",yr2),paste0("Pop6599AA",yr2),paste0("Pct_Assoc_p6599AA",yr2))
        
}


names <- colnames(edu)

edu_made <- edu

edu_made2017 <- t(edu_made)

write.csv(edu_made2017, "edu_made2017.csv")
