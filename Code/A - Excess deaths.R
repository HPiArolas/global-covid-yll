###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Excess death counts
##
###################################################################################################
###################################################################################################

# Description:

# Tests the code needed to choose dates to match excess deaths.


# Notes:

# Removing England for now.
# The US does not have the exact date, but close to it: 2020-04-11
# Excess deaths for ages for which there is no convergence are simply missing from the data Enrique provided
# Note that I used a different version of death counts covid clean; I use the one where we keep all dates.

###################################################################################################
##
##  DIRECTORY
##  
##
###################################################################################################

# User defined directory
AL<-FALSE
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared')
}else{setwd("~/Adeline Research Dropbox/Adeline Lo/COVID-19 - YLL - Shared")}

###################################################################################################
##
##  DATA
##  
##
###################################################################################################


# COVID-19 deaths
###################################################################################################
#if(!AL){data.coverage<-readRDS(file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/deathcounts_clean21-5-2020.rds")
#}else{data.coverage<-readRDS(file="Data/Processed/deathcounts_clean21-5-2020.rds")}

if(!AL){data.coverage<-readRDS(file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/out2_07-7-2020.rds")
}else{data.coverage<-readRDS(file="Data/Processed/out2_07-7-2020.rds")}



# Reading as date
data.coverage$Date<-as.Date(data.coverage$Date,"%Y-%m-%d")
countries<-as.character(unique(data.coverage$Country))


# Excess deaths
###################################################################################################
#if(!AL){data.excess<-read.csv(file='/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/covid_excess_pclm_5.csv')
#}else{data.excess<-read.csv(file="Data/Processed/covid_excess_pclm_5.csv")}


if(!AL){data.excess<-read.csv(file='/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/covid_excess_pclm_5_delay_10_07_2020.csv')
}else{data.excess<-read.csv(file="Data/Processed/covid_excess_pclm_5_delay_10_07_2020.csv")}


# Reading as date
data.excess$last_date<-as.Date(data.excess$last_date,"%Y-%m-%d")

# Changing names to mimic our conventions
levels(data.excess$Country)[levels(data.excess$Country)=="AUT"] <- "Austria"
levels(data.excess$Country)[levels(data.excess$Country)=="BEL"] <- "Belgium"
levels(data.excess$Country)[levels(data.excess$Country)=="DEUTNP"] <- "Germany"
levels(data.excess$Country)[levels(data.excess$Country)=="DNK"] <- "Denmark"
levels(data.excess$Country)[levels(data.excess$Country)=="ESP"] <- "Spain"
levels(data.excess$Country)[levels(data.excess$Country)=="FIN"] <- "Finland"
levels(data.excess$Country)[levels(data.excess$Country)=="GBRTENW"] <- "England"
levels(data.excess$Country)[levels(data.excess$Country)=="England and Wales"] <- "England"
levels(data.excess$Country)[levels(data.excess$Country)=="ISL"] <- "Iceland"
levels(data.excess$Country)[levels(data.excess$Country)=="NLD"] <- "Netherlands"
levels(data.excess$Country)[levels(data.excess$Country)=="NOR"] <- "Norway"
levels(data.excess$Country)[levels(data.excess$Country)=="PRT"] <- "Portugal"
levels(data.excess$Country)[levels(data.excess$Country)=="SWE"] <- "Sweden"
levels(data.excess$Country)[levels(data.excess$Country)=="USA"] <- "USA"

#drop levels of countries 
data.excess$Country<-as.character(factor(data.excess$Country))

###################################################################################################
##
##  CHECKING LAST COINCIDING DATE
##  
##
###################################################################################################

# Data frame to store results
dates.comparison<-data.frame(Country=unique(data.excess$Country))
temp.1<-paste0('Last.date.excess')
temp.2<-paste0('First.date.coverage')
temp.3<-paste0('Available')
# Adding the country dates
for (i in 1:length(unique(data.excess$Country))){ 
  print(unique(data.excess$Country)[i])
  dates.comparison[i,temp.1]<-max(data.excess$last_date[which(data.excess$Country==dates.comparison$Country[i])])
  dates.comparison[i,temp.2]<-min(data.coverage$Date[which(as.character(data.coverage$Country)==dates.comparison$Country[i])])
  dates.comparison[i,temp.3]<-with(dates.comparison,
                                   if(is.na(First.date.coverage[i])==TRUE){
                                     dates.comparison[i,temp.3]<-'No'
                                   }else if (Last.date.excess[i]>First.date.coverage[i]){
                                     'Yes'
                                   } else
                                     'No'
                                   )
}

# List of countries for which we have data overall
countries.excess.covered<-with(dates.comparison,
                               as.character(Country[which(Available=='Yes')]))
# Removing England for now
#countries.excess.covered<-countries.excess.covered[countries.excess.covered != "England"]
# Portugal also missing data
#countries.excess.covered<-countries.excess.covered[countries.excess.covered != "Portugal"]
# Estonia also missing data
countries.excess.covered<-countries.excess.covered[countries.excess.covered != "Estonia"]
# Excluding Denmark and Spain due to mismatch in dates
countries.excess.covered<-countries.excess.covered[countries.excess.covered != "Denmark"]
countries.excess.covered<-countries.excess.covered[countries.excess.covered != "Spain"]
countries.excess.covered<-as.character(countries.excess.covered)


# Removing the date with no deaths for Norway -- 2020-04-13
#data.coverage$Norway_empty<-ifelse(data.coverage$Country=='Norway'&(data.coverage$Date=='2020-04-13'|
#                                                                      data.coverage$Date=='2020-04-12')  ,1,0)

#data.coverage<-data.coverage[which(data.coverage$Norway_empty==0),]


###################################################################################################
##
##  SUBSETTING DATA
##  
##
###################################################################################################


# Countries that work well with the dates
# Austria, Belgium, England, Finland, France, Germany, Italy, Norway, Portugal, Sweden, Switzerland

# Exceptions: 
# Countries we cannot use -- 
# Denmark would need to go back to 2020-05-19 
# Spain would need to go back to 2020-05-21	
# Countries that need to be adjusted?
# Israel uses coverage 2020-05-05	hand coded
# Netherlands hand coded 2020-05-31	
# USA hand coded closest date 2020-05-30

# Choosing data from coverage on both genders such that it coincides with excess last date
# 20 age groups per country X 8 countries
coverage.deaths.excess<-data.frame(Country=rep(countries.excess.covered,20),Age=NA)
# Ordering
coverage.deaths.excess<-with(coverage.deaths.excess, coverage.deaths.excess[order(Country),])
# Adding ages in 5 year intervals
coverage.deaths.excess$Age<-rep(seq(0,95,5),length(countries.excess.covered))
# Adding region
coverage.deaths.excess$Region<-'All'

temp.1<-paste0('Date')
temp.2<-paste0('Deaths')
temp.3<-paste0('Excess_Deaths')
temp.4<-paste0('Excess_Deaths.lp')
temp.5<-paste0('Excess_Deaths.up')
for( i in 1:length(coverage.deaths.excess$Country)){
  print(i)
  # getting the excess deaths date
  coverage.deaths.excess[i,temp.1]<-dates.comparison$Last.date.excess[which(as.character(dates.comparison$Country)==
                                                                              as.character(coverage.deaths.excess$Country[i]))]
  # picking deaths from coverage at that date -- Special case US, which has a close date
  if(coverage.deaths.excess$Country[i]=='USA'){
    coverage.deaths.excess[i,temp.2]<- with(data.coverage, Deaths[which(as.character(Country)=='USA'&
                                                           Region=='All'&
                                                           Sex=='b'&
                                                           Age==coverage.deaths.excess$Age[i]&
                                                           Date=='2020-05-30')])
    
    } else if(coverage.deaths.excess$Country[i]=='Netherlands'){
      coverage.deaths.excess[i,temp.2]<- with(data.coverage, Deaths[which(as.character(Country)=='Netherlands'&
                                                             Region=='All'&
                                                             Sex=='b'&
                                                             Age==coverage.deaths.excess$Age[i]&
                                                             Date=='2020-05-31')])
    }else{
    tmp<-subset(data.coverage,as.character(Country)==as.character(coverage.deaths.excess$Country[i])
                        &Region=='All'&Sex=='b'&Age==coverage.deaths.excess$Age[i])                                    
    tmp_date<-tmp$Date[which.min(abs(tmp$Date-coverage.deaths.excess$Date[i]))]
    coverage.deaths.excess[i,temp.2]<-tmp[which(tmp$Date==tmp_date),temp.2]}
  # Now adding the excess deaths
  # if no value, simply give 0, if it has a value, take it
  coverage.deaths.excess[i,temp.3]<- if (length(data.excess$excess[which(
                                          as.character(data.excess$Country)==as.character(coverage.deaths.excess$Country[i])&
                                          data.excess$Sex=='b'&
                                          data.excess$Age==coverage.deaths.excess$Age[i])])==0){
                                          0
                                          }else{
                                          data.excess$excess[which(
                                          as.character(data.excess$Country)==as.character(coverage.deaths.excess$Country[i])&
                                          data.excess$Sex=='b'&
                                          data.excess$Age==coverage.deaths.excess$Age[i])]
                                          }
  # Now adding the excess deaths lp
  # if no value, simply give 0, if it has a value, take it
  coverage.deaths.excess[i,temp.4]<- if (length(data.excess$excess_lp[which(
                                          as.character(data.excess$Country)==as.character(coverage.deaths.excess$Country[i])&
                                          data.excess$Sex=='b'&
                                          data.excess$Age==coverage.deaths.excess$Age[i])])==0){
                                          0
                                        }else{
                                         data.excess$excess_lp[which(
                                          as.character(data.excess$Country)==as.character(coverage.deaths.excess$Country[i])&
                                         data.excess$Sex=='b'&
                                        data.excess$Age==coverage.deaths.excess$Age[i])]
                                        }
  # Now adding the excess deaths up
  # if no value, simply give 0, if it has a value, take it
                                 coverage.deaths.excess[i,temp.5]<- if (length(data.excess$excess_up[which(
                                  as.character(data.excess$Country)==as.character(coverage.deaths.excess$Country[i])&
                                  data.excess$Sex=='b'&
                                  data.excess$Age==coverage.deaths.excess$Age[i])])==0){
                                     0
                                   }else{
                                data.excess$excess_up[which(
                               as.character(data.excess$Country)==as.character(coverage.deaths.excess$Country[i])&
                               data.excess$Sex=='b'&
                                data.excess$Age==coverage.deaths.excess$Age[i])]
                                   }
}


# Saving
#saveRDS(coverage.deaths.excess,file="Data/Processed/matched_excess_deaths.rds")

if(!AL){saveRDS(coverage.deaths.excess,file="Data/Processed/matched_excess_deaths_delayed_10_07_2020.rds")
}else{saveRDS(coverage.deaths.excess,file="Data/Processed/matched_excess_deaths_delayed_10_07_2020.rds")}


# Saving a final table for comparison; dates checked manually
excess_dates_table<-data.frame(Country=countries.excess.covered, 
  Date_excess=dates.comparison$Last.date.excess[which(is.element(dates.comparison$Country,countries.excess.covered))], 
  Date_coverage=
    c(
      #Austria 
      '2020-06-01	',
      #Belgium 
      '2020-04-20',
      #England
      '2020-06-08',      
      #Denmark 
      #'2020-04-27',
      #Finland 
      '2020-06-01',
      #France 
      '2020-05-04',      
      #Germany  
      '2020-05-18',
      #Israel  
      '2020-05-11',
      #Italy  
      '2020-04-20',        
      # Netherlands
      '2020-05-31',
      # Norway 
      '2020-04-14',
      # Portugal 
      '2020-06-01',      
      # Spain
      #'2020-04-27',
      # Sweden
      '2020-06-08',
      # Switzerland
      '2020-06-01',      
      # USA
      '2020-05-30'
    ))

if(!AL){saveRDS(excess_dates_table,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/excess_dates_table_10_07_2020.rds")
}else{saveRDS(excess_dates_table,file="Data/Processed/excess_dates_table_10_07_2020.rds")}





