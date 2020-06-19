###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Final result compilation
##
###################################################################################################
###################################################################################################


# Description:
# Gathers final results for comparison and to aid writing.


# Notes:
# Mind the user defined paths until I can get the package here to work.
# Run this file, then run B - YLL- Covid19 - policy scenarios, then this again. 

###################################################################################################
##
##  LIBRARIES
##  
##
###################################################################################################

library(ggplot2)
library(dplyr)
#library(hrbrthemes)
#library(viridis)
library(wpp2019)
library(stargazer)


###################################################################################################
##
##  DIRECTORY
##  
##
###################################################################################################

AL<-FALSE
# User defined directory
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared')
}else{setwd("/Users/adelinelo/Adeline Research Dropbox/Adeline Lo/COVID-19 - YLL - Shared")}


###################################################################################################
##
##  DATA
##  
##
###################################################################################################

# Lists of countries
###################################################################################################

# Our sample
if(!AL){our_sample<-read.csv('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/full sample list.csv')
}else{our_sample<-read.csv('Data/full sample list.csv')}
our_sample<-as.character(data.frame(our_sample)[,1])
# Sample with male / female data
if(!AL){gender_sample<-read.csv('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/gender_sample.csv')}else{
  gender_sample<-read.csv('Data/gender_sample.csv')}
gender_sample<-as.character(data.frame(gender_sample)[,1])
# Excess sample
excess_sample<-readRDS("Data/Processed/sample.excess.rds")

# Original sample of deaths
###################################################################################################
out<-readRDS("Data/Processed/deathcounts_clean12-6-2020.rds")

# Population
###################################################################################################
if(!AL){pop<-read.csv('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/pop_complete.csv',row.names = 1)
}else{pop<-read.csv('Data/Processed/pop_complete.csv',row.names = 1)}

# YLLs COVID-19
###################################################################################################
YLL.measures_covid19<-readRDS("Data/Processed/YLL.measures.rds")
YLL.measures_covid19_gendered<-YLL.measures_covid19[which(is.element(YLL.measures_covid19$Country,gender_sample)==TRUE&
                                                            YLL.measures_covid19$YLL.gbd.m!=0&
                                                            YLL.measures_covid19$YLL.un.f!=0&
                                                            YLL.measures_covid19$Country!='Estonia'),]
YLL.measures.rate.esp_covid19<-readRDS("Data/Processed/YLL.measures.rate.esp.rds")
YLL.measures.rate.gbd_covid19<-readRDS("Data/Processed/YLL.measures.rate.gbd.rds")
YLL.measures.rate.cpop_covid19<-readRDS("Data/Processed/YLL.measures_rate_cpop.rds")

YLL.b.age<-readRDS("Data/Processed/yll-b-list.rds")
YLL.b.age.all<-readRDS("Data/Processed/yll-b.rds")
YLL.m.age<-readRDS("Data/Processed/yll-m-list.rds")
YLL.m.age.all<-readRDS("Data/Processed/yll-m.rds")
YLL.f.age<-readRDS("Data/Processed/yll-f-list.rds")
YLL.f.age.all<-readRDS("Data/Processed/yll-f.rds")

# YLLs COVID-19 - excess
###################################################################################################
YLL_excess_measures<-readRDS("Data/Processed/YLL_excess_measures.rds")
YLL_excess_measures_rate_esp<-readRDS("Data/Processed/YLL_excess_measures_rate_esp.rds")
YLL_excess_measures_rate_gbd<-readRDS("Data/Processed/YLL_excess_measures_rate_gbd.rds")
YLL_excess_measures_rate_gbd_cpop<-readRDS("Data/Processed/YLL_excess_measures_rate_gbd_cpop.rds")
YLL_excess_list<-readRDS("Data/Processed/yll-excess-list.rds")
excess_dates_table<-readRDS('excess_dates_table.RDS') 


# YLLs COVID-19 - projected
###################################################################################################
YLL_projected_measures<-readRDS("Data/Processed/YLL_projected_measures.rds")
YLL_projected_measures_rate_esp<-readRDS("Data/Processed/YLL_projected_measures_rate_esp.rds")
YLL_projected_measures_rate_gbd<-readRDS("Data/Processed/YLL_projected_measures_rate_gbd.rds")
YLL_projected_measures_rate_gbd_cpop<-readRDS("Data/Processed/YLL_projected_measures_rate_gbd_cpop.rds")
projected_deaths_min_figures<-readRDS("Data/Processed/projected_deaths_min_figures.rds")



# YLLs other causes - transport / heart / substance -- includes alcohol (substance_) / drugs / flu
###################################################################################################
# Transport
YLL.measures_transport<-readRDS("Data/Processed/transport_YLL_measures.rds")
YLL.measures.rate.esp_transport<-readRDS("Data/Processed/transport_YLL_measures_rate_esp.rds")
YLL.measures.rate.gbd_transport<-readRDS("Data/Processed/transport_YLL_measures_rate_gbd.rds")
YLL.measures.rate.cpop_transport<-readRDS("Data/Processed/transport_YLL_measures_rate_cpop.rds")
# heart
YLL.measures_heart<-readRDS("Data/Processed/heart_YLL_measures.rds")
YLL.measures.rate.esp_heart<-readRDS("Data/Processed/heart_YLL_measures_rate_esp.rds")
YLL.measures.rate.gbd_heart<-readRDS("Data/Processed/heart_YLL_measures_rate_gbd.rds")
YLL.measures.rate.cpop_heart<-readRDS("Data/Processed/heart_YLL_measures_rate_cpop.rds")
# drug
YLL.measures_drug<-readRDS("Data/Processed/drug_YLL_measures.rds")
YLL.measures.rate.esp_drug<-readRDS("Data/Processed/drug_YLL_measures_rate_esp.rds")
YLL.measures.rate.gbd_drug<-readRDS("Data/Processed/drug_YLL_measures_rate_gbd.rds")
YLL.measures.rate.cpop_drug<-readRDS("Data/Processed/drug_YLL_measures_rate_cpop.rds")
# flu
YLL.measures_flu<-readRDS("Data/Processed/flu_YLL_measures.rds")
YLL.measures.rate.esp_flu<-readRDS("Data/Processed/flu_YLL_measures_rate_esp.rds")
YLL.measures.rate.gbd_flu<-readRDS("Data/Processed/flu_YLL_measures_rate_gbd.rds")
YLL.measures.rate.cpop_flu<-readRDS("Data/Processed/flu_YLL_measures_rate_cpop.rds")


View(YLL.measures_flu)
###################################################################################################
##
##  PREPARATIONS
##  
##
###################################################################################################


## Deaths both genders
###################################################################################################
deaths.country<-vector("list",length=length(our_sample))
# 
for (i in 1:length(YLL.b.age)){
  deaths.country[[i]]<-data.frame(Country=YLL.b.age[[i]]$Country[1])
  tmp.var<-c('Deaths_b')
  deaths.country[[i]][,tmp.var]<-sum(YLL.b.age[[i]][,'Deaths'])[1]
}
names(deaths.country)<-our_sample
deaths.country.all<-do.call(rbind,deaths.country)

## Deaths per 1 million inhabitants
###################################################################################################

# Total population by country
sumpop<-tapply(pop$Total,pop$Country,sum)
# Deaths by 1M
deaths.country.all$Deaths_per_million<-(deaths.country.all$Deaths_b/sumpop)*1000000


## Age distribution of deaths per country -- both
###################################################################################################
# Storing only relevant columns
results.deaths.age.country<-vector("list",length=length(our_sample))
# 
for (i in 1:length(YLL.b.age)){
  results.deaths.age.country[[i]]<-data.frame(Country=YLL.b.age[[i]]$Country)
  tmp.var<-c('Age','Deaths')
  results.deaths.age.country[[i]][,tmp.var]<-YLL.b.age[[i]][,tmp.var]
  for(j in 1:length(YLL.b.age[[1]]$Age)){
    # proportion up to that point of YLL
    tmp.var2<-paste('Deaths_b_cum')
    results.deaths.age.country[[i]][j,tmp.var2]<-sum(YLL.b.age[[i]][1:j,'Deaths'])/sum(YLL.b.age[[i]][,'Deaths'],na.rm=TRUE)
  }
}

## Age distribution of deaths per country -- male
###################################################################################################
results.deaths.age.country.male<-vector("list",length=length(gender_sample))
# 
for (i in 1:length(YLL.m.age)){
  results.deaths.age.country.male[[i]]<-data.frame(Country=YLL.m.age[[i]]$Country)
  tmp.var<-c('Age','Deaths')
  results.deaths.age.country.male[[i]][,tmp.var]<-YLL.m.age[[i]][,tmp.var]
  for(j in 1:length(YLL.m.age[[1]]$Age)){
    # proportion up to that point of YLL
    tmp.var2<-paste('Deaths_b_cum')
    results.deaths.age.country.male[[i]][j,tmp.var2]<-round(sum(YLL.m.age[[i]][1:j,'Deaths'])/sum(YLL.m.age[[i]][,'Deaths'],na.rm=TRUE),2)
  }
}
names(results.deaths.age.country.male)<-gender_sample
results.deaths.age.country.male.all<-do.call(rbind,results.deaths.age.country.male)



## Age distribution of deaths per country -- female
###################################################################################################
results.deaths.age.country.female<-vector("list",length=length(gender_sample))
# 
for (i in 1:length(YLL.f.age)){
  results.deaths.age.country.female[[i]]<-data.frame(Country=YLL.f.age[[i]]$Country)
  tmp.var<-c('Age','Deaths')
  results.deaths.age.country.female[[i]][,tmp.var]<-YLL.f.age[[i]][,tmp.var]
  for(j in 1:length(YLL.f.age[[1]]$Age)){
    # proportion up to that point of YLL
    tmp.var2<-paste('Deaths_b_cum')
    results.deaths.age.country.female[[i]][j,tmp.var2]<-round(sum(YLL.f.age[[i]][1:j,'Deaths'])/sum(YLL.f.age[[i]][,'Deaths'],na.rm=TRUE),2)
  }
}
names(results.deaths.age.country.female)<-gender_sample
results.deaths.age.country.female.all<-do.call(rbind,results.deaths.age.country.female)


## Total deaths by country - male, female
###################################################################################################
# First, sum each country
total.deaths.country<-vector("list",length=length(gender_sample))
for (i in 1:length(YLL.f.age)){
  total.deaths.country[[i]]<-data.frame(Country=YLL.f.age[[i]]$Country[1])
  # male deaths
  tmp.var<-c('Male_deaths')
  total.deaths.country[[i]][,tmp.var]<-sum(results.deaths.age.country.male[[i]]$Deaths)
  tmp.var<-c('Female_deaths')
  total.deaths.country[[i]][,tmp.var]<-sum(results.deaths.age.country.female[[i]]$Deaths)
  tmp.var<-c('Male_female_ratio')
  total.deaths.country[[i]][,tmp.var]<-total.deaths.country[[i]][,'Male_deaths']/total.deaths.country[[i]][,'Female_deaths']
}
names(total.deaths.country)<-gender_sample
total.deaths.country.all<-do.call(rbind,total.deaths.country)
# Total deaths
total.deaths.country.all$total_deaths<-total.deaths.country.all$Male_deaths+total.deaths.country.all$Female_deaths


## YLL lost per death gendered
###################################################################################################

#use: total.deaths.country.all male/female deaths per country
#divide YLL (YLL.un.m,YLL.un.f) from measures by total.deaths.country.all
res <-subset(YLL.measures_covid19,Country%in%gender_sample)#subset by countries in gender_sample
#check countries match otherwise throw error
if(!all(res$Country==total.deaths.country.all$Country[which(is.element(as.character(res$Country),as.character(gender_sample))==TRUE)]))
  {print("STOP! Countries don't match perfectly.")}
YLLperDeath_bysex<-data.frame(Country=res$Country
                              ,YLLperDeath_m=res$YLL.un.m/total.deaths.country.all$Male_deaths
                              ,YLLperDeath_f=res$YLL.un.f/total.deaths.country.all$Female_deaths)

## YLL by age -- both genders
###################################################################################################

# Storing only relevant columns
results.YLL.b.age.country<-vector("list",length=length(our_sample))
# 
for (i in 1:length(YLL.b.age)){
  results.YLL.b.age.country[[i]]<-data.frame(Country=YLL.b.age[[i]]$Country)
  tmp.var<-c('Age','Deaths','YLL.gbd.b','YLL.un.b')
  results.YLL.b.age.country[[i]][,tmp.var]<-YLL.b.age[[i]][,tmp.var]
  for(j in 1:length(YLL.b.age[[1]]$Age)){
    # proportion up to that point of YLL
    tmp.var2<-paste('YLL.gbd.b.cum')
    tmp.var3<-paste('YLL.un.b.cum')
    results.YLL.b.age.country[[i]][j,tmp.var2]<-sum(YLL.b.age[[i]][1:j,'YLL.gbd.b'])/sum(YLL.b.age[[i]][,'YLL.gbd.b'],na.rm=TRUE)
    results.YLL.b.age.country[[i]][j,tmp.var3]<-sum(YLL.b.age[[i]][1:j,'YLL.un.b'])/sum(YLL.b.age[[i]][,'YLL.un.b'],na.rm=TRUE)
    
  }
}

## YLL by age -- males and females
###################################################################################################

# Storing only relevant columns
results.YLL.gender.age.country<-vector("list",length=length(gender_sample))
# 
for (i in 1:length(YLL.m.age)){
  # males
  results.YLL.gender.age.country[[i]]<-data.frame(Country=YLL.m.age[[i]]$Country)
  tmp.var<-c('Age','Deaths','YLL.un.m')
  tmp.var2<-c('Age','Deaths_m','YLL.un.m')
  results.YLL.gender.age.country[[i]][,tmp.var2]<-YLL.m.age[[i]][,tmp.var]
  for(j in 1:length(YLL.m.age[[1]]$Age)){
    # proportion up to that point of YLL
    tmp.var3<-paste('YLL.un.m.cum')
    results.YLL.gender.age.country[[i]][j,tmp.var3]<-sum(YLL.m.age[[i]][1:j,'YLL.un.m'])/sum(YLL.m.age[[i]][,'YLL.un.m'],na.rm=TRUE)
}
}

for (i in 1:length(YLL.m.age)){   
  for(j in 1:length(YLL.m.age[[1]]$Age)){
  # females  
  tmp.var<-c('Deaths','YLL.un.f')
  tmp.var2<-c('Deaths_f','YLL.un.f')
  results.YLL.gender.age.country[[i]][,tmp.var2]<-YLL.f.age[[i]][,tmp.var]
    # proportion up to that point of YLL
    tmp.var3<-paste('YLL.un.f.cum')
    results.YLL.gender.age.country[[i]][j,tmp.var3]<-sum(YLL.f.age[[i]][1:j,'YLL.un.f'])/sum(YLL.f.age[[i]][,'YLL.un.f'],na.rm=TRUE)
}
}
names(results.YLL.gender.age.country)<-gender_sample
results.YLL.gender.age.country.all<-do.call(rbind,results.YLL.gender.age.country)

###################################################################################################
##
##  YLL covid19
##  And related measures
##
###################################################################################################

# YLL absolute for both genders by country
# Total YLL lost
# YLL per death total and by country
# YLL rates for both genders and by country

YLL.covid.by_country<-data.frame(
                                      Country=YLL.measures_covid19$Country,
                                      Date=YLL.measures_covid19$Date.death,
                                      YLL.b_abs=YLL.measures_covid19$YLL.un.b,
                                      YLL.b_rates=YLL.measures.rate.cpop_covid19$YLL.un.b,
                                      YLL.m_rates=YLL.measures.rate.cpop_covid19$YLL.un.m,
                                      YLL.f_rates=YLL.measures.rate.cpop_covid19$YLL.un.f,
                                      Deaths=deaths.country.all$Deaths_b)
# Repacing gender data with 0s for NAs
YLL.covid.by_country$YLL.m_rates[YLL.covid.by_country$YLL.m_rates==0]<-NA
YLL.covid.by_country$YLL.f_rates[YLL.covid.by_country$YLL.f_rates==0]<-NA

## Adding gender deaths
###################################################################################################
for (i in 1:length(YLL.covid.by_country$Country)){
  tmp.var<-c('Deaths_f')
  # if we do have data for male / female, fill it, else use NA
  # male
  tmp.var<-c('Deaths_m')
  YLL.covid.by_country[i,tmp.var]<-if(length( 
                 total.deaths.country.all$Male_deaths[which(as.character(total.deaths.country.all$Country)==
                                               as.character(YLL.covid.by_country$Country[i]))])==0){
    NA
  }else{
    total.deaths.country.all$Male_deaths[which(as.character(total.deaths.country.all$Country)==
                                                 as.character(YLL.covid.by_country$Country[i]))]
  }
  # female
  tmp.var<-c('Deaths_f')
  YLL.covid.by_country[i,tmp.var]<-if(length( 
                  total.deaths.country.all$Female_deaths[which(as.character(total.deaths.country.all$Country)==
                  as.character(YLL.covid.by_country$Country[i]))])==0){
                  NA
                      }else{
                  total.deaths.country.all$Female_deaths[which(as.character(total.deaths.country.all$Country)==
                  as.character(YLL.covid.by_country$Country[i]))]
                      }

}

## YLL per death
###################################################################################################    
    
for (i in 1:length(YLL.covid.by_country$Country)){
  # overall
  tmp.var<-c('YLL_per_death_b')
  YLL.covid.by_country[i,tmp.var]<-YLL.covid.by_country$YLL.b_abs[i]/YLL.covid.by_country$Deaths[i]
  # males
  tmp.var<-c('YLL_per_death_males')
  YLL.covid.by_country[i,tmp.var]<-if(length(YLLperDeath_bysex$YLLperDeath_m[which(as.character(
                                        YLL.covid.by_country$Country[i])==as.character(YLLperDeath_bysex$Country))])==0){
                                    NA
                                    }else{
                                    YLLperDeath_bysex$YLLperDeath_m[which(as.character(
                                        YLL.covid.by_country$Country[i])==as.character(YLLperDeath_bysex$Country))] 
                                    }
  # females
  tmp.var<-c('YLL_per_death_females')
  YLL.covid.by_country[i,tmp.var]<-if(length(YLLperDeath_bysex$YLLperDeath_f[which(as.character(
    YLL.covid.by_country$Country[i])==as.character(YLLperDeath_bysex$Country))])==0){
    NA
  }else{
    YLLperDeath_bysex$YLLperDeath_f[which(as.character(
      YLL.covid.by_country$Country[i])==as.character(YLLperDeath_bysex$Country))] 
  }
}



## YLL rates male to female
###################################################################################################  

for (i in 1:length(YLL.covid.by_country$Country)){
  tmp.var<-c('YLL_rates_male_to_female')
  YLL.covid.by_country[i,tmp.var]<-YLL.covid.by_country$YLL.m_rates[i]/YLL.covid.by_country$YLL.f_rates[i]
}    

## Initial guess
###################################################################################################   

# Weighting gendered deaths per male by the inverse of YLL rates male to female leads to equalizing YLL rates.

initial_guess<-data.frame(Country=total.deaths.country.all$Country,
                          Equal_prop=1/YLL.covid.by_country$YLL_rates_male_to_female[which(is.element(YLL.covid.by_country$Country,gender_sample))],
                          YLL_rates_male_to_female=YLL.covid.by_country$YLL_rates_male_to_female[which(is.element(YLL.covid.by_country$Country,gender_sample))],
                          Ratio_deaths=total.deaths.country.all$Male_female_ratio,
                          Ratio_YLL_per_death=YLL.covid.by_country$YLL_per_death_males[which(is.element(YLL.covid.by_country$Country,gender_sample))]/
                            YLL.covid.by_country$YLL_per_death_females[which(is.element(YLL.covid.by_country$Country,gender_sample))],
                          Total_deaths=total.deaths.country.all$total_deaths
                          )

# Average of correction needed, weighted by total number of deaths

for (i in 1:nrow(initial_guess)){
  tmp.var<-c('death_weight')
  initial_guess[i,'death_weight']<-initial_guess[i,'Total_deaths']/sum(initial_guess[,'Total_deaths'])
}

#weighted.mean(initial_guess$Equal_prop, initial_guess$death_weight,na.rm=TRUE)
#mean(initial_guess$Equal_prop,na.rm=TRUE)


###################################################################################################
##
##  Age distributions
##  Of YLL and deaths
##
###################################################################################################

## Gender totals
###################################################################################################
results.gender.total<-data.frame(Value='years_lost')
tmp.var<-c( "YLL.gbd.b"  ,  "YLL.un.b",
            "YLL.gbd.m"  ,  "YLL.un.m",
            "YLL.gbd.f"  ,  "YLL.un.f")
results.gender.total[,tmp.var]<-colSums(YLL.measures_covid19_gendered[, tmp.var],na.rm=TRUE)


Age_distribution<-vector("list",length=length(our_sample))


## Age distribution of deaths both and genders
################################################################################################### 

for (i in 1:length(YLL.b.age)){
  Age_distribution[[i]]<-data.frame(Country=YLL.b.age[[i]]$Country)
  tmp.var<-c('Age')
  Age_distribution[[i]][,tmp.var]<-YLL.b.age[[1]]$Age
  tmp.var<-c('Deaths_both')
  Age_distribution[[i]][,tmp.var]<-YLL.b.age[[i]]$Deaths
  tmp.var<-c('Deaths_m')
  Age_distribution[[i]][,tmp.var]<-if(nrow(subset(YLL.m.age.all,Country==as.character(Age_distribution[[i]]$Country),Deaths))==0){
                                    rep(NA,20)
                                     }else{
                                    subset(YLL.m.age.all,Country==as.character(Age_distribution[[i]]$Country),Deaths) 
                                     }
  tmp.var<-c('Deaths_f')
  Age_distribution[[i]][,tmp.var]<-if(nrow(subset(YLL.f.age.all,Country==as.character(Age_distribution[[i]]$Country),Deaths))==0){
    rep(NA,20)
  }else{
    subset(YLL.f.age.all,Country==as.character(Age_distribution[[i]]$Country),Deaths) 
  }
  
 }
    
## Age distribution of YLL -- all genders
################################################################################################### 

for (i in 1:length(YLL.b.age)){
  # both genders
  tmp.var<-c('YLL_b','YLL_b_cum')
  tmp.var2<-c('YLL.un.b','YLL.un.b.cum')
  Age_distribution[[i]][,tmp.var]<-results.YLL.b.age.country[[i]][,tmp.var2]
  # males
  tmp.var<-c('YLL.un.m')
  Age_distribution[[i]][,tmp.var]<-if(nrow(subset(results.YLL.gender.age.country.all,Country==
                                                  as.character(Age_distribution[[i]]$Country),YLL.un.m))==0){
                                  rep(NA,20)
                                  }else{
                                  subset(results.YLL.gender.age.country.all,Country==as.character(Age_distribution[[i]]$Country),YLL.un.m)}
  tmp.var<-c('YLL.un.m.cum')
  Age_distribution[[i]][,tmp.var]<-if(nrow(subset(results.YLL.gender.age.country.all,Country==
                                                    as.character(Age_distribution[[i]]$Country),YLL.un.m.cum))==0){
                                  rep(NA,20)
                                  }else{
                                  subset(results.YLL.gender.age.country.all,Country==as.character(Age_distribution[[i]]$Country),YLL.un.m.cum)}
  # females
  tmp.var<-c('YLL.un.f')
  Age_distribution[[i]][,tmp.var]<-if(nrow(subset(results.YLL.gender.age.country.all,Country==
                                                  as.character(Age_distribution[[i]]$Country),YLL.un.f))==0){
                                  rep(NA,20)
                                  }else{
                                  subset(results.YLL.gender.age.country.all,Country==as.character(Age_distribution[[i]]$Country),YLL.un.f)}
  tmp.var<-c('YLL.un.f.cum')
  Age_distribution[[i]][,tmp.var]<-if(nrow(subset(results.YLL.gender.age.country.all,Country==
                                                  as.character(Age_distribution[[i]]$Country),YLL.un.f.cum))==0){
                                  rep(NA,20)
                                  }else{
                                  subset(results.YLL.gender.age.country.all,Country==as.character(Age_distribution[[i]]$Country),YLL.un.f.cum)}      
}

# Fixing Pakistan
Age_distribution[[29]]<-Age_distribution[[29]][1:20,]


## Average age at which people die from covid, by males and females
###################################################################################################
#map age intervals to middle of age as an interval
age_at_intervals<-(seq(0,95,5)+seq(5,100,5))/2
#YLL.m.age.all, YLL.f.age.all
avg_age_death_bysex<-vector("list",length(gender_sample))#data.frame(Countries=gender_sample)
for(i in 1:length(gender_sample)){
  print(i)
  m<-subset(YLL.m.age.all,Country==gender_sample[i])
  f<-subset(YLL.f.age.all,Country==gender_sample[i])
  Avg_Age_Death_m <- sum(m$Deaths*age_at_intervals)/sum(m$Deaths)#Var_Age_Death_m<-sum((age_at_intervals-Avg_Age_Death_m)^2*(m$Deaths/sum(m$Deaths)))
  SD_Age_Death_m <- sqrt(sum(m$Deaths*(age_at_intervals - Avg_Age_Death_m)^2)/sum(m$Deaths)) #number of deaths at each interval * (Age of the interval - average death)^2
  Avg_Age_Death_f<-sum(f$Deaths*age_at_intervals)/sum(f$Deaths)
  SD_Age_Death_f <- sqrt(sum(f$Deaths*(age_at_intervals - Avg_Age_Death_f)^2)/sum(f$Deaths))
  avg_age_death_bysex[[i]]<-data.frame(Country=gender_sample[i],Avg_Age_Death_m=Avg_Age_Death_m, SD_Age_Death_m=SD_Age_Death_m
                                       ,Avg_Age_Death_f=Avg_Age_Death_f,SD_Age_Death_f=SD_Age_Death_f)
}
avgsd_age_death_bysex_all<-do.call(rbind,avg_age_death_bysex)
#for every country, get country info from YLL and get `Deaths` variable; sum(multiply by the age_at_intervals)/sum(Deaths)


## Average age at which people die from covid, both genders
###################################################################################################
#map age intervals to middle of age as an interval
age_at_intervals<-(seq(0,95,5)+seq(5,100,5))/2
#YLL.m.age.all, YLL.f.age.all
avgsd_age_death_both<-vector("list",length(our_sample))#data.frame(Countries=gender_sample)
for(i in 1:length(our_sample)){
  print(i)
  b<-subset(YLL.b.age.all,Country==our_sample[i])
  Avg_Age_Death_b <- sum(b$Deaths*age_at_intervals)/sum(b$Deaths)#Var_Age_Death_m<-sum((age_at_intervals-Avg_Age_Death_m)^2*(m$Deaths/sum(m$Deaths)))
  SD_Age_Death_b <- sqrt(sum(b$Deaths*(age_at_intervals - Avg_Age_Death_b)^2)/sum(b$Deaths)) #number of deaths at each interval * (Age of the interval - average death)^2
  avg_age_death_bysex[[i]]<-data.frame(Country=our_sample[i],Avg_Age_Death_b=Avg_Age_Death_b, SD_Age_Death_b=SD_Age_Death_b)
}
avgsd_age_death_both<-do.call(rbind,avg_age_death_bysex)

## Merging
###################################################################################################

avgsd_age_death_all<-merge(avgsd_age_death_both,avgsd_age_death_bysex_all,by='Country',all.x=TRUE)

## Age distribution of deaths -- global
###################################################################################################

# First, summing across countries to get global deaths by age
Age_distribution_all<-do.call(rbind,Age_distribution)
ages_at_death_global<-data.frame(Age=Age_distribution[[1]]$Age)

for (i in 1:length(ages_at_death_global$Age)){
  ages_at_death_global$Deaths[i]<-sum(Age_distribution_all$Deaths_both[which(Age_distribution_all$Age==
                                                                           ages_at_death_global$Age[i] )])
}
# Then, computing mean and variance
avgsd_age_death_both_global<-data.frame(Value='values')#data.frame(Countries=gender_sample)
avgsd_age_death_both_global$Avg_Age_Death_b <- sum(ages_at_death_global$Deaths*age_at_intervals)/
                                            sum(ages_at_death_global$Deaths)#Var_Age_Death_m<-sum((age_at_intervals-Avg_Age_Death_m)^2*(m$Deaths/sum(m$Deaths)))
avgsd_age_death_both_global$SD_Age_Death_b <- sqrt(sum(ages_at_death_global$Deaths*(age_at_intervals - avgsd_age_death_both_global$Avg_Age_Death_b)^2)/
                                            sum(ages_at_death_global$Deaths)) #number of deaths at each interval * (Age of the interval - average death)^2

# Adding genders
## Deaths per age
# Males
for (i in 1:length(ages_at_death_global$Age)){
  ages_at_death_global$Deaths_m[i]<-sum(Age_distribution_all$Deaths_m[which(Age_distribution_all$Age==
                                                                               ages_at_death_global$Age[i] )],na.rm=TRUE)
}
# Females
for (i in 1:length(ages_at_death_global$Age)){
  ages_at_death_global$Deaths_f[i]<-sum(Age_distribution_all$Deaths_f[which(Age_distribution_all$Age==
                                                                              ages_at_death_global$Age[i] )],na.rm=TRUE)
}
## Means and variances
# Males
avgsd_age_death_both_global$Avg_Age_Death_m <- sum(ages_at_death_global$Deaths_m*age_at_intervals)/
                                               sum(ages_at_death_global$Deaths_m)
avgsd_age_death_both_global$SD_Age_Death_m <- sqrt(sum(ages_at_death_global$Deaths_m*(age_at_intervals - avgsd_age_death_both_global$Avg_Age_Death_m)^2)/
                                                     sum(ages_at_death_global$Deaths_m)) 
# Females
avgsd_age_death_both_global$Avg_Age_Death_f <- sum(ages_at_death_global$Deaths_f*age_at_intervals)/
  sum(ages_at_death_global$Deaths_f)
avgsd_age_death_both_global$SD_Age_Death_f <- sqrt(sum(ages_at_death_global$Deaths_f*(age_at_intervals - avgsd_age_death_both_global$Avg_Age_Death_f)^2)/
                                                     sum(ages_at_death_global$Deaths_f)) 
###################################################################################################
##
##  Age distributions cut offs
##  Of YLL and deaths
##
###################################################################################################

## Mass at age cut offs by country
###################################################################################################

results.YLL.b.cutt_off.country<-data.frame(Country=unique(YLL.b.age.all$Country))

for (i in 1:length(results.YLL.b.cutt_off.country$Country)){
  tmp.var<-paste('cut55')
  results.YLL.b.cutt_off.country[i,tmp.var]<-results.YLL.b.age.country[[i]]$YLL.un.b.cum[which(
    results.YLL.b.age.country[[i]]$Age==50)][1]
  tmp.var<-paste('cut75')
  results.YLL.b.cutt_off.country[i,tmp.var]<-results.YLL.b.age.country[[i]]$YLL.un.b.cum[which(
    results.YLL.b.age.country[[i]]$Age==75)][1]-results.YLL.b.cutt_off.country[i,'cut55']
  tmp.var<-paste('cut95')
  results.YLL.b.cutt_off.country[i,tmp.var]<-1-results.YLL.b.age.country[[i]]$YLL.un.b.cum[which(
    results.YLL.b.age.country[[i]]$Age==75)][1]
}  


## YLL by ages total -- "gobal" sum
###################################################################################################
# Frame to store
results.YLL.b.age.total<-data.frame(Age=YLL.b.age[[1]]$Age)
# Collecting results from all countries
for (i in 1:length(results.YLL.b.age.total$Age)){
  tmp.var<-paste('YLL.gbd.b')
  results.YLL.b.age.total[i,tmp.var]<-sum(YLL.b.age.all$YLL.gbd.b[which(YLL.b.age.all$Age==results.YLL.b.age.total$Age[i])],na.rm=TRUE)
  tmp.var<-paste('YLL.un.b')
  results.YLL.b.age.total[i,tmp.var]<-sum(YLL.b.age.all$YLL.un.b[which(YLL.b.age.all$Age==results.YLL.b.age.total$Age[i])],na.rm=TRUE)
}
# Loop within the results to sum
for (i in 1:length(results.YLL.b.age.total$Age)){
  tmp.var<-paste('YLL.gbd.b.cum')
  results.YLL.b.age.total[i,tmp.var]<-sum(results.YLL.b.age.total[(1:i),'YLL.gbd.b'])/sum(results.YLL.b.age.total[,'YLL.gbd.b'],na.rm=TRUE)
  tmp.var<-paste('YLL.un.b.cum')
  results.YLL.b.age.total[i,tmp.var]<-sum(results.YLL.b.age.total[(1:i),'YLL.un.b'])/sum(results.YLL.b.age.total[,'YLL.un.b'],na.rm=TRUE)
}

## Mass at age cut offs global
###################################################################################################

results.YLL.b.cutt_off.global<-data.frame(Value='YLL_proportion')

  tmp.var<-paste('cut55')
  results.YLL.b.cutt_off.global[1,tmp.var]<-results.YLL.b.age.total$YLL.un.b.cum[which(
    results.YLL.b.age.total$Age==50)][1]
  tmp.var<-paste('cut75')
  results.YLL.b.cutt_off.global[1,tmp.var]<-results.YLL.b.age.total$YLL.un.b.cum[which(
    results.YLL.b.age.total$Age==75)]-results.YLL.b.cutt_off.global[1,'cut55']
  tmp.var<-paste('cut95')
  results.YLL.b.cutt_off.global[1,tmp.var]<-1-results.YLL.b.age.total$YLL.un.b.cum[which(
    results.YLL.b.age.total$Age==75)]

###################################################################################################
##
##  Other causes
##  
##
###################################################################################################

## Grand totals: all countries
###################################################################################################

results.oc.country<-data.frame(Country=unique(YLL.measures_transport$Country))
tmp.var1<-c("covid_YLL.un.b")
tmp.var2<-c("YLL.un.b")
results.oc.country[,tmp.var1]<-YLL.measures_covid19[,tmp.var2][which(is.element(YLL.measures_covid19$Country,YLL.measures_transport$Country))]
tmp.var3<-c("transport_YLL.un.b")
tmp.var4<-c("YLL.un.b")
results.oc.country[,tmp.var3]<-YLL.measures_transport[,tmp.var4]
tmp.var5<-c("heart_YLL.un.b")
tmp.var6<-c("YLL.un.b")
results.oc.country[,tmp.var5]<-YLL.measures_heart[,tmp.var6]
tmp.var7<-c("drug_YLL.un.b")
tmp.var8<-c("YLL.un.b")
results.oc.country[,tmp.var7]<-YLL.measures_drug[,tmp.var8]
tmp.var9<-c("flu_YLL.un.b_med","flu_YLL.un.b_max")
tmp.var10<-c("YLL.un.b_med","YLL.un.b_max")
results.oc.country[,tmp.var9]<-YLL.measures_flu[,tmp.var10] 

## Grand totals: all countries -- adding rates
###################################################################################################


tmp.var<-c("YLL.rate.un.b_cpop")
tmp.var2<-c("transport_YLL.rate.un.b_cpop")
results.oc.country[,tmp.var2]<-YLL.measures.rate.cpop_transport[,tmp.var]
tmp.var2<-c("heart_YLL.rate.un.b_cpop")
results.oc.country[,tmp.var2]<-YLL.measures.rate.cpop_heart[,tmp.var]
tmp.var2<-c("drug_YLL.rate.un.b_cpop")
results.oc.country[,tmp.var2]<-YLL.measures.rate.cpop_drug[,tmp.var]
tmp.var2<-c("flu_YLL.rate.un.med.b_cpop")
results.oc.country[,tmp.var2]<-YLL.measures.rate.cpop_flu[,'YLL.un.b_med_cpop']
tmp.var2<-c("flu_YLL.rate.un.max.b_cpop")
results.oc.country[,tmp.var2]<-YLL.measures.rate.cpop_flu[,'YLL.un.b_max_cpop']



###################################################################################################
##
##  Projections
##  
##
###################################################################################################

## Projections: grand totals
###################################################################################################
results.proj.total<-data.frame(Value='years_lost')
tmp.var<-c('YLL.un.b_min', 'YLL.un.b_max')
results.proj.total[,tmp.var]<-colSums(YLL_projected_measures[, tmp.var],na.rm=TRUE)

## Projections: country by country
###################################################################################################
results.proj.country<-data.frame(Country=unique(YLL_projected_measures$Country))
# Projections
tmp.var<-c('YLL.un.b_min', 'YLL.un.b_max')
results.proj.country[,tmp.var]<-YLL_projected_measures[,tmp.var]

## Projections: adding rates to the projections
###################################################################################################

# This is simply dividing by country specific populations

sumpop<-tapply(pop$Total,pop$Country,sum)

# Projections
tmp.var<-c('YLL.rates.un.b_min', 'YLL.rates.un.b_max')
tmp.var2<-c('YLL.un.b_min', 'YLL.un.b_max')
results.proj.country[,tmp.var]<-(results.proj.country[,tmp.var2]/sumpop)*100000

## Projections: adding values for real deaths
###################################################################################################

results.proj.country<-data.frame(
                        Country=results.proj.country$Country,
                        Date=YLL.covid.by_country$Date,
                        YLL.b_abs=YLL.covid.by_country$YLL.b_abs,
                        YLL.b.proj_min=results.proj.country$YLL.un.b_min , 
                        YLL.b.proj_max=results.proj.country$YLL.un.b_max,
                        YLL.b_rates=YLL.covid.by_country$YLL.b_rates,
                        YLL.rates.b_min=results.proj.country$YLL.rates.un.b_min,
                        YLL.rates.b_max=results.proj.country$YLL.rates.un.b_max)
                        

results.proj.country$YLL.rates.ratio.current_projected_min<-results.proj.country$YLL.rates.b_min/results.proj.country$YLL.b_rates

# Adding deaths projected
results.proj.country$project_deaths<-projected_deaths_min_figures

###################################################################################################
##
##  Excess
##  
##
###################################################################################################

## Excess: adding values for real deaths
###################################################################################################

# Keep the country naming out of the loop
YLL.comparison_excess<-data.frame(Country=YLL_excess_measures$Country)
# Adding deaths
for (i in 1:length(excess_sample)){
  tmp.var<-c('Excess_deaths')
  YLL.comparison_excess[i,tmp.var]<-sum(YLL_excess_list[[i]][,'Excess_Deaths'])
  tmp.var<-c('Deaths')
  YLL.comparison_excess[i,tmp.var]<-sum(YLL_excess_list[[i]][,'Deaths'])
}
# Adding dates
YLL.comparison_excess$Dates_excess<-YLL_excess_measures$Date
# Adding absolute measures
YLL.comparison_excess$YLL_b_excess_deaths<-YLL_excess_measures$YLL.un.b_ed
YLL.comparison_excess$YLL_b_deaths<-YLL_excess_measures$YLL.un.b_d
# Rates
YLL.comparison_excess$YLL_rates_b_excess_deaths<-YLL_excess_measures_rate_gbd_cpop$YLL.un.b_ed
YLL.comparison_excess$YLL_rates_b_deaths<-YLL_excess_measures_rate_gbd_cpop$YLL.un.b_d
# Excess / official counts
YLL.comparison_excess$Ratio_excess_death<-YLL.comparison_excess$YLL_rates_b_excess_deaths/
                                          YLL.comparison_excess$YLL_rates_b_deaths
# 
YLL.comparison_excess$Date_excess<-excess_dates_table$Date_excess
YLL.comparison_excess$Date_coverage<-excess_dates_table$Date_coverage

View(YLL_excess_measures)



###################################################################################################
##
##  DIRECTORY: Final results
##  
##
###################################################################################################

AL<-FALSE
# User defined directory
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Final results')
}else{setwd("/Users/adelinelo/Adeline Research Dropbox/Adeline Lo/COVID-19 - YLL - Shared/Data/Final results")}


## Total number of deaths, both genders combined
saveRDS(deaths.country.all,file="total_deaths_country_b.rds")

## YLL absolutes gendered
saveRDS(results.gender.total,file="absolute_YLL_gendered.rds")

## Age distribution of deaths per country -- both
saveRDS(results.deaths.age.country,file="deaths_age_country_b.rds")

## Compensating death chages
saveRDS(initial_guess,file="initial_guess_full.rds")
saveRDS(initial_guess,file="initial_guess_full.rds")

## Compensating death chages
saveRDS(results.YLL.b.cutt_off.global,file="yll_global_cut_off.rds")

## Age distribution of deaths per country
## YLL lost per age, gendered, YLL commulative gendered
saveRDS(Age_distribution,file="YLL_covid19_age_cummulative.rds")
## Yll cut offs per country
saveRDS(results.YLL.b.cutt_off.country,file="YLL_covid19_age_cummulative_countries.rds")

hist(results.YLL.b.cutt_off.country$cut55)

## YLL losts per death, rates, absolutes both gendered at not
saveRDS(YLL.covid.by_country,file="YLL_covid19_complete.rds")

saveRDS(YLL.covid.by_country[,c('Country','YLL_per_death_males', 'YLL_per_death_females','YLL_rates_male_to_female')],file="YLL_covid19_per_death.rds")





## Average age at which people die from covid gendered and not
saveRDS(avgsd_age_death_all,file="Age_death_descriptives_all.rds")

## YLL lost per age all countries combined, then at cut offs
saveRDS(results.YLL.b.age.total,file="YLL_lost_by_age_global.rds")
saveRDS(results.YLL.b.cutt_off.global,file="YLL_lost_by_age_global_cut_offs.rds")

## Excess deaths
saveRDS(YLL.comparison_excess,file="YLL.comparison_excess.rds")


## Projections vs current
saveRDS(results.proj.country,file="YLL_projections.rds")


## Comparisons to other causes
results.oc.country$covid_YLL.rate.un.b_cpop<-YLL.covid.by_country$YLL.b_rates
saveRDS(results.oc.country,file="YLL_other_causes_comparison.rds")



## Average age at death, age distribution at death
saveRDS(ages_at_death_global,file="ages_at_death_global.rds")
saveRDS(avgsd_age_death_both_global,file="avgsd_age_death_both_global.rds")
saveRDS(avgsd_age_death_bysex_all,file="avgsd_age_death_bysex_all.rds")



# Gender totals
saveRDS(results.gender.total[,c('YLL.un.m','YLL.un.f')],file="results.gender.total.rds")


# Dates and deaths
table_deaths_dates_p1<-data.frame(
                     Country=YLL.measures_covid19$Country,
                     Dates_covid_counts=YLL.measures_covid19$'Date.death',
                     Covid19_deaths_b=round(YLL.covid.by_country$Deaths,0),
                     Covid19_deaths_m=round(YLL.covid.by_country$Deaths_m,0),
                     Covid19_deaths_f=round(YLL.covid.by_country$Deaths_f,0))



# Dates and deaths
table_deaths_dates_p2<-data.frame(
  Country=YLL_excess_measures$Country,
  Dates_excess=YLL_excess_measures$Date,
  Excess_deaths=round(YLL.comparison_excess$Excess_deaths,0),
  Deaths=round(YLL.comparison_excess$Deaths,0))


# Merging the two into a final table
table_deaths_dates<-merge(table_deaths_dates_p1,table_deaths_dates_p2,by=c('Country'),all.x=TRUE)

saveRDS(table_deaths_dates,file="table_deaths_dates.rds")

