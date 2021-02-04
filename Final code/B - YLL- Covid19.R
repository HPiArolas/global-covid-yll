###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Calculates years of life lost (YLL) and related measures 



# Notes:
# Mind the user defined paths until I can get the package here to work.
# Data cleaning should be done in the data preparation files for the last version.
# This includes issues with covid-19 death count dataset.
# For countries for which we do not have gender specific data, we use YLL.gbd.m in the final measures.

###################################################################################################
##
##  LIBRARIES
##  
##
###################################################################################################

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(wpp2019)

###################################################################################################
##
##  DIRECTORY
##  
##
###################################################################################################

AL<-FALSE
# User defined directory
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared')
}else{setwd("/Users/adelinelo/Adeline Research Dropbox/Adeline Lo/COVID-HPA/COVID-19 - YLL - Shared")}

###################################################################################################
##
##  DATA
##  
##
###################################################################################################

# COVID-19 deaths 
###################################################################################################
if(!AL){out<-readRDS("Data/Processed/deathcounts_clean06-01-2021.rds")
}else{out<-readRDS("Data/Processed/deathcounts_clean06-01-2021.rds")}

# COVID-19 excess deaths
###################################################################################################

# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
if(!AL){nytexcess<-read.csv("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Excess deaths/NYTExcessDeaths.csv",sep=",")
}else{nytexcess<-read.csv("Data/Excess deaths/NYTExcessDeaths.csv",sep=",")}
#we need to create a dataframe, with Country, Excess Deaths as variables. These will be accessed later to create for the out data, by country, the `Excess_Deaths` variable.
#matched countries
matched<-c("Austria","Belgium","Denmark","France","Germany","Italy","Netherlands","Norway"
           ,"Portugal","South Africa","Spain","Sweden","Switzerland")
nytexcess<-nytexcess[which(nytexcess$Country%in%matched),]

# SLE
###################################################################################################
# GBD life expectancy best case standard
if(!AL){sle.gbd<-readRDS("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/country_gbd_sle.2016.rds")
}else{sle.gbd<-readRDS("Data/Processed/country_gbd_sle.2016.rds")}
# Country specific life expectancies
if(!AL){sle_both.un<-readRDS('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/country_both_sle_un.rds')
}else{sle_both.un<-readRDS('Data/Processed/country_both_sle_un.rds')}
if(!AL){sle_male.un<-readRDS('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/country_male_sle_un.rds')
}else{sle_male.un<-readRDS('Data/Processed/country_male_sle_un.rds')}
if(!AL){sle_female.un<-readRDS('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/country_female_sle_un.rds')
}else{sle_female.un<-readRDS('Data/Processed/country_female_sle_un.rds')}

# Population
###################################################################################################
if(!AL){pop<-read.csv('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/pop_complete.csv',row.names = 1)
}else{pop<-read.csv('Data/Processed/pop_complete.csv',row.names = 1)}
# List of countries
###################################################################################################
# Country list file with full sample considered
if(!AL){sample.countries<-read.csv("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/full sample list.csv",header=TRUE)
}else{sample.countries<-read.csv("Data/full sample list.csv",header=TRUE)}
# Saving them as a list
sample.countries.list<-as.character(data.frame(sample.countries)[,1])
# List of countries with no data
no.data.countries<- c('Northern Ireland')
# Final list 
countries<-sample.countries.list[which(is.element(sample.countries.list,no.data.countries)==FALSE)]
# Saving it again
if(!AL){saveRDS(countries,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/our_sample.rds") 
}else{saveRDS(countries,file="Data/Processed/our_sample.rds") }
# Which countries have gender 
#gender.count<-as.character(unique(out$Country[which(out$Sex=='f'&out$Region=='All')]))
if(!AL){gender.count<-read.csv("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/gender_sample.csv")
}else{gender.count<-read.csv("Data/gender_sample.csv")}
gender.count<-as.character(data.frame(gender.count)[,1])

# Standard population weights
###################################################################################################
#  European standard
if(!AL){file.pop.std.gbd<-paste("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/pop_std.gbd.RDS")
}else{file.pop.std.gbd<-paste("Data/Processed/pop_std.gbd.RDS")}
pop_std.gbd<-readRDS(file.pop.std.gbd)
#  GBD standard
if(!AL){file.pop.std.esp<-paste("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/pop_std.esp.RDS")
}else{file.pop.std.esp<-paste("Data/Processed/pop_std.esp.RDS")}
pop_std.esp<-readRDS(file.pop.std.esp)

# Projected deaths
###################################################################################################
if(!AL){file.projections_covid_deaths<-paste("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/projections_covid_deaths.RDS")
}else{file.projections_covid_deaths<-paste("Data/Processed/projections_covid_deaths.RDS")}
# Imperial college based projections
projection.covid.deaths<-readRDS(file.projections_covid_deaths)

###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables

# Coinciding for which we have data already collected, i.e. sample.countries
gender.count.sample<-gender.count

# Total count in the out file, not necessarily in our sample
total.count<-as.character(unique(out$Country[which(out$Sex=='b')]))

# Data holders
yll.data.both<-vector("list",length=length(countries))
# Pick countries with male / female data
countries_mf<-as.character(data.frame(gender.count.sample)[,1])
yll.data.male<-yll.data.female<-vector("list",length=length(countries_mf))

#sum(out$Deaths[which(out$Country=='Austria'&out$Sex=='b')])
#sum(out$Deaths[which(out$Country=='Finland'&out$Sex=='b')])

##  Both sexes
###################################################################################################

for(i in 1:length(countries)){
  cat("Country=",countries[i],"\n")

  c<-subset(out,Country==countries[i]&Sex=="b"&Region=="All")
  c<-c[which(c$Date==max(c$Date)),]
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b<-c$Deaths*sle.gbd$sle 
  # number of years lost per age -- country specific
  tmp.sle.b<-subset(sle_both.un,country==countries[i])
  c$YLL.un.b<-c$Deaths*tmp.sle.b$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries[i],matched)){
    c$Excess_Deaths<-(c$Deaths/sum(c$Deaths))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries[i])]
    #this second part is accessing the country's excess death value in nytexcess, such that it matches the current country from out we're working on atm
  }else{
    c$Excess_Deaths<-c$Deaths #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
  }
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.b.ed<-c$Excess_Deaths*sle.gbd$sle
  # number of years lost per age -- country specific
  c$YLL.un.b.ed<-c$Excess_Deaths*tmp.sle.b$sle
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Total
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.b<-(c$YLL.gbd.b/tmp.population$Total)*100000 
  c$YLL.rate.un.b<-(c$YLL.un.b/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b.ed<-(c$YLL.gbd.b.ed/tmp.population$Total)*100000 
  c$YLL.rate.un.b.ed<-(c$YLL.un.b.ed/tmp.population$Total)*100000 
  # reordering columns
  yll.data.both[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 Cases=c$Cases,
                                 Deaths=c$Deaths,
                                 YLL.gbd.b=c$YLL.gbd.b,
                                 YLL.un.b=c$YLL.un.b, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.b.ed=c$YLL.gbd.b.ed, 
                                 YLL.un.b.ed=c$YLL.un.b.ed,
                                 YLL.rate.gbd.b=c$YLL.rate.gbd.b,
                                 YLL.rate.un.b=c$YLL.rate.un.b,
                                 YLL.rate.gbd.b.ed=c$YLL.rate.gbd.b.ed,
                                 YLL.rate.un.b.ed=c$YLL.rate.un.b.ed
                                 
  )
}
names(yll.data.both)<-countries
yll.data.both.all<-do.call(rbind,yll.data.both)
#check if always 0 or 20: 
table(yll.data.both.all$Country,yll.data.both.all$Sex)
# ver with countries 
if(!AL){saveRDS(yll.data.both,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-b-list.rds") 
}else{saveRDS(yll.data.both,file="Data/Processed/yll-b-list.rds") }
#this is the ver with all countries in one full dataframe
if(!AL){saveRDS(yll.data.both.all,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-b.rds") 
}else{saveRDS(yll.data.both.all,file="Data/Processed/yll-b.rds") }
##  Male only
###################################################################################################

for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="m"&Region=="All")
  c<-c[which(c$Date==max(c$Date)),]
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.m<-c$Deaths*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_male.un,country==countries_mf[i])
  c$YLL.un.m<-c$Deaths*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$Deaths/sum(c$Deaths))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
    #this second part is accessing the country's excess death value in nytexcess, such that it matches the current country from out we're working on atm
  }else{
    c$Excess_Deaths<-c$Deaths #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
  }
  # standard gbd SLE
  c$YLL.gbd.m.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.m.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Male
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.m<-(c$YLL.gbd.m/tmp.population$Male)*100000 
  c$YLL.rate.un.m<-(c$YLL.un.m/tmp.population$Male)*100000 
  c$YLL.rate.gbd.m.ed<-(c$YLL.gbd.m.ed/tmp.population$Male)*100000 
  c$YLL.rate.un.m.ed<-(c$YLL.un.m.ed/tmp.population$Male)*100000 
  # reordering columns
  yll.data.male[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 Cases=c$Cases,
                                 Deaths=c$Deaths,
                                 YLL.gbd.m=c$YLL.gbd.m,
                                 YLL.un.m=c$YLL.un.m, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.m.ed=c$YLL.gbd.m.ed, 
                                 YLL.un.m.ed=c$YLL.un.m.ed,
                                 YLL.rate.gbd.m=c$YLL.rate.gbd.m,
                                 YLL.rate.un.m=c$YLL.rate.un.m,
                                 YLL.rate.gbd.m.ed=c$YLL.rate.gbd.m.ed,
                                 YLL.rate.un.m.ed=c$YLL.rate.un.m.ed
                                 
  )
}
names(yll.data.male)<-countries_mf
yll.data.male.all<-do.call(rbind,yll.data.male)
#check if always 0 or 20: 
table(yll.data.male.all$Country,yll.data.male.all$Sex)
#ver with countries_mf 
if(!AL){saveRDS(yll.data.male,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-m-list.rds")
}else{saveRDS(yll.data.male,file="Data/Processed/yll-m-list.rds")}
#this is the ver with all countries_mf in one full dataframe
if(!AL){saveRDS(yll.data.male.all,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-m.rds") 
}else{saveRDS(yll.data.male.all,file="Data/Processed/yll-m.rds") }
##  Female only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="f"&Region=="All")
  c<-c[which(c$Date==max(c$Date)),]
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.f<-c$Deaths*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_female.un,country==countries_mf[i])
  c$YLL.un.f<-c$Deaths*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$Deaths/sum(c$Deaths))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
    #this second part is accessing the country's excess death value in nytexcess, such that it matches the current country from out we're working on atm
  }else{
    c$Excess_Deaths<-c$Deaths #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
  }
  # standard gbd SLE
  c$YLL.gbd.f.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.f.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Female
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.f<-(c$YLL.gbd.f/tmp.population$Female)*100000 
  c$YLL.rate.un.f<-(c$YLL.un.f/tmp.population$Female)*100000 
  c$YLL.rate.gbd.f.ed<-(c$YLL.gbd.f.ed/tmp.population$Female)*100000 
  c$YLL.rate.un.f.ed<-(c$YLL.un.f.ed/tmp.population$Female)*100000 
  # reordering columns
  yll.data.female[[i]]<-data.frame(Country=c$Country,
                                   Date=c$Date,
                                   Sex=c$Sex,
                                   Age=c$Age,
                                   Population=c$Population,
                                   Cases=c$Cases,
                                   Deaths=c$Deaths,
                                   YLL.gbd.f=c$YLL.gbd.f,
                                   YLL.un.f=c$YLL.un.f, 
                                   Excess_Deaths=c$Excess_Deaths,
                                   YLL.gbd.f.ed=c$YLL.gbd.f.ed, 
                                   YLL.un.f.ed=c$YLL.un.f.ed,
                                   YLL.rate.gbd.f=c$YLL.rate.gbd.f,
                                   YLL.rate.un.f=c$YLL.rate.un.f,
                                   YLL.rate.gbd.f.ed=c$YLL.rate.gbd.f.ed,
                                   YLL.rate.un.f.ed=c$YLL.rate.un.f.ed
                                   
  )
}
names(yll.data.female)<-countries_mf
yll.data.female.all<-do.call(rbind,yll.data.female)
#check if always 0 or 20: 
table(yll.data.female.all$Country,yll.data.female.all$Sex)
#ver with countries_mf 
if(!AL){saveRDS(yll.data.female,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-f-list.rds")
}else{saveRDS(yll.data.female,file="Data/Processed/yll-f-list.rds")}
  #this is the ver with all countries_mf in one full dataframe
if(!AL){saveRDS(yll.data.female.all,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-f.rds") 
}else{saveRDS(yll.data.female.all,file="Data/Processed/yll-f.rds")}

###################################################################################################
##
##  AGGREGATION : YLL ABSOLUTE NUMBERS
##  
##
###################################################################################################

# Total YLL by country, YLL rates with standardized population
# File format is: Country, Date.death, Date.edeath, 
# YLL.gbd.b, YLL.un.b,YLL.gbd.b.ed, YLL.un.b.ed, 
# YLL.gbd.m, YLL.un.m, YLL.gbd.m.ed, YLL.un.m.ed,
# YLL.gbd.f, YLL.un.f,YLL.gbd.f.ed, YLL.un.f.ed

# Similarly for rates

# Columns currently included
colnames.yll.measures<-c('Country', 'Date.death', 'Date.edeath', 
                         'YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed', 
                         'YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed',
                         'YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed')

# Data frame to store results
YLL.measures<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)<-colnames.yll.measures
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  tmp.ind2<-which(as.character(yll.data.male.all$Country)==as.character(YLL.measures$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.gbd.b","YLL.un.b","YLL.gbd.b.ed","YLL.un.b.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.m","YLL.un.m","YLL.gbd.m.ed","YLL.un.m.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind2, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.f","YLL.un.f","YLL.gbd.f.ed","YLL.un.f.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind2, tmp.var],na.rm=TRUE)
}
YLL.measures$Date.death<-Date.death

# saving
if(!AL){saveRDS(YLL.measures,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL.measures.rds") 
}else{saveRDS(YLL.measures,file="Data/Processed/YLL.measures.rds")}

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 'Date.death', 'Date.edeath', 
                         'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                         'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                         'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################

# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  tmp.ind2<-which(as.character(yll.data.male.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.esp$percent_pop),na.rm=TRUE)
  
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  #deal with possible NAs
  if(nrow(yll.data.male.all[tmp.ind2, tmp.var])==20){
    YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind2, tmp.var]*(pop_std.esp$percent_pop),na.rm=TRUE)
  }else{YLL.measures.rate.esp[i,tmp.var]<-colSums(matrix(0,nrow=20,length(tmp.var))*(pop_std.esp$percent_pop),na.rm=TRUE)}
  
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  #deal with possible NAs
  if(nrow(yll.data.female.all[tmp.ind2, tmp.var])==20){
    YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind2, tmp.var]*(pop_std.esp$percent_pop),na.rm=TRUE)
  }else{YLL.measures.rate.esp[i,tmp.var]<-colSums(matrix(0,nrow=20,length(tmp.var))*(pop_std.esp$percent_pop),na.rm=TRUE)}
  
  
  
  
}
YLL.measures.rate.esp$Date.death<-Date.death
YLL.measures.rate.esp$Date.edeath<-Date.edeath

# saving
if(!AL){saveRDS(YLL.measures.rate.esp,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL.measures.rate.esp.rds") 
}else{saveRDS(YLL.measures.rate.esp,file="Data/Processed/YLL.measures.rate.esp.rds")}
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################


colnames.yll.measures.rate.gbd<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  tmp.ind2<-which(as.character(yll.data.male.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  #deal with possible NAs
  if(nrow(yll.data.male.all[tmp.ind2, tmp.var])==20){
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind2, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  }else{YLL.measures.rate.gbd[i,tmp.var]<-colSums(matrix(0,nrow=20,length(tmp.var))*(pop_std.gbd$percent_pop/100),na.rm=TRUE)}
  
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  #deal with possible NAs
  if(nrow(yll.data.female.all[tmp.ind2, tmp.var])==20){
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind2, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  }else{YLL.measures.rate.gbd[i,tmp.var]<-colSums(matrix(0,nrow=20,length(tmp.var))*(pop_std.gbd$percent_pop/100),na.rm=TRUE)}
}
YLL.measures.rate.gbd$Date.death<-Date.death
YLL.measures.rate.gbd$Date.edeath<-Date.edeath

# saving
if(!AL){saveRDS(YLL.measures.rate.gbd,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL.measures.rate.gbd.rds") 
}else{saveRDS(YLL.measures.rate.gbd,file="Data/Processed/YLL.measures.rate.gbd.rds")}



###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC
##  
##
###################################################################################################

if(!AL){dat<-readRDS(file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL.measures.rds") 
}else{dat<-readRDS(file="Data/Processed/YLL.measures.rds")}

sumpop<-tapply(pop$Total,pop$Country,sum)
sumpop_f<-tapply(pop$Female,pop$Country,sum)
sumpop_m<-tapply(pop$Male,pop$Country,sum)
dat$Pop_f<-sumpop_f[which(names(sumpop_f)%in%dat$Country)]
dat$Pop_m<-sumpop_m[which(names(sumpop_m)%in%dat$Country)]
dat$Pop<-sumpop[which(names(sumpop)%in%dat$Country)]

rates_m<-dat[,c('YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed')]/dat$Pop_m *100000
rates_f<-dat[,c('YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed')]/dat$Pop_f *100000
rates<-dat[,c('YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed')]/dat$Pop *100000

yll_rates<-as.data.frame(cbind(dat$Country,dat$Date.death,rates,rates_m,rates_f,dat$Pop,dat$Pop_m,dat$Pop_f))

# saving
if(!AL){saveRDS(yll_rates,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL.measures_rate_cpop.rds") 
}else{saveRDS(yll_rates,file="Data/Processed/YLL.measures_rate_cpop.rds")}



#rates<-dat[,c(                         'YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed', 
#                                       'YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed',
#                                       'YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed')]/dat$Pop *100000
#yll_rates<-as.data.frame(cbind(dat$Country,dat$Date.death,rates,dat$Pop))
#names(yll_rates)<-c("Country","Date",
#                    'YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed', 
#                    'YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed',
#                    'YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed','Population')
#

# saving
#if(!AL){saveRDS(yll_rates,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL.measures_rate_cpop.rds") 
#}else{saveRDS(yll_rates,file="Data/Processed/YLL.measures_rate_cpop.rds")}






