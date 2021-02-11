###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Calculates EXCESS years of life lost (YLL) and related measures 



# Notes:
# Mind the user defined paths until I can get the package here to work.
# Data cleaning should be done in the data preparation files for the last version.
# This includes issues with covid-19 death count dataset.

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
}else{setwd("/Users/adelinelo/Adeline Research Dropbox/Adeline Lo/COVID-19 - YLL - Shared")}

###################################################################################################
##
##  DATA
##  
##
###################################################################################################

# COVID-19 excess deaths
###################################################################################################

# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
#if(!AL){excess<-readRDS("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/matched_excess_deaths.rds")
#}else{excess<-readRDS("Data/Processed/matched_excess_deaths.rds")}

## This uses data accounting for delays
# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
if(!AL){excess<-readRDS("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/matched_excess_deaths_delayed_10_07_2020.rds")
}else{excess<-readRDS("Data/Processed/matched_excess_deaths_delayed_10_07_2020.rds")}


# Choosing the max of excess or regular deaths
for (i in 1:length(excess$Country)){
excess$Excess_Deaths[i]<-max(excess$Deaths[i],excess$Excess_Deaths[i])
}
#unique(excess$Country)

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
countries<-sort(unique(excess$Country))

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


###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables
# * for both Excess_Deaths and Deaths in `excess` dateframe (where Deaths was previously created so matched in dates with Excess_Deaths)
# Data holders
yll.data.both<-vector("list",length=length(countries))

##  Both sexes
###################################################################################################
for(i in 1:length(countries)){
  cat("Country=",paste(countries[i]),"\n")
  # choose national level data for both genders
  c<-subset(excess,Country==countries[i])
  #Regular Excess Deaths
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b_ed <- c$Excess_Deaths*sle.gbd$sle 
  c$YLL.gbd.b_d <- c$Deaths*sle.gbd$sle 
  # number of years lost per age -- country specific
  tmp.sle.b<-subset(sle_both.un,as.character(country)==countries[i])
  c$YLL.un.b_ed <- c$Excess_Deaths*tmp.sle.b$sle_un 
  c$YLL.un.b_d <- c$Deaths*tmp.sle.b$sle_un 
  
  # Excess Deaths lp/up (no Deaths)
  # Excess Deaths lp
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b_lp<-c$Excess_Deaths.lp*sle.gbd$sle 
  # number of years lost per age -- country specific
  c$YLL.un.b_lp<-c$Excess_Deaths.lp*tmp.sle.b$sle_un 
  # Excess Deaths up
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b_up<-c$Excess_Deaths.up*sle.gbd$sle 
  # number of years lost per age -- country specific
  c$YLL.un.b_up<-c$Excess_Deaths.up*tmp.sle.b$sle_un 
  # subsetting to population
  tmp.population<-subset(pop,as.character(Country)==countries[i])
  # population count
  c$Population<-tmp.population$Total
  
  # computing rates for each measure
  c$YLL.rate.gbd.b_ed<-(c$YLL.gbd.b_ed/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b_d<-(c$YLL.gbd.b_d/tmp.population$Total)*100000 
  c$YLL.rate.un.b_ed<-(c$YLL.un.b_ed/tmp.population$Total)*100000 
  c$YLL.rate.un.b_d<-(c$YLL.un.b_d/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b_lp<-(c$YLL.gbd.b_lp/tmp.population$Total)*100000 
  c$YLL.rate.un.b_lp<-(c$YLL.un.b_lp/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b_up<-(c$YLL.gbd.b_up/tmp.population$Total)*100000 
  c$YLL.rate.un.b_up<-(c$YLL.un.b_up/tmp.population$Total)*100000 
  
  # reordering columns
  yll.data.both[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Age=c$Age,
                                 Population=c$Population,
                                 Deaths=c$Deaths,
                                 YLL.gbd.b_ed=c$YLL.gbd.b_ed,YLL.gbd.b_d=c$YLL.gbd.b_d,
                                 YLL.gbd.b_lp=c$YLL.gbd.b_lp,YLL.gbd.b_up=c$YLL.gbd.b_up,
                                 YLL.un.b_ed=c$YLL.un.b_ed, YLL.un.b_d=c$YLL.un.b_d,
                                 YLL.un.b_lp=c$YLL.un.b_lp, YLL.un.b_up=c$YLL.un.b_up, 
                                 Excess_Deaths=c$Excess_Deaths,Excess_Deaths_lp=c$Excess_Deaths.lp,Excess_Deaths_up=c$Excess_Deaths.up,
                                 YLL.rate.gbd.b_ed=c$YLL.rate.gbd.b_ed,YLL.rate.gbd.b_d=c$YLL.rate.gbd.b_d,
                                 YLL.rate.gbd.b_lp=c$YLL.rate.gbd.b_lp,YLL.rate.gbd.b_up=c$YLL.rate.gbd.b_up,
                                 YLL.rate.un.b_ed=c$YLL.rate.un.b_ed,YLL.rate.un.b_d=c$YLL.rate.un.b_d,
                                 YLL.rate.un.b_lp=c$YLL.rate.un.b_lp,YLL.rate.un.b_up=c$YLL.rate.un.b_up
  )
}
#names(yll.data.both)<-countries
yll.data.both.all<-do.call(rbind,yll.data.both)
# ver with countries 
if(!AL){saveRDS(yll.data.both,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-excess-list.rds") 
}else{saveRDS(yll.data.both,file="Data/Processed/yll-excess-list.rds") }
#this is the ver with all countries in one full dataframe
if(!AL){saveRDS(yll.data.both.all,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/yll-excess.rds") 
}else{saveRDS(yll.data.both.all,file="Data/Processed/yll-excess.rds") }


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
colnames.yll.measures<-c('Country', 'Date', "YLL.gbd.b_ed","YLL.gbd.b_d"
                         ,"YLL.gbd.b_lp","YLL.gbd.b_up"
                         ,"YLL.un.b_ed","YLL.un.b_d"
                         ,"YLL.un.b_lp","YLL.un.b_up")

# Data frame to store results
YLL.measures<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)<-colnames.yll.measures
Date<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  Date[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  tmp.var<-c("YLL.gbd.b_ed","YLL.gbd.b_d","YLL.gbd.b_lp","YLL.gbd.b_up"
             ,"YLL.un.b_ed","YLL.un.b_d","YLL.un.b_lp","YLL.un.b_up")
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
}
YLL.measures$Date<-Date

# saving
if(!AL){saveRDS(YLL.measures,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL_excess_measures.rds") 
}else{saveRDS(YLL.measures,file="Data/Processed/YLL_excess_measures.rds")}

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 'Date' 
                                  ,"YLL.rate.gbd.b_ed", "YLL.rate.gbd.b_d"
                                  ,"YLL.rate.gbd.b_lp","YLL.rate.gbd.b_up"
                                  ,"YLL.rate.un.b_ed","YLL.rate.un.b_d"
                                  ,"YLL.rate.un.b_lp","YLL.rate.un.b_up" )
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
Date<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  Date[i]<-yll.data.both.all$Date[tmp.ind][1]
  tmp.var<-c("YLL.rate.gbd.b_ed","YLL.rate.gbd.b_d", "YLL.rate.gbd.b_lp","YLL.rate.gbd.b_up"
             ,"YLL.rate.un.b_ed","YLL.rate.un.b_d","YLL.rate.un.b_lp","YLL.rate.un.b_up")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
}
YLL.measures.rate.esp$Date<-Date

# saving
if(!AL){saveRDS(YLL.measures.rate.esp,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL_excess_measures_rate_esp.rds") 
}else{saveRDS(YLL.measures.rate.esp,file="Data/Processed/YLL_excess_measures_rate_esp.rds")}
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################

colnames.yll.measures.rate.gbd<-c('Country', 'Date' 
                                  ,"YLL.rate.gbd.b_ed","YLL.rate.gbd.b_d"
                                  ,"YLL.rate.gbd.b_lp","YLL.rate.gbd.b_up"
                                  ,"YLL.rate.un.b_ed","YLL.rate.un.b_d"
                                  ,"YLL.rate.un.b_lp","YLL.rate.un.b_up" )

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
Date<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  Date[i]<-yll.data.both.all$Date[tmp.ind][1]
  tmp.var<-c("YLL.rate.gbd.b_ed", "YLL.rate.gbd.b_d","YLL.rate.gbd.b_lp","YLL.rate.gbd.b_up"
             ,"YLL.rate.un.b_ed", "YLL.rate.un.b_d","YLL.rate.un.b_lp","YLL.rate.un.b_up")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
}
YLL.measures.rate.gbd$Date<-Date

# saving
if(!AL){saveRDS(YLL.measures.rate.gbd,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL_excess_measures_rate_gbd.rds") 
}else{saveRDS(YLL.measures.rate.gbd,file="Data/Processed/YLL_excess_measures_rate_gbd.rds")}

###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC
##  
##
###################################################################################################

if(!AL){dat<-readRDS(file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL_excess_measures.rds") 
}else{dat<-readRDS(file="Data/Processed/YLL_excess_measures.rds")}

sumpop<-tapply(pop$Total,pop$Country,sum)
dat$Pop<-sumpop[which(names(sumpop)%in%dat$Country)]

rates<-dat[,c("YLL.gbd.b_d","YLL.gbd.b_ed","YLL.gbd.b_lp","YLL.gbd.b_up"
              ,"YLL.un.b_d","YLL.un.b_ed","YLL.un.b_lp","YLL.un.b_up")]/dat$Pop *100000
yll_rates<-as.data.frame(cbind(dat$Country,dat$Date,rates,dat$Pop))
names(yll_rates)<-c("Country","Date"
                    ,"YLL.gbd.b_d","YLL.gbd.b_ed","YLL.gbd.b_lp","YLL.gbd.b_up"
                    ,"YLL.un.b_d","YLL.un.b_ed","YLL.un.b_lp","YLL.un.b_up"
                    ,"Pop")


# saving
if(!AL){saveRDS(yll_rates,file="/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Processed/YLL_excess_measures_rate_gbd_cpop.rds") 
}else{saveRDS(yll_rates,file="Data/Processed/YLL_excess_measures_rate_gbd_cpop.rds")}

# Saving the sample of countries used here
sample.excess<-as.character(countries)
saveRDS(sample.excess,file="Data/Processed/sample.excess.rds")
