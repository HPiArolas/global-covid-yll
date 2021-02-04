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


# Subset out the 3 african countries with fewer than 6 deaths
# Total YLL lost 
# Total YLL lost males - females
# Age cut offs years of life lost -- global and country specific 
# Total number of deaths
# Calculation for Mikko equivalent number of deaths reduction.
# Average YLL per death.
# Male and female deaths, and age at which they happen.
# Average age at which deaths happen.


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
library(kableExtra)
library(sjPlot)
library(sjmisc)
library(tidyverse)

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

# Lists of countries
###################################################################################################

# User defined directory
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Final results')
}else{setwd("/Users/adelinelo/Adeline Research Dropbox/Adeline Lo/COVID-HPA/COVID-19 - YLL - Shared/Data/Final results")}

# Data
###################################################################################################
excess<-readRDS('YLL.comparison_excess_06_01_2021.rds') ### need dates
projections<-readRDS('YLL_projections.rds')
other_causes<-readRDS('YLL_other_causes_comparison.rds')
age_distribution<-readRDS('YLL_covid19_age_cummulative.rds') 
age_distribution<-do.call(rbind,age_distribution)
age_cut_off<-  readRDS('YLL_covid19_age_cummulative_countries.rds')
covid_main<-readRDS('YLL_covid19_complete.rds')
ages_at_death<-readRDS("Age_death_descriptives_all.rds")
avg_ages_at_death_global<-readRDS("avgsd_age_death_both_global.rds")
avg_ages_at_death_bysex_all<-readRDS("avgsd_age_death_bysex_all.rds")
yll_per_death<-readRDS("YLL_covid19_per_death.rds")
compensatory_deaths<-readRDS(file="initial_guess_full.rds")
age_cut_off_globlal<-readRDS('YLL_lost_by_age_global_cut_offs.rds')
total_deaths<-readRDS('total_deaths_country_b.rds')
sample_affected<-readRDS(file='sample_affected.RDS')
yll_gendered<-readRDS("absolute_YLL_gendered.rds")
yll_global_cut_off<-readRDS("yll_global_cut_off.rds")
death_dates<-readRDS("table_deaths_dates.rds")
initial_guess<-readRDS("initial_guess_full.rds")
life_expectancy_at0<-readRDS('country_both_sle_un.rds')
# complete dataset on deaths
#out<-read.csv("Data/Death counts/Output_5_19-5-2020.csv",skip=1) 
if(!AL){complete_dataset<-read.csv("/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Death counts/Output_5_06-01-2021.csv", skip=3)
}else{complete_dataset<-read.csv("../Death counts/Output_5_06-01-2021.csv", skip=3)}
  
###################################################################################################
##
##  PREPARATIONS
##  
##
###################################################################################################

## Period covered in sample
###################################################################################################

time_coverage<-data.frame(country=death_dates[,1],
                          first_date=NA,
                          last_date=death_dates[,2])

# The entire dataset subset to our sample
complete_dataset_sample<-subset(complete_dataset,Country%in%sample_affected$Country)
# Declaring dates
complete_dataset_sample$Date<-as.Date(complete_dataset_sample$Date,format="%d.%m.%Y")



# Looping to pick dates
for (i in 1:nrow(time_coverage)){
  tmp<-time_coverage[i,1]
  time_coverage[i,2]<-min(complete_dataset_sample$Date[which(complete_dataset_sample$Country==tmp)])
}
# Transforming it back into date format using a function (loops break dates)
date_fun <- function(x) {
  tmp <- as.Date(x, origin = "1970-01-01")
  return(tmp)
}
# Applying it to our dates
time_coverage$first_date<-date_fun(time_coverage[,2])
# Creating a days difference variable
time_coverage$days_covered<-difftime(time_coverage$last_date,time_coverage$first_date, units = c("days"))+1
# Storing
saveRDS(time_coverage,file='time_coverage.RDS')


## Total number of deaths
###################################################################################################
# Subsetting
total_deaths<-subset(total_deaths,Country%in%sample_affected$Country)
# Total number of deaths in sample
total_deaths_sample<-sum(total_deaths$Deaths_b)
# Total deaths gendered
total_deaths_sample_m<-sum(covid_main$Deaths_m,na.rm=TRUE)
total_deaths_sample_f<-sum(covid_main$Deaths_f,na.rm=TRUE)
# Ratio
ratio_male_female_deaths<-total_deaths_sample_m/total_deaths_sample_f



## Total number of YLL
###################################################################################################
# Subsetting
covid_main<-subset(covid_main,Country%in%sample_affected$Country)
# Total number of absoluteYLLs in sample
total_YLL_lost<-sum(covid_main$YLL.b_abs)

# Life expectancy at 0 and "lives lost"
###################################################################################################

# Simply choose life expectancy and birth and average across the sample
sample_average_e0<-mean(life_expectancy_at0$sle_un[which(life_expectancy_at0$age.code==0)])
# 74.85 years of e0.
# Dividing total yll lost by e0 to get "lives lost"
total_lives_lost<-total_YLL_lost/sample_average_e0


## Total number of YLL by gender
###################################################################################################
results.gender.total<-data.frame(Value='years_lost')
tmp.var<-c( "YLL.un.b",
            "YLL.un.m",
            "YLL.un.f")
results.gender.total[,tmp.var]<-colSums(yll_gendered[, tmp.var],na.rm=TRUE)
YLL_men_over_total<-results.gender.total$YLL.un.m/(results.gender.total$YLL.un.f+results.gender.total$YLL.un.m)
YLL_women_over_total<-results.gender.total$YLL.un.f/(results.gender.total$YLL.un.f+results.gender.total$YLL.un.m)
results.gender.total$YLL.un.m/results.gender.total$YLL.un.f #In text "Men have lost 45% more years of life than women"
## Global age 
###################################################################################################
table_global_YLL_age_cut_offs<-yll_global_cut_off     
# Under 75 YLLs
yll_under_75_global<-table_global_YLL_age_cut_offs$cut55+table_global_YLL_age_cut_offs$cut75

## YLL lost per death
###################################################################################################
YLL_lost_per_death<-total_YLL_lost/total_deaths_sample

## YLL lost per death -- by gender
###################################################################################################
YLL_lost_per_death_m<-results.gender.total$YLL.un.m/total_deaths_sample_m
YLL_lost_per_death_f<-results.gender.total$YLL.un.f/total_deaths_sample_f

## YLL lost per death -- country and males / females
###################################################################################################

## Average age at death global both genders
###################################################################################################
avg_ages_at_death_global

## Average age at death by country and gender
###################################################################################################
avg_ages_at_death_bysex_all

yll_per_death_na<-na.omit(yll_per_death)

# Gendered deaths
deaths_sample_m<-round(na.omit(covid_main$Deaths_m),0)
deaths_sample_f<-round(na.omit(covid_main$Deaths_f),0)


table_gender_imbalance<-data.frame(Country=avg_ages_at_death_bysex_all$Country,#64
                                  'YLL per death males'=yll_per_death_na$YLL_per_death_males,
                                  'YLL per death females'=yll_per_death_na$YLL_per_death_females,
                                  'Mean age-at-death males'=avg_ages_at_death_bysex_all$Avg_Age_Death_m,
                                  'Mean age-at-death females'=avg_ages_at_death_bysex_all$Avg_Age_Death_f,
                                  'Deaths males'=deaths_sample_m,
                                  'Deaths females'=deaths_sample_f)
### Table S4:
#Gender Imbalance Table with Stargazer
stargazer(table_gender_imbalance,summary=FALSE, rownames=FALSE)
#Gender Imbalance Table with kable
kable(table_gender_imbalance,format="latex",booktabs = T, longtable = T,linesep = "",digits = 3
      ,col.names = c("Country", "Males","Females", "Males","Females","Males","Females")
      , caption = "Gender imbalances: COVID-19 YLL rates, COVID-19 death counts, and ages-at-deaths."
      ,label = "S4")%>%
  kable_styling(latex_options=c("striped","hold_position","scale_down","repeat_header"))%>%
  add_header_above(c(" ", "YLL per death" = 2,"Mean age-at-death" = 2,"Deaths" = 2), bold=T) %>%
  row_spec(0, bold=T, color="black",background="lightblue")%>%
  column_spec(2:7, width = "2cm")

## Compensatory deaths
###################################################################################################
weighted.mean(initial_guess$Equal_prop, initial_guess$death_weight,na.rm=TRUE)
mean(initial_guess$Equal_prop,na.rm=TRUE)

## Projected YLLs
###################################################################################################
YLL_total_projected<-sum(projections$YLL.b.proj_min)

ratio_YLL_projected_to_current<-YLL_total_projected/total_YLL_lost

## Table death dates and counts
###################################################################################################
# Table S1: Covid-19 attributed deaths by date, coutnry, gender Only covid
#COVID deaths Table with Stargazer
stargazer(death_dates[,1:5], summary=FALSE, rownames=FALSE)
#COVID deaths Table with kable
kable(death_dates[,1:5],format="latex",booktabs = T, longtable = T,linesep = "",digits = 3
      ,col.names = c("Country", "Date","Both", "Males","Females")
      , caption = "COVID-19 attributed deaths by date, country and gender."
      ,label = "S1")%>%
  kable_styling(latex_options=c("striped","hold_position","scale_down","repeat_header"))%>%
  row_spec(0, bold=T, color="black",background="lightblue")%>%
  column_spec(2:5, width = "2cm")



# Excess
death_dates_excess<-data.frame(Country=death_dates$Country,
                               Date=death_dates$Dates_excess,
                               'Excess deaths'=death_dates$Excess_deaths,
                               'COVID-19 deaths'=death_dates$Deaths)

death_dates_excess<-na.exclude(death_dates_excess)
# Removing row names
row.names(death_dates_excess)<-NULL
# Table S2: Estimated excess deaths and COVID-19 deaths
#Excess deaths Table with Stargazer
stargazer(death_dates_excess, summary=FALSE, rownames=FALSE)
#Excess deaths Table with kable
kable(death_dates_excess,format="latex",booktabs = T, longtable = T,linesep = "",digits = 3
      ,col.names = c("Country", "Date","Excess deaths", "COVID-19 deaths")
      , caption = "Estimated excess deaths and COVID-19 deaths."
      ,label = "S2")%>%
  kable_styling(latex_options=c("striped","hold_position","scale_down","repeat_header"))%>%
  row_spec(0, bold=T, color="black",background="lightblue")%>%
  column_spec(2:4, width = "2cm")

# Ratio of covid YLL to excess YLL
ratio_covid_excess<-data.frame(country=excess$Country,
                               coverage_covid=excess$YLL_rates_b_deaths/excess$YLL_rates_b_excess_deaths,
                               excess_over_covid=excess$YLL_rates_b_excess_deaths/excess$YLL_rates_b_deaths)

## Deaths that equalize 
###################################################################################################

table_equalizing_deaths<-data.frame(Country=initial_guess$Country,
                                    'Equalizing proportion'=initial_guess$Equal_prop)

# the death weighted average is computed in the code file: C - Compilation final measures
# Table S5: Ratio of equalizing to current deaths
#Equalizing deaths Table with Stargazer
stargazer(table_equalizing_deaths, summary=FALSE, rownames=FALSE)
#Equalizing deaths Table with Kable
  #split into 2 groups and cbind
table_equalizing_deaths2<-cbind(table_equalizing_deaths[1:(nrow(table_equalizing_deaths)/2),]
                                ,table_equalizing_deaths[((nrow(table_equalizing_deaths)/2)+1):nrow(table_equalizing_deaths),])
kable(table_equalizing_deaths2,format="latex",booktabs = T, longtable = T,linesep = "",digits = 3
      ,col.names = c("Country", "Equalizing proportion", "Country", "Equalizing proportion")
      , caption = "Ratio of equalizing to current deaths."
      ,label = "S5")%>%
  kable_styling(latex_options=c("striped","hold_position","scale_down","repeat_header"))%>%
  row_spec(0, bold=T, color="black",background="lightblue")%>%
  column_spec(2, width = "3cm")%>%
  column_spec(3, width = "2cm")%>%
  column_spec(4, width = "3cm")
# Total YLL lost 
# Total YLL lost males - females
# Age cut offs years of life lost -- global and country specific 
# Total number of deaths
# Calculation equivalent number of deaths reduction.
# Average YLL per death.
# Male and female deaths, and age at which they happen.
# Average age at which deaths happen.


