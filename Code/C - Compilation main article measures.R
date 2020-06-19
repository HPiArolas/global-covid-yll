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

AL<-FALSE
# User defined directory
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Final results')
}else{setwd("/Users/adelinelo/Adeline Research Dropbox/Adeline Lo/COVID-19 - YLL - Shared/Data/Final results")}

# Data
###################################################################################################
excess<-readRDS('YLL.comparison_excess.rds') ### need dates
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
  
###################################################################################################
##
##  PREPARATIONS
##  
##
###################################################################################################


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
ratio_male_feamle_deaths<-total_deaths_sample_m/total_deaths_sample_f



## Total number of YLL
###################################################################################################
# Subsetting
covid_main<-subset(covid_main,Country%in%sample_affected$Country)
# Total number of absoluteYLLs in sample
total_YLL_lost<-sum(covid_main$YLL.b_abs)

## Total number of YLL by gender
###################################################################################################
results.gender.total<-data.frame(Value='years_lost')
tmp.var<-c( "YLL.un.b",
            "YLL.un.m",
            "YLL.un.f")
results.gender.total[,tmp.var]<-colSums(yll_gendered[, tmp.var],na.rm=TRUE)
YLL_men_over_total<-results.gender.total$YLL.un.m/(results.gender.total$YLL.un.f+results.gender.total$YLL.un.m)

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


table_gender_imbalance<-data.frame(Country=avg_ages_at_death_bysex_all$Country,
                                  'YLL per death males'=yll_per_death_na$YLL_per_death_males,
                                  'YLL per death females'=yll_per_death_na$YLL_per_death_females,
                                  'Mean age-at-death males'=avg_ages_at_death_bysex_all$Avg_Age_Death_m,
                                  'Mean age-at-death females'=avg_ages_at_death_bysex_all$Avg_Age_Death_f,
                                  'Deaths males'=deaths_sample_m,
                                  'Deaths females'=deaths_sample_f)

stargazer(table_gender_imbalance,summary=FALSE, rownames=FALSE)

## Compensatory deaths
###################################################################################################
weighted.mean(initial_guess$Equal_prop, initial_guess$death_weight,na.rm=TRUE)

## Projected YLLs
###################################################################################################
YLL_total_projected<-sum(projections$YLL.b.proj_min)

ratio_YLL_projected_to_current<-YLL_total_projected/total_YLL_lost

## Table death dates and counts
###################################################################################################
# Only covid
stargazer(death_dates[,1:5], summary=FALSE, rownames=FALSE)
# Excess
death_dates_excess<-data.frame(Country=death_dates$Country,
                               Date=death_dates$Dates_excess,
                               'Excess deaths'=death_dates$Excess_deaths,
                               'COVID-19 deaths'=death_dates$Deaths)

death_dates_excess<-na.exclude(death_dates_excess)

stargazer(death_dates_excess, summary=FALSE, rownames=FALSE)

## Deaths that equalize 
###################################################################################################

table_equalizing_deaths<-data.frame(Country=initial_guess$Country,
                                    'Equalizing proportion'=initial_guess$Equal_prop)

stargazer(table_equalizing_deaths, summary=FALSE, rownames=FALSE)

# Subset out the 3 african countries with fewer than 6 deaths
# Total YLL lost 
# Total YLL lost males - females
# Age cut offs years of life lost -- global and country specific 
# Total number of deaths
# Calculation for Mikko equivalent number of deaths reduction.
# Average YLL per death.
# Male and female deaths, and age at which they happen.
# Average age at which deaths happen.

