###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Final result compilation
##
###################################################################################################
###################################################################################################


# Description:
# Final tables used in the policy brief.


# Notes:
# Mind the user defined paths until I can get the package here to work.

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

AL<-TRUE
# User defined directory
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared/Data/Final results')
}else{setwd("/Users/adelinelo/Adeline Research Dropbox/Adeline Lo/COVID-HPA/COVID-19 - YLL - Shared/Data/Final results")}


excess<-readRDS('YLL.comparison_excess_06_01_2021.rds') ### need dates
projections<-readRDS('YLL_projections.rds')
other_causes<-readRDS('YLL_other_causes_comparison.rds')
age_distribution<-readRDS('YLL_covid19_age_cummulative.rds') 
age_distribution<-do.call(rbind,age_distribution)
age_cut_off<-  readRDS('YLL_covid19_age_cummulative_countries.rds')
covid_main<-readRDS('YLL_covid19_complete.rds')
ages_at_death<-readRDS("Age_death_descriptives_all.rds")
yll_per_death<-readRDS("YLL_covid19_per_death.rds")
compensatory_deaths<-readRDS(file="initial_guess_full.rds")
age_cut_off_globlal<-readRDS('YLL_lost_by_age_global_cut_offs.rds')


test<-data.frame(covid_main$Date)
###################################################################################################
##
##  EXTRA FIGURES
##  
##
###################################################################################################

# Weighted average global reduction in the number of deaths for males to compensate.
#weighted.mean(compensatory_deaths$Equal_prop, compensatory_deaths$death_weight,na.rm=TRUE)


###################################################################################################
##
##  LIST OF FINAL COUNTRIES
##  
##
###################################################################################################

# Test sample with YLL rates at least over 10
sample_affected<-data.frame(
  Country=covid_main$Country[which(covid_main$Deaths>0)])
# Keep this sample for later
write.csv(sample_affected,file='sample_affected.csv')
saveRDS(sample_affected,file='sample_affected.RDS')

###################################################################################################
##
##  FINAL TABLE MATERIAL
##  
##
###################################################################################################

# Excess
table_excess<-data.frame(
              Country=excess$Country,
              Date_excess=excess$Dates_excess,
              Excess_deaths=excess$Excess_deaths,
              Deaths=excess$Deaths,
              YLL_rates_excess_deaths=excess$YLL_rates_b_excess_deaths,
              YLL_rates_deaths=excess$YLL_rates_b_deaths, 
              Ratio_excess_death=excess$Ratio_excess_death
)
 
write.csv(table_excess,file='table_excess.csv')
saveRDS(table_excess,file='table_excess.rds')

table_ratio_excess<-data.frame( Country=excess$Country, YLL_excess_ratio=excess$Ratio_excess_death)

# Projections
table_projections<-data.frame(
                Country=projections$Country,
                Deaths=round(covid_main$Deaths,0),
                Deaths_proj=projections$project_deaths,
                YLL_rates=projections$YLL.b_rates,
                YLL_rates_proj=projections$YLL.rates.b_min,
                Ratio_rates=projections$YLL.rates.ratio.current_projected_min)

write.csv(table_projections,file='table_projections.csv')

### Table S3:
#Projection Table with Stargazer
stargazer(table_projections,summary=FALSE, rownames=FALSE)
#Projection Table with Kable
kable(table_projections,format="latex",booktabs = T, longtable = T,linesep = "",digits = 3
      ,col.names = c("Country", "Deaths Current","Projected", "YLL rates Current",
                     "Projected", "Ratio of YLL Current over proj.")
      , caption = "Current and projected: COVID-19 deaths and YLL rates."
      ,label = "S3")%>%
  kable_styling(latex_options=c("striped","hold_position","scale_down","repeat_header"))%>%
  row_spec(0, bold=T, color="black",background="lightblue")%>%
  column_spec(2:5, width = "2cm")%>%
  column_spec(6, width = "2.5cm")

table_ratio_projections<-data.frame( Country=projections$Country, YLL_projection_ratio=projections$YLL.rates.ratio.current_projected_min)

# Other causes
table_covid_other<-data.frame(
                  Country=covid_main$Country,
                  Date=covid_main$Date,
                  YLL_rates_covid=covid_main$YLL.b_rates,
                  YLL_rates_heart=other_causes$heart_YLL.rate.un.b_cpop,
                  YLL_rates_drug=other_causes$drug_YLL.rate.un.b_cpop,
                  YLL_rates_transport=other_causes$transport_YLL.rate.un.b_cpop,
                  YLL_rates_flu_max=other_causes$flu_YLL.rate.un.max.b_cpop,
                  YLL_rates_flu_med=other_causes$ flu_YLL.rate.un.med.b_cpop)

length(other_causes$flu_YLL.rate.un.med.b_cpop)


# Other causes plus excess and projections
# Adding excess and projection to this dataframe
table_covid_other<-merge(table_covid_other,
                         table_ratio_excess,by=c('Country'), all.x=TRUE)

table_covid_other<-merge(table_covid_other,
                         table_ratio_projections,by=c('Country'), all.x=TRUE)

write.csv(table_covid_other,file='table_covid_other.csv')

saveRDS(table_covid_other,file='table_covid_other.rds')
                  
# Ages and genders
table_YLL_age<-age_distribution
table_YLL_age_cut_offs<-age_cut_off      

write.csv(table_YLL_age,file='table_YLL_age.csv')
write.csv(table_YLL_age_cut_offs,file='table_YLL_age_cut_offs.csv')
saveRDS(table_YLL_age_cut_offs,file='table_YLL_age_cut_offs.rds')

# YLL per death
table_yll_per_death<-yll_per_death

write.csv(table_yll_per_death,file='table_yll_per_death.csv')


# Mean age
table_age_at_death<-ages_at_death

write.csv(table_age_at_death,file='table_age_at_death.csv')



