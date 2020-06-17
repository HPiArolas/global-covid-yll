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

excess           <- readRDS(here("Data","YLL.comparison_excess.rds")) ### need dates
projections      <- readRDS(here("Data","YLL_projections.rds"))
other_causes     <- readRDS(here("Data","YLL_other_causes_comparison.rds"))
age_distribution <- readRDS(here("Data","YLL_covid19_age_cummulative.rds"))
age_distribution <- do.call(rbind,age_distribution)

age_cut_off      <- readRDS(here("Data","YLL_covid19_age_cummulative_countries.rds"))
covid_main       <- readRDS(here("Data","YLL_covid19_complete.rds"))
ages_at_death    <- readRDS(here("Data","Age_death_descriptives_all.rds"))
yll_per_death    <- readRDS(here("Data","YLL_covid19_per_death.rds"))
compensatory_deaths <- readRDS(here("Data","initial_guess_full.rds"))
age_cut_off_globlal <- readRDS(here("Data","YLL_lost_by_age_global_cut_offs.rds"))


sum(covid_main$Deaths)
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
  Country=covid_main$Country[which(covid_main$Deaths>6)])
# Keep this sample for later
write.csv(sample_affected,
          file=here("Data","sample_affected.csv"))
saveRDS(sample_affected,
        file=here("Data","sample_affected.RDS"))

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

write.csv(table_excess,
          file=here("Data","table_excess.csv"))
saveRDS(table_excess,
        file=here("Data","table_excess.rds"))

table_ratio_excess<-data.frame( Country=excess$Country, YLL_excess_ratio=excess$Ratio_excess_death)

# Projections
table_projections<-data.frame(
                Country=projections$Country,
                Deaths=covid_main$Deaths,
                Deaths_proj=projections$project_deaths,
                YLL_rates=projections$YLL.b_rates,
                YLL_rates_proj=projections$YLL.rates.b_min,
                Ratio_rates=projections$YLL.rates.ratio.current_projected_min)

write.csv(table_projections,
          file=here("Data","table_projections.csv"))

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
                         table_ratio_excess,by=c("Country"), all.x=TRUE)

table_covid_other<-merge(table_covid_other,
                         table_ratio_projections,by=c("Country"), all.x=TRUE)

write.csv(table_covid_other,
          file=here("Data","table_covid_other.csv"))

saveRDS(table_covid_other,
        file=here("Data","table_covid_other.rds"))




                  
# Ages and genders
table_YLL_age<-age_distribution
table_YLL_age_cut_offs<-age_cut_off      

write.csv(table_YLL_age,
          file=here("Data","table_YLL_age.csv"))
write.csv(table_YLL_age_cut_offs,
          file=here("Data","table_YLL_age_cut_offs.csv"))
saveRDS(table_YLL_age_cut_offs,
        file=here("Data","table_YLL_age_cut_offs.rds"))

# YLL per death
table_yll_per_death<-yll_per_death

write.csv(table_yll_per_death,
          file=here("Data","table_yll_per_death.csv"))


# Mean age
table_age_at_death<-ages_at_death

write.csv(table_age_at_death,
          file=here("Data","table_age_at_death.csv"))



