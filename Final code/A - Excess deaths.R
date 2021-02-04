library(tidyverse)
library(lubridate)
rm(list=ls())

###################################################################################################
##
##  DIRECTORY
##  
##
###################################################################################################

# User defined directory
AL<-FALSE
if(!AL){setwd('/Users/Usuario/Dropbox/1 - A - A - Recerca/1 - Current work/COVID-19 - YLL - Shared')
}else{setwd("~/Adeline Research Dropbox/Adeline Lo/COVID-HPA/COVID-19 - YLL - Shared")}


# Sample of countries
full_sample<-read.csv("Data/full sample list.csv", header=TRUE)

data_coverage <- read_rds("Data/Processed/out2_06-01-2021.rds") %>% 
  mutate(Date = ymd(Date),
         Country = as.character(Country)) %>% 
  drop_na(Deaths) %>% 
  filter(!(Country == "Austria" & grepl("ECDC", Code)),
         !(Country == "Netherlands" & grepl("ECDC", Code)),
         !(Country == "Germany" & grepl("ECDC", Code)),
         !(Country == "Czechia" & grepl("ECDC", Code)))

data_excess <- read_csv("Data/Excess deaths/covid_excess_pclm_5_ages_0_95.csv") %>% 
  rename(Date_ex = last_date,
         Excess_Deaths = excess,
         Excess_Deaths.lp = excess_lp,
         Excess_Deaths.up = excess_up) %>% 
  filter(Age != "All", 
         Sex == "b") %>% 
  mutate(Age = as.integer(Age)) %>% 
  select(Country, Date_ex, Age, Excess_Deaths, Excess_Deaths.lp, Excess_Deaths.up)

cts_excess <- unique(data_excess$Country)

dates_excess <- data_excess %>% 
  select(Country, Date_ex) %>% 
  unique()

data_coverage2 <- data_coverage %>% 
  filter(Country %in% cts_excess,
         Region == "All",
         Sex == "b") %>% 
  select(Country, Date, Age, Deaths) 

# %>% 
#   mutate(Age = ifelse(Age == 95, 90, Age)) %>% 
#   group_by(Country, Date, Age) %>% 
#   summarise(Deaths = sum(Deaths)) %>% 
#   ungroup()
  
closest_dates <- data_coverage2 %>% 
  select(Country, Date) %>% 
  unique() %>% 
  left_join(dates_excess) %>% 
  mutate(diff = Date - Date_ex) %>% 
  group_by(Country) %>% 
  filter(diff == min(abs(diff))) %>% 
  ungroup()

dates_coverage <- closest_dates %>% 
  select(Country, Date) %>% 
  mutate(keep = 1)
  
data_deaths <- data_coverage2 %>% 
  left_join(dates_coverage) %>% 
  filter(keep == 1) %>% 
  select(-keep) %>% 
  left_join(data_excess) %>% 
  rename(Date_excess = Date_ex,
         Date_coverage = Date)

# Restricting to our sample
data_deaths<-data_deaths[which(is.element(data_deaths$Country,full_sample$Country)==TRUE),]

excess_dates_table <- data_deaths %>% 
  select(Country, Date_excess, Date_coverage) %>% 
  unique()

ctrs_to_include <- excess_dates_table %>% 
  select(Country) %>% 
  unique()

# Removing countries with excessive distance between excess dates and coverage
excess_dates_table_2<-excess_dates_table[which(abs(difftime(excess_dates_table$Date_excess,excess_dates_table$Date_coverage, units=c('days')))<=5),]
# Further restricting data_deaths to those within range of coverage
data_deaths_2<-data_deaths[which(is.element(data_deaths$Country,excess_dates_table_2$Country)==TRUE),]
# Generating the new sample of countries to inclode
ctrs_to_include_2 <- excess_dates_table_2 %>% 
  select(Country) %>% 
  unique()


write_rds(data_deaths_2, "Data/Processed/matched_excess_deaths_delayed_06_01_2021.rds")
write_rds(excess_dates_table_2, "Data/Processed/excess_dates_table_06_01_2021.rds")
write_rds(ctrs_to_include_2, "Data/Processed/countries_to_include_excess_06_01_2021.rds")
