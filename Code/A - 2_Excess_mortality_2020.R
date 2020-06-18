###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Summarize EXCESS deaths since 24th February, 2020, in all countries by sex and age 

rm(list=ls())

db_all <- read_csv(here("Data", "baseline_excess_pclm_5.csv"))

unique(db_all$Country)

# countries with 2 weeks delay in weekly deaths reports 
delays <- c("Finland", "USA", "Norway", "Spain", "Sweden")

max_week <- db_all %>%  
  filter(Year == 2020) %>% 
  group_by(Country) %>% 
  summarise(last_week = max(Week)) %>% 
  mutate(last_week = ifelse(Country %in% delays, last_week - 2, last_week))
  
db2 <- db_all %>% 
  left_join(max_week) %>%
  filter(date >= "2020-02-24",
         Week <= last_week, 
         excess >= 0) %>% 
  mutate(excess_lp = ifelse(Deaths - up > 0, Deaths - up, 0),
         excess_up = Deaths - lp,
         Age = as.character(Age)) %>% 
  group_by(Country, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths),
            baseline = sum(pred),
            excess = sum(excess),
            excess_lp = sum(excess_lp),
            excess_up = sum(excess_up),
            last_week = max(Week)) %>% 
  ungroup()

unique(db2$Sex)
unique(db2$Age)
unique(db2$Country)

lists <- db2 %>% 
  mutate(id = 1,
         Age = as.integer(Age)) %>% 
  group_by(Country, Sex, Age) %>% 
  summarise(id = sum(id))

db3 <- db2 %>% 
  group_by(Country, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            baseline = sum(baseline),
            excess = sum(excess),
            excess_lp = sum(excess_lp),
            excess_up = sum(excess_up),
            last_week = max(last_week)) %>% 
  mutate(Age = "Aggr")

db4 <- bind_rows(db2, db3) %>% 
  ungroup() %>% 
  mutate(last_date = as.Date(paste(2020, last_week, 1, sep="-"), "%Y-%U-%u")) %>% 
  arrange(Country, Sex, suppressWarnings(as.integer(Age)))

write_csv(db4,  file = here("Data", "covid_excess_pclm_5_delay.csv"))

