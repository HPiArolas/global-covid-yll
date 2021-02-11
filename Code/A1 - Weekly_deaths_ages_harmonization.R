# Description:
# Weekly all-cause mortality age harmonization in 5-year age groups

library(here)
source(here("Code/A0 - Functions.R"))

# Loading data
##############
# Input weekly mortality data from input files in the STMF, 
# Data version: January 15 2021
zipdf <- unzip(here("Data", "STMFinput.zip"), list = TRUE)

# loading all cause deaths from all countries in STMF
db_d <- NULL
for(i in 1:length(zipdf$Name)){
  csv_file <- zipdf$Name[i]
  temp <- read_csv(unz(here("Data", "STMFinput.zip"), csv_file))
  db_d <- db_d %>% 
    bind_rows(temp)
}

# info on country names and codes
ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  select(Country, PopCode) %>% 
  mutate(PopCode = ifelse(PopCode == "AUS2", "AUS", PopCode)) %>% 
  filter(PopCode != "GBR")


# Population exposures (offsets) from HMD for age harmonization
HMDcountries <- ctr_codes %>% pull(PopCode)
countries <- ctr_codes %>% pull(Country)
names(countries) <- HMDcountries

OffsetsL <- lapply(HMDcountries, function(xyz, us, pw, countries){
  X         <- readHMDweb(xyz, "Exposures_1x1", us, pw)
  X %>% 
    filter(Year >= 2010) %>% 
    pivot_longer(Female:Total, names_to = "Sex", values_to = "Population") %>% 
    mutate(Country = countries[xyz],
           Sex = case_when(Sex == "Male" ~ "m",
                           Sex == "Female" ~ "f",
                           Sex == "Total" ~ "b"),
           Population = as.numeric(Population),
           AgeInt = 1) %>% 
    arrange(Year, Sex, Age) %>% 
    select(Country, Year, Sex, Age, AgeInt, Population)
  
}, us = us, pw = pw, countries = countries)

names(OffsetsL) <- countries
# pad Offsets rightward.

# Use population in the last available year until 2020
Offsets_test <- 
  OffsetsL %>% 
  bind_rows() %>% 
  group_by(Country, Sex) %>% 
  do(pad_offsets(chunk = .data)) %>% 
  ungroup()

Offsets_test %>% 
  filter(is.na(Population)) %>% 
  select(Country) %>% 
  unique()

# Canada has missing values in exposures, so we use population 
# estimates, instead
can_pop <- readHMDweb("CAN", "Population", us, pw) %>% 
  filter(Year >= 2010) %>%
  select(Year, Age, Female1, Male1, Total1) %>% 
  pivot_longer(Female1:Total1, names_to = "Sex", values_to = "Population") %>% 
  mutate(Country = "Canada",
         Sex = case_when(Sex == "Male1" ~ "m",
                         Sex == "Female1" ~ "f",
                         Sex == "Total1" ~ "b"),
         AgeInt = 1) %>% 
  arrange(Year, Sex, Age) %>% 
  select(Country, Year, Sex, Age, AgeInt, Population)

OffsetsL$Canada <- can_pop

Offsets <- 
  OffsetsL %>% 
  bind_rows() %>% 
  group_by(Country, Sex) %>% 
  do(pad_offsets(chunk = .data)) %>% 
  ungroup()

Offsets %>% 
  filter(is.na(Population))

unique(Offsets$Country)

rm(OffsetsL);gc()
saveRDS(Offsets, here("Data", "Offsets.rds"))

# preparing mortality data
##########################

# distributing deaths at unknown weeks and ages
# step by step to avoid restarting the whole process
db_d2 <- db_d %>% 
  mutate(PopCode = ifelse(PopCode == "AUS2", "AUS", PopCode)) %>% 
  select(-Access, -Type) %>% 
  mutate(Age = ifelse(Age == "Unknown", "UNK", Age),
         Week = ifelse(is.na(Week), "UNK", Week)) %>% 
  group_by(PopCode, Year, Week, Sex) %>% 
  # scale TOT
  do(dist_tot(chunk = .data)) %>% 
  # redistribute UNK Age
  do(dist_unk(chunk = .data)) %>% 
  ungroup() 

db_d3 <- db_d2 %>% 
  # redistribute UNK Week
  group_by(PopCode, Year, Sex, Age) %>% 
  do(dist_unk_week(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = as.integer(Age),
         Week = as.integer(Week)) %>% 
  arrange(PopCode, Year, Week, Sex, Age)

db_d4 <- db_d3 %>% 
  group_by(PopCode, Year, Week) %>% 
  do(scale_sexes(chunk = .data)) %>% 
  ungroup() %>% 
  left_join(ctr_codes) %>% 
  group_by(Country, 
           Year,
           Week,
           Sex) %>% 
  mutate(AgeInterval = case_when(
    AgeInterval == "+" ~ as.character(111 - max(Age)),
    TRUE ~ AgeInterval),
    AgeInterval = as.integer(AgeInterval)) %>% 
  rename(AgeInt = AgeInterval) %>% 
  select(Country, PopCode, Year, Week, Sex, Age, AgeInt, Deaths) %>% 
  arrange(Country, Year, Week, Sex, Age) %>% 
  ungroup()
  
unique(db_d4$Week)
unique(db_d4$Age) %>% sort()
unique(db_d4$Sex)
unique(db_d4$Country)

saveRDS(db_d4, here("Data", "stmf.rds"))
db_d4 <- read_rds(here("Data", "stmf.rds"))


# selecting countries that need age harmonization
#################################################

# grouping ages 0-4 and 90+ in all countries 
db_d5 <- db_d4 %>% 
  select(Country, PopCode, Year, Week, Sex, Age, Deaths) %>% 
  filter(Age != "UNK" & Age != "TOT") %>% 
  mutate(Age = as.numeric(Age), 
         Age = case_when(Age >= 90 ~ 90, 
                         Age < 5 ~ 0,
                         TRUE ~ Age)) %>% 
  group_by(Country, PopCode, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  drop_na()

db_d6 <- db_d5 %>% 
  group_by(Country, PopCode, Year, Week, Sex) %>% 
  mutate(AgeGroups = sum(n()),
         last = ifelse(1:n() == AgeGroups, 1, 0),
         AgeInt = case_when(last == 0 ~ lead(Age) - Age, 
                            last == 1 ~ 111 - Age)) %>% 
  ungroup()

# countries that don't need splitting of age intervals 
# (19 age groups: 0-4, 5-9, ..., 85-89, 90+)
db_ok <- db_d6 %>%
  filter(Year >= 2010,
         AgeGroups == 19) %>% 
  select(Country, PopCode, Year, Week, Sex, Age, AgeInt, Deaths) %>% 
  drop_na()

# last week of observation by country
last_weeks <- db_ok %>% 
  filter(Year == 2020) %>% 
  group_by(PopCode) %>% 
  summarise(max_week = max(Week))

# weeks by country
table(db_ok$PopCode)

# series that need splitting of deaths into 5-year age intervals
# countries with weeks that contain fewer than 19 age groups
db_to_ungr <- db_d6 %>%
  filter(Year >= 2010,
         AgeGroups < 19) %>% 
  select(Country, PopCode, Year, Week, Sex, Age, AgeInt, Deaths)

# weeks to ungroup in each case
db_to_ungr %>%
  select(Country, PopCode, Year, Week) %>%
  unique() %>% 
  group_by(PopCode) %>% 
  summarise(min_year = min(Year),
         max_year = max(Year),
         weeks = sum(n()))

# AUT, NLD, and SWE have wide age groups only during a the last weeks,
# Deaths during this few weeks are ungrouped based on previous distributions 

exc_ungr <- c("AUT", "NLD", "SWE")

db_to_ungr2 <- db_to_ungr %>% 
  filter(!(PopCode %in% exc_ungr))

# ungrouping all ages for age intervals larger than 5-year
##########################################################

stmf <- db_to_ungr2

# Using PCLM functions to split age groups
start_time <- Sys.time()
stmf_out <-
  split(stmf,
        list(stmf$Country,
             stmf$Year,
             stmf$Week,
             stmf$Sex),
        drop = TRUE) %>%
  parallelsugar::mclapply(try_harmonize_age_p, Offsets = Offsets, mc.cores = 6)
end_time <- Sys.time()
end_time - start_time

stmf_out2 <- stmf_out %>% 
  bind_rows()

# Grouping in ages in 5-year intervals
stmf_out3 <- stmf_out2 %>% 
  mutate(Age = floor(Age / 5) * 5,
         Age = ifelse(Age > 90, 90, Age)) %>% 
  group_by(Country, PopCode, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  ungroup() %>% 
  mutate(Source = "Ungrouped")

# appending all (original and ungrouped) populations together
db_ok2 <- db_ok %>%
  select(Country, PopCode, Year, Week, Sex, Age, Deaths) %>% 
  mutate(Source = "Original")

db_deaths <- bind_rows(db_ok2, stmf_out3) %>% 
  arrange(Country, PopCode, Year, Week, Sex, Age) 

unique(db_deaths$PopCode)

write_rds(db_deaths, here("Data", "deaths_stmf_age5.rds"))
