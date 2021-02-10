# Description:
# Correspondance of dates and deaths between excess and confirmed deaths 

source(here("Code/A00_functions.R"))

data_coverage <- read_rds(here("Data", "out2_06-01-2021.rds")) %>% 
  mutate(Date = ymd(Date),
         Country = as.character(Country)) %>% 
  drop_na(Deaths) %>% 
  filter(!(Country == "Austria" & grepl("ECDC", Code)),
         !(Country == "Netherlands" & grepl("ECDC", Code)),
         !(Country == "Germany" & grepl("ECDC", Code)),
         !(Country == "Czechia" & grepl("ECDC", Code)))

data_excess <- read_csv(here("Data", "covid_excess_pclm_5_ages_0_95.csv")) %>% 
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

excess_dates_table <- data_deaths %>% 
  select(Country, Date_excess, Date_coverage) %>% 
  unique()

ctrs_to_include <- excess_dates_table %>% 
  select(Country) %>% 
  unique()

dim(ctrs_to_include)

write_rds(data_deaths, here("Data", "matched_excess_deaths_delayed.rds"))
write_rds(excess_dates_table, here("Data", "excess_dates_table.rds"))
write_rds(ctrs_to_include, here("Data", "countries_to_include_excess.rds"))
