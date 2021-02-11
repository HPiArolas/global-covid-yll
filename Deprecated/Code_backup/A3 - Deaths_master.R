# Description:
# Creating the master database with weekly deaths and exposures

source(here("Code/A0 - Functions.R"))

ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  mutate(PopCode = ifelse(PopCode == "AUS2", "AUS", PopCode))
db_d <- read_rds(here("Data", "deaths_stmf_age5.rds")) %>% 
  mutate(Country = ifelse(Country == "Scothland", "Scotland", Country))
db_p <- read_rds(here("Data", "pop_interpol_week_age5.rds"))

unique(db_d$PopCode) %>% sort()
unique(db_p$Country) %>% sort()

# Merging mortality data and population estimates
#################################################

db_dp <- db_d %>% 
  left_join(ctr_codes) %>% 
  mutate(wpp_code = ifelse(is.na(wpp_code), Country, wpp_code)) %>% 
  select(-Country) %>% 
  rename(Country = wpp_code) %>% 
  left_join(db_p, by = c("Country", "Year", "Week", "Age", "Sex")) %>% 
  drop_na() %>% 
  filter(PopCode != "RUS",
         Year >= 2010)

unique(db_dp$PopCode) %>% sort()

# definition of flu seasons and heat waves
##########################################
# For northern countries
flu_season <- c(seq(1, 14, 1), seq(46, 54, 1))
heat_waves <- seq(27, 35, 1)
# For southern countries
south_cts <- c("Australia", "Chile", "New Zealand") 
flu_season_south <- c(seq(21, 42, 1))
heat_waves_south <- c(seq(1, 7, 1), seq(52, 54, 1))
# Initial year for baseline estimation
ym <- 2010

# Formating data for baseline estimation
########################################
db2 <- db_dp %>% 
  # estimating exposures in person-weeks and rounding deaths to min 1
  mutate(Exposure = Pop / 52.25,
         Deaths = round(Deaths, 0) + 1) %>% 
  select(Country, PopCode, Year, Week, Sex, Age, Deaths, Exposure) %>% 
  filter(Year >= ym) %>% 
  arrange(Country, Sex, Age, Year, Week) %>% 
  group_by(Country, Age, Sex) %>% 
  mutate(t = 1:n()) %>% 
  ungroup() %>% 
  # adding sinusoidal terms for seasonality
  mutate(sn52 = sin((2*pi*t)/(52)),
         cs52 = cos((2*pi*t)/(52)),
         # excluding winter and summer weeks, as well as 2009 and COVID-19 pandemics
         include = case_when(Country %in% south_cts &
                               !(Week %in% heat_waves_south | Week %in% flu_season_south) &
                               (Year != 2020 & Year != 2009) ~ 1,
                             !(Country %in% south_cts) &
                               !(Week %in% heat_waves | Week %in% flu_season) &
                               (Year != 2020 & Year != 2009) ~ 1,
                             TRUE ~ 0),
         include = factor(include)) %>% 
  drop_na()

write_rds(db2, here("Data", "db_for_baseline_age5.rds"))

gc()
