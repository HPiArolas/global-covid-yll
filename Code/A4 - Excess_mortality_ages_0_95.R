# Description:
# Summarize EXCESS deaths since week 8, 2020, in all countries by sex and age 

source(here("Code/A0 - Functions.R"))

# mortality baseline estimates
baseline_files <- fs::dir_ls(here("Data", "baseline_by_country"))
db_all <- vroom(baseline_files)

# country codes and names
ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  select(Country, PopCode) %>% 
  mutate(PopCode = ifelse(PopCode == "AUS2", "AUS", PopCode))


# Note:
#######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Estimates for USA at all ages and England and Wales below age 30 must 
# be excluded given the inaccurate partition of age groups
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

db2 <- db_all %>% 
  select(PopCode, Year, Week, Date, Sex, Age, Deaths, Baseline, lp, up, Exposure) %>% 
  filter(Year >= 2020,
         PopCode != "USA") %>% 
  mutate(Baseline = ifelse(PopCode == "GBRTENW" & Age < 30, Deaths, Baseline),
         lp = ifelse(PopCode == "GBRTENW" & Age < 30, Deaths, lp),
         up = ifelse(PopCode == "GBRTENW" & Age < 30, Deaths, up))


unique(db2$PopCode)

# STMF metadata states that mortality during the last three weeks reported 
# is incomplete. Then, these last three weeks are excluded from estimates.
max_weeks <- db2 %>% 
  group_by(PopCode) %>% 
  summarise(max_week = max(Week) - 3) %>% 
  ungroup()

# start to count excess deaths since week 8, 2020
week_ini <- 8
ISOweek::ISOweek2date("2020-W08-1")

db3 <- db2 %>% 
  left_join(ctr_codes) %>% 
  left_join(max_weeks) %>% 
  mutate(excess = ifelse(Deaths > Baseline, Deaths - Baseline, 0),
         excess_lp = ifelse(Deaths > up, Deaths - up, 0),
         excess_up = ifelse(Deaths > lp, Deaths - lp, 0),
         baseline = ifelse(Baseline > Deaths, Deaths, Baseline),
         week1 = week_ini,
         Age = as.character(Age)) %>% 
  filter(Week >= week1 & Week <= max_week) %>% 
  group_by(Country, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths),
            baseline = sum(Baseline),
            excess = sum(excess),
            excess_lp = sum(excess_lp),
            excess_up = sum(excess_up),
            last_week = max(Week)) %>% 
  ungroup() %>% 
  arrange(Sex, suppressWarnings(as.integer(Age)))

# Splitting last age group, from 90+ to 90-94 and 95+ 
#####################################################
a <- seq(0, 90, 5)

cts <- unique(db3$Country)
sxs <- c("b", "f", "m")
db_adj <- tibble()

for (c in cts){
  for (s in sxs){
    temp <- db3 %>% 
      filter(Country == c,
             Sex == s)
    
    l_week <- 
      temp %>% 
      pull(last_week) %>% 
      unique
    
    new_last <- 
      tibble(Country = c, 
             Sex = s, 
             Age = c("90", "95"), 
             last_week = l_week,
             Deaths = ungr_last(a, temp$Deaths), 
             excess = ungr_last(a, temp$excess), 
             excess_lp = ungr_last(a, temp$excess_lp), 
             excess_up = ungr_last(a, temp$excess_up), 
             baseline = ungr_last(a, temp$baseline)) 
    
    temp2 <- temp %>% 
      filter(Age != 90) %>% 
      bind_rows(new_last)
    
    db_adj <- db_adj %>% 
      bind_rows(temp2)
  }    
}

db4 <- db_adj %>% 
  group_by(Country, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            baseline = sum(baseline),
            excess = sum(excess),
            excess_lp = sum(excess_lp),
            excess_up = sum(excess_up),
            last_week = max(last_week)) %>% 
  mutate(Age = "All") %>% 
  ungroup()

db5 <- bind_rows(db_adj, db4) %>% 
  arrange(Country, Sex, suppressWarnings(as.integer(Age))) %>% 
  mutate(last_date = ISOweek::ISOweek2date(paste0(2020, "-W", sprintf("%02d", last_week), "-7")),
         Country = case_when(Country == "England_Wales" ~ "England", 
                             Country == "Republic of Korea" ~ "South Korea",
                             Country == "Northern Irland" ~ "Northern Ireland",
                             TRUE ~ Country)) 

write_csv(db5, here("Data", "covid_excess_pclm_5_ages_0_95.csv"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plots of excess mortality estimates
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####################################

# Creating Figures folder if it does not exist
if (!dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}

# plot of excess mortality by country with prediction intervals 
db5 %>% 
  filter(Sex == "b",
         Age == "All") %>% 
  mutate(excess_lp = excess_lp / 1000,
         excess_up = excess_up / 1000,
         excess = excess / 1000) %>% 
  ggplot()+
  geom_errorbar(aes(xmin = excess_lp, xmax = excess_up, y = reorder(Country, excess)), col = "#F05142", alpha = 0.9, size = 0.6, width = 0.4)+
  geom_point(aes(excess, reorder(Country, excess)), col = "#E22412")+
  scale_x_continuous(breaks = seq(0, 1500, 10), labels = comma)+
  labs(title = "Excess mortality since Feb 24th 2020", x = "Excess mortality (in thousands)", y = "")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=12),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=1)
  )
ggsave(here("Figures", "fig_s10_excess.png"), dpi = 600, width = 6, height = 6)


# plot of baseline and observed mortality by country
db5 %>% 
  filter(Sex == "b",
         Age == "All") %>% 
  mutate(baseline = baseline / 1000,
         Deaths = Deaths / 1000) %>% 
  ggplot()+
  geom_point(aes(baseline, reorder(Country, Deaths)), col = "#2b8cbe", alpha = 0.5)+
  geom_jitter(aes(Deaths, Country), alpha = 0.5, width = 0, height = 0.1)+
  scale_x_continuous(labels = comma, breaks = seq(0, 800, 100))+
  annotate("point", x = 490, y = 2.5, alpha = 0.5, size = 2)+
  annotate("point", x = 490, y = 4, col = "#2b8cbe", alpha = 0.5, size = 2)+
  annotate("text", x = 510, y = 2.5, label = "Observed deaths", hjust = 0, size = 3.5)+
  annotate("text", x = 510, y = 4, col = "#2b8cbe", label = "Predicted baseline", hjust = 0, size = 3.5)+
  labs(title = "Death counts since Feb 24th 2020", x = "Death counts (in thousands)", y = "")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=12),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=1)
  )
ggsave(here("Figures", "fig_s11_deaths_baseline.png"), dpi = 600, width = 6, height = 6)

