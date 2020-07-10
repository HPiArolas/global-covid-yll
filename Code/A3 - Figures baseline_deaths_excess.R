###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Figures of Excess mortality estimates


if (!dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}

db_all <- read_csv(here("Data", "baseline_excess_pclm_5.csv"))

unique(db_all$Country)

# HMD: "Data for 2020 is preliminary and for the last 1-3 available weeks may be incomplete"

max_week <- db_all %>%  
  filter(Year == 2020) %>% 
  group_by(Country) %>% 
  summarise(last_week = max(Week)) %>% 
  mutate(last_week = last_week - 3) %>% 
  ungroup()
  
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
            lp = sum(lp),
            up = sum(up),
            last_week = max(Week)) %>% 
  ungroup()

db3 <- db2 %>% 
  group_by(Country, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            baseline = sum(baseline),
            excess = sum(excess),
            excess_lp = sum(excess_lp),
            excess_up = sum(excess_up),
            lp = sum(lp),
            up = sum(up),
            last_week = max(last_week)) %>% 
  mutate(Age = "All")

db_plot <- db_all %>% 
  left_join(max_week) %>%
  filter(date >= "2020-02-24",
         Week <= last_week) %>% 
  mutate(Deaths = ifelse(Deaths - up > 0, Deaths, up)) %>% 
  group_by(Country, Sex) %>% 
  summarise(Deaths = sum(Deaths),
            baseline = sum(pred),
            baseline_lp = sum(lp),
            baseline_up = sum(up),
            last_week = max(last_week)) %>% 
  ungroup()

db3 %>% 
  filter(Sex == "b") %>% 
  mutate(excess_lp = excess_lp / 1000,
         excess_up = excess_up / 1000,
         excess = excess / 1000) %>% 
  ggplot()+
  geom_errorbar(aes(xmin = excess_lp, xmax = excess_up, y = Country), col = "#F05142", alpha = 0.9, size = 0.6, width = 0.4)+
  geom_point(aes(excess, Country), col = "#E22412")+
  scale_x_continuous(breaks = seq(0, 1500, 10), labels = comma)+
  labs(title = "Excess mortality since Feb 24th 2020", x = "Excess mortality (in thousands)")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=12),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11)
  )
ggsave(here("Figures",
            "excess.png"), dpi = 600, width = 6, height = 4)

db_plot %>% 
  filter(Sex == "b") %>% 
  mutate(baseline = baseline / 1000,
         Deaths = Deaths / 1000,
         baseline_lp = baseline_lp / 1000,
         baseline_up = baseline_up / 1000) %>% 
  ggplot()+
  geom_point(aes(baseline, Country), col = "#2b8cbe")+
  geom_point(aes(Deaths, Country))+
  geom_errorbar(aes(xmin = baseline_lp, xmax = baseline_up, y = Country), col = "#2b8cbe", alpha = 0.8, size = 0.7, width = 0.5)+
  scale_x_continuous(labels = comma)+
  annotate("point", x = 600, y = 2.5)+
  annotate("point", x = 600, y = 3.5, col = "#2b8cbe")+
  annotate("text", x = 610, y = 2.5, label = "Observed deaths", hjust = 0, size = 3)+
  annotate("text", x = 610, y = 3.5, col = "#2b8cbe", label = "Predicted baseline", hjust = 0, size = 3)+
  labs(title = "Death counts since Feb 24th 2020", x = "Death counts (in thousands)")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=12),
    axis.text.x = element_text(size=8),
    axis.text.y = element_text(size=8),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11)
  )
ggsave(here("Figures",
            "deaths_baseline.png"), dpi = 600, width = 6, height = 4)

