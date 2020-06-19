rm(list=ls())
pkgs <- c("tidyverse",
          "lubridate",
          "haven",
          "scales")

lapply(pkgs, require, character.only = T)

setwd("C:/Users/kikep/Dropbox/excess_deaths/")
db_all <- read_csv("data/baseline_excess_pclm_5.csv")

unique(db_all$Country)

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
  mutate(Age = "Aggr")

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
            last_week = max(Week)) %>% 
  ungroup()

db3 %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_errorbar(aes(xmin = excess_lp, xmax = excess_up, y = Country), col = "#e34a33", alpha = 0.6, size = 0.7, width = 0.5)+
  geom_point(aes(excess, Country), col = "#e34a33")+
  scale_x_continuous(breaks = seq(0, 70000, 10000), labels = comma)+
  labs(title = "Excess mortality since Feb 24th 2020", x = "Excess mortality")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=13),
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11)
  )
ggsave("figures/excess.png", dpi = 300, width = 6, height = 4)

db_plot %>% 
  filter(Sex == "b") %>% 
  ggplot()+
  geom_point(aes(baseline, Country), col = "#2b8cbe")+
  geom_point(aes(Deaths, Country))+
  geom_errorbar(aes(xmin = baseline_lp, xmax = baseline_up, y = Country), col = "#2b8cbe", alpha = 0.6, size = 0.7, width = 0.5)+
  scale_x_continuous(labels = comma)+
  annotate("point", x = 350000, y = 2.5)+
  annotate("point", x = 350000, y = 3.5, col = "#2b8cbe")+
  annotate("text", x = 360000, y = 2.5, label = "Observed deaths", hjust = 0, size = 3)+
  annotate("text", x = 360000, y = 3.5, col = "#2b8cbe", label = "Predicted baseline", hjust = 0, size = 3)+
  labs(title = "Death counts since Feb 24th 2020", x = "Death counts")+
  theme_bw()+
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size=13),
    axis.text.x = element_text(size=10),
    axis.text.y = element_text(size=10),
    axis.title.x = element_text(size=11),
    axis.title.y = element_text(size=11)
  )
ggsave("figures/deaths_baseline.png", dpi = 300, width = 6, height = 4)

