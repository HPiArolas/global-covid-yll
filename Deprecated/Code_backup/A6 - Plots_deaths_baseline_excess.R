# Description:
# Plots of excess mortality estimates

source(here("Code/A0 - Functions.R"))

if (!dir.exists(here("Figures"))){
  dir.create(here("Figures"))
}


db <- read_csv(here("Data", "covid_excess_pclm_5_ages_0_95.csv"))

db %>% 
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

db %>% 
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
