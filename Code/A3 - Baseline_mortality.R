# Description:
# Estimate baseline mortality in all countries by sex and age 


source(here("Code/A0 - Functions.R"))

# Creating the master database with weekly deaths and exposures
###############################################################
ctr_codes <- read_csv(here("Data", "country_codes.csv")) %>% 
  mutate(PopCode = ifelse(PopCode == "AUS2", "AUS", PopCode))
db_d <- read_rds(here("Data", "deaths_stmf_age5.rds")) %>% 
  mutate(Country = ifelse(Country == "Scothland", "Scotland", Country))
db_p <- read_rds(here("Data", "pop_interpol_week_age5.rds"))

unique(db_d$PopCode) %>% sort()
unique(db_p$Country) %>% sort()

# Merging mortality data and population estimates
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
db_de <- db_dp %>% 
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

gc()


####################################################
# estimating baseline for each country, sex, and age
####################################################
# loading packages for baseline estimation
p_load(pkgs_bsl, character.only = TRUE)
select <- dplyr::select
# setting cores use for parallel processing
registerDoParallel(cores = 6)

if (!dir.exists(here("Data","single_est"))){
  dir.create(here("Data","baseline_by_country"))
}

# starting year of observation
ym <- 2010
cts <- unique(db_de$PopCode)
sxs <- unique(db_de$Sex)
ags <- unique(db_de$Age)

db_blns_all <- NULL
for (c in cts) {
  db_blns <- NULL
  for (s in sxs) {
    for (a in ags) {
      
      temp <- db_de %>% 
        filter(PopCode == c,
               Sex == s,
               Age == a) %>% 
        select(Year, Week, t, Deaths, Exposure, sn52, cs52, include)
      
      cat(paste(c, s, a, "\n", sep = "_"))
      
      temp2 <- fit_baseline(temp) %>% 
        mutate(PopCode = c,
               Sex = s,
               Age = a,
               Date = ISOweek::ISOweek2date(paste0(Year, "-W", sprintf("%02d",Week), "-7")),
               mx_b = 100000 * Baseline / Exposure,
               mx_b_u = 100000 * up / Exposure,
               mx_b_l = 100000 * lp / Exposure,
               mx_d = 100000 * Deaths / Exposure) 
      
      db_blns <- db_blns %>% 
        bind_rows(temp2)
      
      ## plots of estimates
      ## ~~~~~~~~~~~~~~~~~~
      # temp2 %>%
      #   ggplot()+
      #   geom_vline(xintercept = ymd("2020-04-03"), col = "#b80c09", alpha = 0.1, size = 5)+
      #   geom_line(aes(Date, mx_d), size = 0.4)+
      #   geom_ribbon(aes(Date, ymin = mx_b_l, ymax = mx_b_u), fill = "#01BAEF", alpha = 0.25)+
      #   geom_line(aes(Date, mx_b), col = "#01BAEF", alpha = 0.9, size = 0.6)+
      #   scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 year", date_labels = "%Y")+
      #   labs(title=paste0(c, "_", s, "_", a))+
      #   theme_bw()+
      #   theme(
      #     panel.grid.minor = element_blank(),
      #     plot.title = element_text(size=11),
      #     axis.text.x = element_text(size=8),
      #     axis.text.y = element_text(size=8),
      #     axis.title.x = element_text(size=10),
      #     axis.title.y = element_text(size=10))+
      # ggsave(paste0("Figures/excess_singles/", c, "_", s, "_", a, "_", ym, "knots3.png"), dpi = 300, width = 6, height = 4)
    }
  }
  db_blns <- db_blns %>% 
    mutate(PopCode = c)
  write_csv(db_blns, path = here("Data", "baseline_by_country", paste0(c, "_baseline", ym, ".csv")))
  db_blns_all <- bind_rows(db_blns_all, db_blns)
}

# write_rds(db_blns, "Data/baseline_mortality.rds")

detach(package:MASS)
