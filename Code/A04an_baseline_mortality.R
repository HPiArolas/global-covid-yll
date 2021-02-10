# Description:
# Estimate baseline mortality in all countries by sex and age 

source(here("Code/A00_functions.R"))
# loading packages for baseline estimation
p_load(pkgs_bsl, character.only = TRUE)
select <- dplyr::select
registerDoParallel(cores = 6)

if (!dir.exists(here("Data","single_est"))){
  dir.create(here("Data","baseline_by_country"))
}

# reading weekly mortality data
db_de <- read_rds(here("Data", "db_for_baseline_age5.rds"))

####################################################
# estimating baseline for each country, sex, and age
####################################################
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
