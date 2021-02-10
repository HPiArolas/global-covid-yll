# Description:
# Weekly exposures interpolation

library(here)
source(here("Code", "A00_functions.R"))
ctr_codes <- read_csv(here("Data", "country_codes.csv"))

# Country names and codes 
#########################
pcodes <- unzip(here("Data", "STMFinput.zip"), list = TRUE) %>% 
  mutate(Name2 = str_replace(Name, "stmf.csv", "")) %>% 
  dplyr::pull(Name2)

ctrs <- ctr_codes %>% 
  filter(PopCode %in% c(pcodes, "GBR")) %>% 
  drop_na() %>% 
  dplyr::pull(wpp_code)


# loading annual and single-year of age population estimates from the WPP
#########################################################################

# female and male estimates stored in the OSF project
osf_retrieve_file("wxpdz") %>%
  osf_download(conflicts = "overwrite",
               path = "Data")

osf_retrieve_file("tn4zh") %>%
  osf_download(conflicts = "overwrite",
               path = "Data") 

pop_m <- read_xlsx(unzip(here("Data", "wpp_m.zip")),
                   skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  filter(Country %in% ctrs, 
         Year >= 2000) %>% 
  gather(-Country, -Year, key = "Age", value = "Pop") %>% 
  mutate(Age = as.integer(Age),
         Sex = "m")

pop_f <- read_xlsx(unzip(here("Data", "wpp_f.zip")),
                   skip = 16) %>% 
  select(3, 8:109) %>% 
  rename(Country = 1, 
         Year = 2) %>% 
  filter(Country %in% ctrs, 
         Year >= 2000) %>% 
  gather(-Country, -Year, key = "Age", value = "Pop") %>% 
  mutate(Age = as.integer(Age),
         Sex = "f")

file.remove(here("wpp_f.xlsx"), 
            here("wpp_m.xlsx"),
            here("Data", "wpp_f.zip"), 
            here("Data", "wpp_m.zip"))

pop_wpp <- bind_rows(pop_m, pop_f) %>% 
  mutate(Pop = as.numeric(Pop) * 1000) 

unique(pop_wpp$Country) %>% sort()

# loading annual and single-year of age population estimates from the HMD
# for England, Wales, Scotland, Northern Ireland, and Taiwan 
#########################################################################

HMDcountries <- c("GBR_NIR", "GBR_SCO", "GBRTENW", "TWN")
countries <- ctr_codes %>% 
  filter(PopCode %in% HMDcountries) %>% 
  pull(Country)
names(countries) <- HMDcountries

OffsetsL <- lapply(HMDcountries, function(xyz, us, pw, countries){
  X         <- readHMDweb(xyz, "Exposures_1x1", us, pw)
  X %>% 
    filter(Year >= 2000) %>% 
    pivot_longer(Female:Total, names_to = "Sex", values_to = "Pop") %>% 
    mutate(Country = countries[xyz],
           Sex = case_when(Sex == "Male" ~ "m",
                           Sex == "Female" ~ "f",
                           Sex == "Total" ~ "b"),
           Pop = as.numeric(Pop)) %>% 
    arrange(Year, Sex, Age) %>% 
    select(Country, Year, Sex, Age, Pop)
  
}, us = us, pw = pw, countries = countries)

names(OffsetsL) <- countries

pop_hmd <- 
  OffsetsL %>% 
  bind_rows() %>% 
  group_by(Country, Sex) %>% 
  do(pad_offsets(chunk = .data)) %>% 
  ungroup() %>% 
  mutate(Age = ifelse(Age >= 100, 100, Age)) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup() %>% 
  filter(Sex != "b")

pop_all <- 
  bind_rows(pop_hmd, pop_wpp) %>% 
  mutate(Age = floor(Age / 5) * 5) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(Pop = sum(Pop),
            Week = 27) %>%
  ungroup()
  
###########################################################
# Interpolating population estimates to weeks using splines
###########################################################

# weeks by year between 2000 and 2020
db_w <- expand_grid(Year = 2000:2020, Week = 1:52) %>% 
  bind_rows(tibble(Year = c(2004, 2009, 2015, 2020), Week = 53)) %>% 
  arrange(Year, Week) %>% 
  mutate(t = 1:n())

ages <- unique(pop_all$Age)

ctrs <- unique(pop_all$Country)

inters_pop <- NULL

for(c in ctrs){
  for(s in c("m", "f")){
    pop_temp <- pop_all %>% 
      filter(Country == c,
             Sex == s)
    for(a in ages){
      
      db_w_temp <- db_w %>% 
        mutate(Country = c,
               Sex = s,
               Age = a) %>% 
        left_join(pop_temp)
      
      db_w_temp2 <- db_w_temp %>% 
        left_join(interpop(db_w_temp)) %>% 
        mutate(Country = c,
               Age = a,
               Sex = s)
      
      inters_pop <- inters_pop %>% 
        bind_rows(db_w_temp2)
      
    }
  }
}

unique(inters_pop$Country)

# Visual test
#############
c <- "Austria"
a <- 0
s <- "f"

inters_pop %>% 
  filter(Country == c,
         Age == a,
         Sex == s) %>% 
  ggplot()+
  geom_line(aes(t, Pop2), col = "black")+
  geom_point(aes(t, Pop), col = "red")
  
# closing age at 90
inters_pop2 <- inters_pop %>% 
  mutate(Age = ifelse(Age > 90, 90, Age)) %>% 
  group_by(Country, Year, Week, t, Sex, Age) %>% 
  summarise(Pop = sum(Pop2)) %>% 
  ungroup()
           
# population estimates for all sex
inters_popt <- inters_pop2 %>% 
  group_by(Year, Week, t, Country, Age) %>% 
  summarise(Pop = sum(Pop)) %>% 
  ungroup() %>% 
  mutate(Sex = "b")

inters_pop3 <- bind_rows(inters_pop2,
                         inters_popt) %>% 
  arrange(Country, Sex, Age, t) %>% 
  select(-t)

write_rds(inters_pop3, here("Data", "pop_interpol_week_age5.rds"))
unique(inters_pop3$Country) %>% sort()


