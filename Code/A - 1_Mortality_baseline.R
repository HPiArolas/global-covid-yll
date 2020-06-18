###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Calculates EXCESS deaths from weekly mortality baselines in all countries by sex and age 
# rm(list=ls())

if (!dir.exists(here("Data","single_est"))){
  dir.create(here("Data","single_est"))
}


###################################################################################
# reading data of weekly mortality in 5-years age groups and exposures from the HMD
###################################################################################

db_d <- read_rds(here("Data", "stmf_5.rds"))
db_e <- read_rds(here("Data", "OffsetsHMD.rds"))

db_e2 <- db_e %>% 
  mutate(Age = floor(Age / 5) * 5,
         Age = ifelse(Age >= 100, 100, Age)) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(e = sum(Population)) %>% 
  ungroup()

db <- db_d %>% 
  group_by(Country, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  filter(Year >= 2014) %>% 
  left_join(db_e2) %>% 
  rename(exposure = e) %>% 
  ungroup() %>% 
  mutate(Deaths = ifelse(Deaths == 0, 1, Deaths)) 

# minimum year to include
ym <- 2014
skip_to_next <- F

cts <- unique(db$Country)
sxs <- unique(db$Sex)
ags <- unique(db$Age)

registerDoParallel(cores = 4)

##############################
### function for bootstrapping 
##############################

boot_pi <- function(model, odata, pdata, n, p) {
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(2020)
  seeds <- round(runif(n, 1, 1000), 0)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = pdata, type = "response"), 
                    lp = boot_ci[, 1], 
                    up = boot_ci[, 2]))
}

#############################################################
### function for fitting model for each country, sex, and age
#############################################################

fit_baseline <- function(ct = c, sx = s, ag = a, ymin = ym) {

  cat(paste(ct, sx, ag, "\n", sep = "_"))
  skip_to_next <- F

  db2 <- db %>% 
    filter(Country == ct,
           Sex == sx,
           Age == ag,
           Year >= ymin,
           Week <= 52) %>% 
    arrange(Year, Week) %>% 
    mutate(Deaths = as.integer(Deaths),
           # adding time trend and seasonal components
           t = row_number(),
           sn52 = sin((2*pi*t)/(52)),
           cs52 = cos((2*pi*t)/(52)),
           # excluding winter (wks 46-14), summer (wks 27-35), 2009 and COVID-19 pandemics
           include = ifelse(((Week >= 15 & Week <= 26) |
                               (Week >= 36 & Week <= 45)) &
                              (Year != 2020 & Year != 2009),
                            1, 0))
  
  # data to include in the model 
  db_bline <- db2 %>% 
    filter(include == 1)
  
  ##########################
  # model fitting evaluation
  ##########################
  # evaluate the seasonal parameter with AIC
  train_base <- db_bline %>% 
    filter(row_number() <= floor(nrow(db_bline)/2))
  
  valid_base <- db_bline %>% 
    filter(row_number() > floor(nrow(db_bline)/2))
  
  no_sea1 = gnm(Deaths ~ ns(t, 3) + offset(log(exposure)), 
                data = train_base, 
                family = poisson(link="log"))
  
  no_sea2 = gnm(Deaths ~ ns(t, 3) + offset(log(exposure)), 
                data = valid_base, 
                contrain = "*", 
                contrainTo = coef(reg1),
                family=poisson(link="log"))
  
  sea1 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
             data = train_base, 
             family = poisson(link="log"))
  
  sea2 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
             data = valid_base, 
             contrain = "*", 
             contrainTo = coef(reg1),
             family=poisson(link="log"))
  
  if (no_sea2$aic - sea2$aic > 6) {
    # evaluating for overdispersion adjustment for seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                   family = poisson, data = db_bline)
    
    # negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                          data = db_bline, maxit = 1000), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                     data = db_bline, maxit = 1000)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                   family = poisson, data = db_bline)
    
    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                          data = db_bline, maxit = 1000), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                     data = db_bline, maxit = 1000)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                  family = poisson, data = db_bline)
    }
  }

  
  ################################################
  # predicting values and 95% confidence intervals
  ################################################
  
  # bootstrapping
  tryCatch({
  db3 <- cbind(db2, 
               boot_pi(base, db_bline, db2, 2000, 0.95))
  }, error=function(e){ skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
  db4 <- db3 %>% 
    mutate(date = as.Date(paste(Year, Week, 1, sep="-"), "%Y-%U-%u"),
           m_pred = pred / exposure,
           excess = Deaths - pred,
           m_excess = excess / exposure,
           exc_reg_pi = ifelse(Deaths > up, 1, 0)) %>% 
    dplyr::select(Country, date, everything())
  
  write_csv(db4, path = here("Data",
                             "single_est", 
                             paste0(ct, "_", sx, "_", ag, "_weekly_mortality_tibble.csv")))
  return(db4)
}

####################################################
# estimating baseline for each country, sex, and age
####################################################

for (c in cts) {
  dbc <- db %>%
    filter(Country == c)
  sxs <- unique(dbc$Sex)
  for (s in sxs) {
    for (a in ags) {
      cat(paste(c, s, a, "\n", sep = "_"))
      fit_baseline(ct = c, sx = s, ag = a, ymin = ym)
    }
  }
}

#########################
# appending all estimates
#########################

db_all <- NULL

temp = list.files(here("Data","single_est"), pattern="*.csv")

# i <- 1
length(temp)
for (i in 1:length(temp)) {
  db_temp <- read_csv(temp[i]) %>% 
    as_tibble() %>% 
    mutate(Sex = as.character(Sex),
           Sex = ifelse(Sex == "FALSE", "f", Sex))
  db_all <- bind_rows(db_all, db_temp)
}

detach(package:MASS)

write_csv(db_all, path = here("Data", "baseline_excess_pclm_5.csv"))

###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Calculates EXCESS deaths from weekly mortality baselines in all countries by sex and age


###################################################################################
# reading data of weekly mortality in 5-years age groups and exposures from the HMD
###################################################################################

db_d <- read_rds(here("Data", "stmf_5.rds"))
db_e <- read_rds(here("Data", "OffsetsHMD.rds"))

db_e2 <- db_e %>% 
  mutate(Age = floor(Age / 5) * 5,
         Age = ifelse(Age >= 100, 100, Age)) %>% 
  group_by(Country, Year, Sex, Age) %>% 
  summarise(e = sum(Population)) %>% 
  ungroup()

db <- db_d %>% 
  group_by(Country, Year, Week, Sex, Age) %>% 
  summarise(Deaths = sum(Deaths)) %>% 
  filter(Year >= 2014) %>% 
  left_join(db_e2) %>% 
  rename(exposure = e) %>% 
  ungroup() %>% 
  mutate(Deaths = ifelse(Deaths == 0, 1, Deaths)) 

# minimum year to include
ym <- 2014
skip_to_next <- F

cts <- unique(db$Country)
sxs <- unique(db$Sex)
ags <- unique(db$Age)

registerDoParallel(cores = 4)

##############################
### function for bootstrapping 
##############################

boot_pi <- function(model, odata, pdata, n, p) {
  lp <- (1 - p) / 2
  up <- 1 - lp
  set.seed(2020)
  seeds <- round(runif(n, 1, 1000), 0)
  boot_y <- foreach(i = 1:n, .combine = rbind) %dopar% {
    set.seed(seeds[i])
    bdata <- odata[sample(seq(nrow(odata)), size = nrow(odata), replace = TRUE), ]
    bpred <- predict(update(model, data = bdata), type = "response", newdata = pdata)
    rpois(length(bpred), lambda = bpred)
  }
  boot_ci <- t(apply(boot_y, 2, quantile, c(lp, up)))
  return(data.frame(pred = predict(model, newdata = pdata, type = "response"), 
                    lp = boot_ci[, 1], 
                    up = boot_ci[, 2]))
}

#############################################################
### function for fitting model for each country, sex, and age
#############################################################

fit_baseline <- function(ct = c, sx = s, ag = a, ymin = ym) {

  cat(paste(ct, sx, ag, "\n", sep = "_"))
  skip_to_next <- F

  db2 <- db %>% 
    filter(Country == ct,
           Sex == sx,
           Age == ag,
           Year >= ymin,
           Week <= 52) %>% 
    arrange(Year, Week) %>% 
    mutate(Deaths = as.integer(Deaths),
           # adding time trend and seasonal components
           t = row_number(),
           sn52 = sin((2*pi*t)/(52)),
           cs52 = cos((2*pi*t)/(52)),
           # excluding winter (wks 46-14), summer (wks 27-35), 2009 and COVID-19 pandemics
           include = ifelse(((Week >= 15 & Week <= 26) |
                               (Week >= 36 & Week <= 45)) &
                              (Year != 2020 & Year != 2009),
                            1, 0))
  
  # data to include in the model 
  db_bline <- db2 %>% 
    filter(include == 1)
  
  ##########################
  # model fitting evaluation
  ##########################
  # evaluate the seasonal parameter with AIC
  train_base <- db_bline %>% 
    filter(row_number() <= floor(nrow(db_bline)/2))
  
  valid_base <- db_bline %>% 
    filter(row_number() > floor(nrow(db_bline)/2))
  
  no_sea1 = gnm(Deaths ~ ns(t, 3) + offset(log(exposure)), 
                data = train_base, 
                family = poisson(link="log"))
  
  no_sea2 = gnm(Deaths ~ ns(t, 3) + offset(log(exposure)), 
                data = valid_base, 
                contrain = "*", 
                contrainTo = coef(reg1),
                family=poisson(link="log"))
  
  sea1 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
             data = train_base, 
             family = poisson(link="log"))
  
  sea2 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
             data = valid_base, 
             contrain = "*", 
             contrainTo = coef(reg1),
             family=poisson(link="log"))
  
  if (no_sea2$aic - sea2$aic > 6) {
    # evaluating for overdispersion adjustment for seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                   family = poisson, data = db_bline)
    
    # negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                          data = db_bline, maxit = 1000), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                     data = db_bline, maxit = 1000)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(exposure)), 
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                   family = poisson, data = db_bline)
    
    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                          data = db_bline, maxit = 1000), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                     data = db_bline, maxit = 1000)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + offset(log(exposure)), 
                  family = poisson, data = db_bline)
    }
  }

  
  ################################################
  # predicting values and 95% confidence intervals
  ################################################
  
  # bootstrapping
  tryCatch({
  db3 <- cbind(db2, 
               boot_pi(base, db_bline, db2, 2000, 0.95))
  }, error=function(e){ skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
  db4 <- db3 %>% 
    mutate(date = as.Date(paste(Year, Week, 1, sep="-"), "%Y-%U-%u"),
           m_pred = pred / exposure,
           excess = Deaths - pred,
           m_excess = excess / exposure,
           exc_reg_pi = ifelse(Deaths > up, 1, 0)) %>% 
    dplyr::select(Country, date, everything())
  
  write_csv(db4, file = here("Data/single_est/", paste0(ct, "_", sx, "_", ag, "_weekly_mortality_tibble.csv")))
  return(db4)
}

####################################################
# estimating baseline for each country, sex, and age
####################################################

for (c in cts) {
  dbc <- db %>%
    filter(Country == c)
  sxs <- unique(dbc$Sex)
  for (s in sxs) {
    for (a in ags) {
      cat(paste(c, s, a, "\n", sep = "_"))
      fit_baseline(ct = c, sx = s, ag = a, ymin = ym)
    }
  }
}

#########################
# appending all estimates
#########################

db_all <- NULL

temp = list.files(here("Data","single_est"), pattern="*.csv")

# i <- 1
length(temp)
for (i in 1:length(temp)) {
  db_temp <- read_csv(temp[i]) %>% 
    as_tibble() %>% 
    mutate(Sex = as.character(Sex),
           Sex = ifelse(Sex == "FALSE", "f", Sex))
  db_all <- bind_rows(db_all, db_temp)
}

detach(package:MASS)

write_csv(db_all, file = here("Data", "baseline_excess_pclm_5.csv"))

