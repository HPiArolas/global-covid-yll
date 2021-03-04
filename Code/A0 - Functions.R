# Description:
# functions used for excess mortality scripts

Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL","English")

# install pacman to streamline further package installation
if(!require("pacman", character.only = TRUE)) {
  install.packages("pacman", dep = TRUE)
  if (!require("pacman", character.only = TRUE))
    stop("Package pacman not found")
}

library(pacman)
# Required CRAN packages
pkgs <- c("tidyverse",
          "here",
          "lubridate",
          "readxl",
          "vroom",
          "ungroup",
          "HMDHFDplus",
          "parallel",
          "parallelsugar",
          "ISOweek",
          "scales",
          "remotes",
          "osfr")

# required packages for baseline estimation
pkgs_bsl <- c("stats", 
              "splines",
              "MASS",
              "gnm",
              'doParallel', 
              'foreach')

packages_git <- c("parallelsugar")


# Install required CRAN packages if not available yet
if(!sum(!p_isinstalled(pkgs))==0) {
  p_install(
    package = pkgs[!p_isinstalled(pkgs)], 
    character.only = TRUE
  )
}

if(!sum(!p_isinstalled(pkgs_bsl))==0) {
  p_install(
    package = pkgs_bsl[!p_isinstalled(pkgs_bsl)], 
    character.only = TRUE
  )
}

# install from github if necessary
if (!p_isinstalled("parallelsugar")){
  library(remotes)
  install_github("nathanvan/parallelsugar")
}

# loading basic packages
p_load(pkgs, character.only = TRUE)
# loading github packages
p_load(packages_git, character.only = TRUE)


# HMD user and password
if(!'us' %in% ls()){
  cat("Please instert your HMD User:")
  us <- userInput()
}
if(!'pw' %in% ls()){
  cat("Please instert your HMD password:")
  pw <- userInput()
}

interpop <- function(db){
  ys <- db %>% drop_na() %>% pull(t)
  ps <- db %>% drop_na() %>% pull(Pop)
  ws <- db %>% pull(t)
  md2 <- smooth.spline(x = ys, y = ps)
  inter_pop <- tibble(t = ws,
                      Pop2 = predict(md2, ws)$y)
  return(inter_pop)
}

pad_offsets <- function(chunk){
  MY <- max(chunk$Year)
  if (MY < 2020){
    X <- chunk %>% 
      filter(Year == MY)
    extend <- list()
    for (i in 1:(2020-MY)){
      extend[[i]] <- X %>% 
        mutate(Year = Year + i)
    }
    extend <- bind_rows(extend)
    chunk <-
      chunk %>% 
      bind_rows(extend)
  }
  chunk
}

# data prep function preamble
dist_unk <- function(chunk){
  if ("UNK" %in% chunk$Age){
    UNK   <- chunk %>% filter(Age == "UNK") %>% pull(Deaths)
    chunk <- chunk %>% filter(Age != "UNK") %>% 
      mutate(Deaths = Deaths + UNK * Deaths / sum(Deaths) )
  }
  chunk
}

dist_tot <- function(chunk){
  if ("TOT" %in% chunk$Age){
    TOT   <- chunk %>% filter(Age == "TOT") %>% pull(Deaths)
    chunk <- chunk %>% filter(! Age %in% c("TOT","UNK")) %>% 
      mutate(Deaths = TOT * Deaths / sum(Deaths) )
  }
  chunk
}

# within Country, Year, Sex, Age
dist_unk_week <- function(chunk){
  if ("UNK" %in% chunk$Week){
    UNK   <- chunk %>% filter(Week == "UNK") %>% pull(Deaths)
    chunk <- chunk %>% filter(Week != "UNK") %>% 
      mutate(Deaths = Deaths + UNK * Deaths / sum(Deaths) )
  }
  chunk
}

scale_sexes <- function(chunk){
  if (setequal(c("m","f","b"),unique(chunk$Sex))){
    cnames <- colnames(chunk)
    chunk <-
      chunk %>% 
      pivot_wider(names_from = Sex,
                  values_from = Deaths) %>% 
      mutate(m = m / (m + f) * b,
             f = f / (m + f) * b,
             m = ifelse(is.nan(m),0,m),
             f = ifelse(is.nan(f),0,f)) %>% 
      pivot_longer(m:b, names_to = "Sex", values_to = "Deaths") %>% 
      select(all_of(cnames))
  }
  chunk
}


# Age harmonization functions
#############################
harmonize_age <- function(chunk, Offsets = NULL, N = 5, OAnew = 110){
  Age     <- chunk %>% pull(Age)
  AgeInt  <- chunk %>% pull(AgeInt)
  Deaths   <- chunk %>% pull(Deaths) 
  
  # maybe we don't need to do anything but lower the OAG?
  if (all(AgeInt == N) & max(Age) >= OAnew){
    Deaths   <- groupOAG(Deaths, Age, OAnew = OAnew)
    Age      <- Age[1:length(Deaths)]
    AgeInt   <- AgeInt[1:length(Deaths)]
    return(select(chunk, Age, AgeInt, Deaths))
  }
  # --------------------------------- #
  # otherwise get offset sorted out.  #
  # Offsets now handled in advance    #
  # --------------------------------- #
  .Country <- chunk %>% pull(Country) %>% "["(1)
  .Sex     <- chunk %>% pull(Sex) %>% "["(1)
  .Year     <- chunk %>% pull(Year) %>% "["(1)
  if (!is.null(Offsets)){
    Offset   <- Offsets %>% 
      filter(Country == .Country,
             Year == .Year,
             Sex == .Sex)
  } else {
    Offset <- tibble()
  }
  
  if (nrow(Offset) == 111){
    pop     <- Offset %>% pull(Population)
    age_pop <- Offset %>% pull(Age)
    
    # so need to rescale in next step (pattern looks OK)
    # V1      <- pclm(x = Age,
    #                 y = Deaths,
    #                 nlast = AgeInt[length(AgeInt)],
    #                 offset = pop,
    #                 control = list(lambda = 1000, deg = 3))$fitted * pop
    
    # with BIC optimization
    V1      <- pclm(x = Age,
                    y = Deaths,
                    nlast = AgeInt[length(AgeInt)],
                    offset = pop)$fitted * pop
  }  else {
    # if no offsets are available then run through without.
    # V1      <- pclm(x = Age, 
    #                 y = Deaths, 
    #                 nlast = AgeInt[length(AgeInt)], 
    #                 control = list(lambda = 1000, deg = 3))$fitted
    # with BIC optimization
    V1      <- pclm(x = Age, 
                    y = Deaths, 
                    nlast = AgeInt[length(AgeInt)])$fitted
  }
  
  # Important to rescale
  V1      <- rescaleAgeGroups(Value1 = V1, 
                              AgeInt1 = rep(1,length(V1)), 
                              Value2 = Deaths, 
                              AgeInt2 = AgeInt, 
                              splitfun = graduate_uniform)
  
  # division by 0, it's a thing
  V1[is.nan(V1)] <- 0
  
  VN      <- groupAges(V1, 0:110, N = N, OAnew = OAnew)
  Age     <- names2age(VN)
  AgeInt  <- rep(N, length(VN))
  
  tibble(Age = Age, AgeInt = AgeInt, Deaths = VN)
}

# for doing the above in mclapply()
harmonize_age_p <- function(chunk, Offsets){
  cnames   <- colnames(chunk)
  .PopCode <- chunk %>% pull(PopCode) %>% '['(1)
  .Country <- chunk %>% pull(Country) %>% '['(1)
  .Year    <- chunk %>% pull(Year) %>% '['(1)
  .Week    <- chunk %>% pull(Week) %>% '['(1)
  .Sex     <- chunk %>% pull(Sex) %>% '['(1)
  out <- harmonize_age(chunk, Offsets, N = 5, OAnew = 100)
  out %>% 
    mutate(PopCode = .PopCode,
           Country = .Country,
           Year = .Year,
           Week = .Week,
           Sex = .Sex) %>% 
    select(all_of(cnames))
}

# let's not get hung up on errors...
try_harmonize_age_p <- function(X,Offsets){
  Y <- try(harmonize_age_p(X,Offsets))
  if (class(Y)[1] == "try-error"){
    cat(X$Country[1],X$Year[1],X$Week[1],X$Sex[1],"\n")
    return(X[0,])
  }
  Y
}

# function to ungroup last age group (90+) of baseline estimates into groups 
# age groups 90-94 and 95+
ungr_last <- function(a, d){
  val90 <- d[length(d)]
  V1      <- pclm(x = a, 
                  y = d, 
                  nlast = 20)$fitted
  chunk2 <- 
    tibble(Age = 0:109, outs = V1) %>% 
    mutate(Age = floor(Age / 5) * 5,
           Age = ifelse(Age > 95, 95, Age )) %>% 
    group_by(Age) %>% 
    summarise(outs = sum(outs))
  
  chunk3 <- 
    chunk2 %>% 
    filter(Age >= 90) %>% 
    mutate(all90_95 = sum(outs),
           to_dist = val90,
           outs2 = to_dist * (outs / all90_95)) %>% 
    pull(outs2)
}



##################################
### functions for excess mortality
##################################

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
  return(data.frame(Baseline = predict(model, newdata = pdata, type = "response"), 
                    lp = boot_ci[, 1], 
                    up = boot_ci[, 2]))
}


### function for fitting model for each Region, sex, and age
#############################################################

# ct <- "Spain"
# sx <- "b"
# ag <- 80
# ymin <- 2014
# db2 <- temp

fit_baseline <- function(db2) {
  # db2 <- temp
  skip_to_next <- F
  
  # # data to include in the model 
  db_bline <- db2 %>%
    filter(include == 1)
  
  # model fitting evaluation
  ##########################
  # evaluate the seasonal parameter with AIC
  train_base <- db_bline %>% 
    filter(row_number() <= floor(nrow(db_bline)/2))
  
  valid_base <- db_bline %>% 
    filter(row_number() > floor(nrow(db_bline)/2))
  
  no_sea1 = gnm(Deaths ~ ns(t, 3) + offset(log(Exposure)), 
                data = train_base, 
                family = poisson(link="log"))
  
  no_sea2 = gnm(Deaths ~ ns(t, 3) + offset(log(Exposure)), 
                data = valid_base, 
                contrain = "*", 
                contrainTo = coef(reg1),
                family=poisson(link="log"))
  
  sea1 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
             data = train_base, 
             family = poisson(link="log"))
  
  sea2 = gnm(Deaths ~ ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
             data = valid_base, 
             contrain = "*", 
             contrainTo = coef(reg1),
             family=poisson(link="log"))
  
  if (no_sea2$aic - sea2$aic > 6) {
    # evaluating for overdispersion adjustment for seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[1] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + sn52 + cs52 + offset(log(Exposure)), 
                  family = poisson, data = db_bline)
    }
  } else {
    # evaluating for overdispersion adjustment for non-seasonal model
    # Poisson model
    base_po <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                   family = poisson, data = db_bline)
    
    # Negative binomial to account for overdispersion
    base_nb <- try(MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                                data = db_bline), silent = T)
    ov_sign <- try(base_nb$theta / base_nb$SE.theta > 1.96, silent = T)
    if (class(base_nb)[1] == "try-error" | is.na(ov_sign)) {
      base_nb$aic <- base_po$aic
      base_nb$converged <- F
      ov_sign <- F
    }
    # compare AIC between Poisson and Negative binomial and fit the best model
    if ((base_po$aic - base_nb$aic >= 6) & (ov_sign) & (base_nb$converged) & class(base_nb)[3] != "try-error") {
      base <- MASS::glm.nb(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                           data = db_bline)
    } else {
      base <- glm(Deaths ~ splines::ns(t, 3) + offset(log(Exposure)), 
                  family = poisson, data = db_bline)
    }
  }
  
  # predicting values and 95% confidence intervals
  ################################################
  
  # bootstrapping
  tryCatch({
    db3 <- cbind(db2, 
                 boot_pi(base, db_bline, db2, 2000, 0.95))
  }, error=function(e){ skip_to_next <<- TRUE})
  
  if(skip_to_next) { next } 
  
  return(db3)
}
