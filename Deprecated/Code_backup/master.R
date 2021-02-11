

###################################################################################################
##
##  LIBRARIES
##  
###################################################################################################

# All of these packages are on CRAN
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(wpp2019)
library(readr)
library(countrycode)
library(paletteer) 
library(grid)
library(here)
library(lubridate)
library(readxl) 
library(doParallel) 
library(foreach)
library(stats)
library(splines)
library(MASS)
library(gnm)
library(scales)

here <- here::here

# 1) 
source(here("Code","A1 - Mortality_baseline.R"))

# 2) 
source(here("Code","A2 - Excess_mortality_2020.R"))

# 3) 
source(here("Code","A3 - Figures baseline_deaths_excess.R"))

# 4) 
source(here("Code","B - Excess YLL.R"))

# 5)
source(here("Code","B - Other Causes YLL.R"))

# 6)
source(here("Code","B - Projected YLL.R"))

# 7)
source(here("Code","C - Compilation final measures.R"))

# 8)
source(here("Code","C - Figures.R"))

# 9)
source(here("Code","C - Final tables.R"))







