###################################################################################################
###################################################################################################
##
##  COVID-19 YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Calculates PROJECTED years of life lost (YLL) and related measures 



# Notes:
# Data cleaning should be done in the data preparation files for the last version.
# This includes issues with covid-19 death count dataset.


###################################################################################################
##
##  DATA
##  
##
###################################################################################################
# COVID-19 deaths
###################################################################################################
out <- readRDS(here("Data","deathcounts_clean12-6-2020.rds"))

# COVID-19 projected deaths
###################################################################################################

projected <- readRDS(here("Data","projections_covid_deaths.rds"))

# SLE
###################################################################################################
# GBD life expectancy best case standard
sle.gbd       <- readRDS(here("Data","country_gbd_sle.2016.rds"))
# Country specific life expectancies
sle_both.un   <- readRDS(here('Data','country_both_sle_un.rds'))
sle_male.un   <- readRDS(here('Data','country_male_sle_un.rds'))
sle_female.un <- readRDS(here('Data','country_female_sle_un.rds'))

# Population
###################################################################################################
pop<-read.csv(here('Data','pop_complete.csv'),row.names = 1)

# List of countries
###################################################################################################
# Country list file with full sample considered
our_sample <- read.csv(here('Data','full sample list.csv'))
our_sample <- as.character(data.frame(our_sample)[,1])
countries  <- our_sample

# Standard population weights
###################################################################################################
#  European standard
pop_std.gbd      <- readRDS(here("Data","pop_std.gbd.RDS"))
#  GBD standard
pop_std.esp      <- readRDS(here("Data","pop_std.esp.RDS"))

###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables

# Data holders
yll.data.both<-vector("list",length=length(countries))

#i<-30

##  Both sexes
###################################################################################################
for(i in 1:length(countries)){
  cat("Country=",countries[i],"\n")
  # choose national level data for both genders
  c<-subset(projected,as.character(Country)==countries[i])
  d<-subset(out,as.character(Country)==countries[i])
  d<-subset(d,Date==max(d$Date))
  Age_Projected_Min<-(d$Deaths/sum(d$Deaths))*c$proj.deaths.min
  Age_Projected_Max<-(d$Deaths/sum(d$Deaths))*c$proj.deaths.max
  #this second part is accessing the country's projected death value in projected, such that it matches the current country fr
  tmp.sle.b<-subset(sle_both.un,country==countries[i])
  # Excess Deaths lp
  # number of years lost per age -- standard SLE from GBD
  YLL.gbd.b_min<-Age_Projected_Min*sle.gbd$sle 
  # number of years lost per age -- country specific
  YLL.un.b_min<-Age_Projected_Min*tmp.sle.b$sle_un 
  # Excess Deaths up
  # number of years lost per age -- standard SLE from GBD
  YLL.gbd.b_max<-Age_Projected_Max*sle.gbd$sle 
  # number of years lost per age -- country specific
  YLL.un.b_max<-Age_Projected_Max*tmp.sle.b$sle_un 
  
  # subsetting to population
  tmp.population<-subset(pop,as.character(Country)==countries[i])
  # population count
  Population<-tmp.population$Total
  
  # computing rates for each measure
  YLL.rate.gbd.b_max<-(YLL.gbd.b_max/tmp.population$Total)*100000 
  YLL.rate.un.b_max<-(YLL.un.b_max/tmp.population$Total)*100000 
  YLL.rate.gbd.b_min<-(YLL.gbd.b_min/tmp.population$Total)*100000 
  YLL.rate.un.b_min<-(YLL.un.b_min/tmp.population$Total)*100000 
  
  # reordering columns
  yll.data.both[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Age=sort(unique(d$Age)),
                                 Population=Population,
                                 Age_Projected_Min=Age_Projected_Min,Age_Projected_Max=Age_Projected_Max,
                                 YLL.gbd.b_min=YLL.gbd.b_min, YLL.un.b_min=YLL.un.b_min,
                                 YLL.gbd.b_max=YLL.gbd.b_max, YLL.un.b_max=YLL.un.b_max,
                                 YLL.rate.gbd.b_max=YLL.rate.gbd.b_max,YLL.rate.un.b_max=YLL.rate.un.b_max,
                                 YLL.rate.gbd.b_min=YLL.rate.gbd.b_min,YLL.rate.un.b_min=YLL.rate.un.b_min
  )
}
names(yll.data.both)<-countries
yll.data.both.all<-do.call(rbind,yll.data.both)
# ver with countries 
saveRDS(yll.data.both,
        file=here("Data","yll-projected-list.rds") )
#this is the ver with all countries in one full dataframe
saveRDS(yll.data.both.all,
        file=here("Data","yll-projected.rds") )


###################################################################################################
##
##  AGGREGATION : YLL ABSOLUTE NUMBERS
##  
##
###################################################################################################

# Total YLL by country, YLL rates with standardized population
# File format is: Country, Date.death, Date.edeath, 
# names(yll.data.both.all)

# Similarly for rates

# Columns currently included
colnames.yll.measures<-c('Country', 'Date'
                         , "YLL.gbd.b_min","YLL.un.b_min"
                         ,"YLL.gbd.b_max","YLL.un.b_max"
                         )

# Data frame to store results
YLL.measures<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)<-colnames.yll.measures
Date<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  Date[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  tmp.var<-c("YLL.gbd.b_min","YLL.un.b_min"
             ,"YLL.gbd.b_max","YLL.un.b_max")
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
}
YLL.measures$Date<-as.Date(Date)

# saving
saveRDS(YLL.measures,
        file=here("Data","YLL_projected_measures.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 'Date'
                                  ,"YLL.rate.gbd.b_max","YLL.rate.un.b_max"
                                  ,"YLL.rate.gbd.b_min","YLL.rate.un.b_min" )
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
Date<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  Date[i]<-yll.data.both.all$Date[tmp.ind][1]
  tmp.var<-c("YLL.rate.gbd.b_max","YLL.rate.un.b_max"
             ,"YLL.rate.gbd.b_min","YLL.rate.un.b_min" )
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
}
YLL.measures.rate.esp$Date<-Date

# saving
saveRDS(YLL.measures.rate.esp,
        file=here("Data","YLL_projected_measures_rate_esp.rds"))
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################

colnames.yll.measures.rate.gbd<-c('Country', 'Date'
                                  ,"YLL.rate.gbd.b_max","YLL.rate.un.b_max"
                                  ,"YLL.rate.gbd.b_min","YLL.rate.un.b_min" )

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
Date<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  Date[i]<-yll.data.both.all$Date[tmp.ind][1]
  tmp.var<-c("YLL.rate.gbd.b_max","YLL.rate.un.b_max"
             ,"YLL.rate.gbd.b_min","YLL.rate.un.b_min" )
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
}
YLL.measures.rate.gbd$Date<-Date

# saving
saveRDS(YLL.measures.rate.gbd,
        file=here("Data","YLL_projected_measures_rate_gbd.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC
##  
##
###################################################################################################

dat<-readRDS(file=here("Data","YLL_projected_measures.rds"))

sumpop<-tapply(pop$Total,pop$Country,sum)
dat$Pop<-sumpop[which(names(sumpop)%in%dat$Country)]

rates<-dat[,c('YLL.un.b_min', 'YLL.un.b_max')]/dat$Pop *100000

yll_rates<-as.data.frame(cbind(dat$Country,dat$Date,rates,dat$Pop))


# saving
saveRDS(yll_rates,
        file=here("Data","YLL_projected_measures_rate_gbd_cpop.rds"))

# Keeping also the projected deaths
saveRDS(projected$proj.deaths.min,
        file=here("Data","projected_deaths_min_figures.rds"))
