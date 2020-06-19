###################################################################################################
###################################################################################################
##
##  Other Causes YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Calculates years of life lost (YLL) and related measures 

# Notes:
# Mind the user defined paths.


###################################################################################################
##
##  DATA
##  
##
###################################################################################################

# Other Causes deaths: Transport
###################################################################################################
out <- readRDS(here("Data","other_cause_transport.rds"))

#choose YLL or Deaths
out <-subset(out, Measure =="Deaths")
###################################################################################################

# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
# Needs to be included in repo
nytexcess <- read.csv(here("Data", "NYTExcessDeaths.csv"),
                           sep=",")

matched<-c("Austria","Belgium","Denmark","France","Germany","Italy","Netherlands","Norway"
           ,"Portugal","South Africa","Spain","Sweden","Switzerland")
nytexcessv<-nytexcess[which(nytexcess$Country%in%matched),]

# SLE
###################################################################################################
# GBD life expectancy best case standard
sle.gbd<-readRDS(here("Data","country_gbd_sle.2016.rds"))

# Country specific life expectancies
sle_both.un   <- readRDS(here("Data","country_both_sle_un.rds"))
sle_male.un   <- readRDS(here("Data","country_male_sle_un.rds"))
sle_female.un <- readRDS(here("Data","country_female_sle_un.rds"))

# Population
###################################################################################################
pop           <- read.csv(here("Data","pop_complete_2017.csv"),
                          row.names = 1)

# List of countries
###################################################################################################
# Country list file with full sample considered
sample.countries      <- read.csv(here("Data", "full sample list.csv"),
                                  header = TRUE)
# Saving them as a list
sample.countries.list <-as.character(data.frame(sample.countries)[,1])
# List of countries with no data
no.data.countries     <- c('Northern Ireland')
# Final list 
countries <- sample.countries.list[which(is.element(sample.countries.list,no.data.countries)==FALSE)]
# Match countries with data from Other Cause of Death
countries <-intersect(countries, sort(unique(out$Country)))

# Standard population weights
###################################################################################################
#  European standard

pop_std.gbd <- readRDS(here("Data","pop_std.gbd.RDS"))
#  GBD standard

pop_std.esp <- readRDS(here("Data", "pop_std.esp.RDS"))

###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables

# Which countries have gender 
gender.count        <- as.character(unique(out$Country[which(out$Sex=='f')]))
# Coinciding for which we have data already collected, i.e. sample.countries
gender.count.sample <- gender.count[which(is.element(gender.count,countries))]

# Total count in the out file, not necessarily in our sample
total.count    <- as.character(unique(out$Country[which(out$Sex=='b')]))

# Data holders
yll.data.both  <- vector("list",length=length(countries))
# Pick countries with male / female data
countries_mf   <- gender.count.sample
yll.data.male  <- yll.data.female<-vector("list",length=length(countries_mf))


##  Both sexes
###################################################################################################
for(i in 1:length(countries)){
  cat("Country=",countries[i],"\n")
  # choose national level data for both genders
  c            <- subset(out,Country==countries[i]&Sex=="b")  
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b  <- c$val*sle.gbd$sle 
  # number of years lost per age -- country specific
  tmp.sle.b    <- subset(sle_both.un,country==countries[i])
  c$YLL.un.b   <- c$val*tmp.sle.b$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries[i],matched)){
    c$Excess_Deaths <- (c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries[i])]
    #this second part is accessing the country's excess death value in nytexcess, such that it matches the current country from out we're working on atm
  }else{
    c$Excess_Deaths <- c$val #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
  }
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.b.ed <- c$Excess_Deaths*sle.gbd$sle
  # number of years lost per age -- country specific
  c$YLL.un.b.ed  <- c$Excess_Deaths*tmp.sle.b$sle
  # subsetting to population
  tmp.population <- subset(pop,Country==countries[i])
  # population count
  c$Population   <- tmp.population$Total
  # sex
  c$Sex<-c$Sex #? why is this here?
  # computing rates for each measure
  c$YLL.rate.gbd.b     <- (c$YLL.gbd.b/tmp.population$Total)*100000 
  c$YLL.rate.un.b      <- (c$YLL.un.b/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b.ed  <- (c$YLL.gbd.b.ed/tmp.population$Total)*100000 
  c$YLL.rate.un.b.ed   <- (c$YLL.un.b.ed/tmp.population$Total)*100000 
  # reordering columns
  yll.data.both[[i]] <- data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases, #No cases info for Other Causes of Death
                                 Deaths=c$val,
                                 YLL.gbd.b=c$YLL.gbd.b,
                                 YLL.un.b=c$YLL.un.b, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.b.ed=c$YLL.gbd.b.ed, 
                                 YLL.un.b.ed=c$YLL.un.b.ed,
                                 YLL.rate.gbd.b=c$YLL.rate.gbd.b,
                                 YLL.rate.un.b=c$YLL.rate.un.b,
                                 YLL.rate.gbd.b.ed=c$YLL.rate.gbd.b.ed,
                                 YLL.rate.un.b.ed=c$YLL.rate.un.b.ed
                                 
  )
}
names(yll.data.both) <- countries
yll.data.both.all    <- do.call(rbind,yll.data.both)
# ver with countries 
saveRDS(yll.data.both,
        file = here("Data", "transport-yll-b-list.rds"))
#this is the ver with all countries in one full dataframe
saveRDS(yll.data.both.all,
        file = here("Data", "transport-yll-b.rds"))

##  Male only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c           <-subset(out,Country==countries_mf[i]&Sex=="m")  
  c           <-  c[which(c$Date==max(c$Date)),]
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.m <- c$val*sle.gbd$sle
  # using country specific data
  tmp.sle     <- subset(sle_male.un,country==countries_mf[i])
  c$YLL.un.m  <- c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths <- (c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
   }else{
    c$Excess_Deaths <- c$val
  }
  # standard gbd SLE
  c$YLL.gbd.m.ed <- c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.m.ed  <- c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population <- subset(pop,Country==countries[i])
  # population count
  c$Population   <- tmp.population$Male
  # sex
  c$Sex          <- c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.m    <- (c$YLL.gbd.m/tmp.population$Male)*100000 
  c$YLL.rate.un.m     <- (c$YLL.un.m/tmp.population$Male)*100000 
  c$YLL.rate.gbd.m.ed <- (c$YLL.gbd.m.ed/tmp.population$Male)*100000 
  c$YLL.rate.un.m.ed  <- (c$YLL.un.m.ed/tmp.population$Male)*100000 
  # reordering columns
  yll.data.male[[i]] <- data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases,
                                 Deaths=c$val,
                                 YLL.gbd.m=c$YLL.gbd.m,
                                 YLL.un.m=c$YLL.un.m, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.m.ed=c$YLL.gbd.m.ed, 
                                 YLL.un.m.ed=c$YLL.un.m.ed,
                                 YLL.rate.gbd.m=c$YLL.rate.gbd.m,
                                 YLL.rate.un.m=c$YLL.rate.un.m,
                                 YLL.rate.gbd.m.ed=c$YLL.rate.gbd.m.ed,
                                 YLL.rate.un.m.ed=c$YLL.rate.un.m.ed
                                 
  )
}
names(yll.data.male) <- countries_mf
yll.data.male.all    <- do.call(rbind,yll.data.male)
#ver with countries_mf 
saveRDS(yll.data.male,
        file = here("Data","transport-yll-m-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.male,
        file = here("Data","transport-yll-m.rds"))
##  Female only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="f")
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.f<-c$val*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_female.un,country==countries_mf[i])
  c$YLL.un.f<-c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths<-c$val
  }
  # standard gbd SLE
  c$YLL.gbd.f.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.f.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Female
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.f<-(c$YLL.gbd.f/tmp.population$Female)*100000 
  c$YLL.rate.un.f<-(c$YLL.un.f/tmp.population$Female)*100000 
  c$YLL.rate.gbd.f.ed<-(c$YLL.gbd.f.ed/tmp.population$Female)*100000 
  c$YLL.rate.un.f.ed<-(c$YLL.un.f.ed/tmp.population$Female)*100000 
  # reordering columns
  yll.data.female[[i]]<-data.frame(Country=c$Country,
                                   Date=c$Date,
                                   Sex=c$Sex,
                                   Age=c$Age,
                                   Population=c$Population,
                                   #Cases=c$Cases,
                                   Deaths=c$val,
                                   YLL.gbd.f=c$YLL.gbd.f,
                                   YLL.un.f=c$YLL.un.f, 
                                   Excess_Deaths=c$Excess_Deaths,
                                   YLL.gbd.f.ed=c$YLL.gbd.f.ed, 
                                   YLL.un.f.ed=c$YLL.un.f.ed,
                                   YLL.rate.gbd.f=c$YLL.rate.gbd.f,
                                   YLL.rate.un.f=c$YLL.rate.un.f,
                                   YLL.rate.gbd.f.ed=c$YLL.rate.gbd.f.ed,
                                   YLL.rate.un.f.ed=c$YLL.rate.un.f.ed
                                   
  )
}
names(yll.data.female) <- countries_mf
yll.data.female.all    <- do.call(rbind,yll.data.female)
#ver with countries_mf 
saveRDS(yll.data.female,
        file = here("Data","transport-yll-f-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.female,
        file = here("Data","transport-yll-f.rds"))
###################################################################################################
##
##  AGGREGATION : YLL ABSOLUTE NUMBERS
##  
##
###################################################################################################

# Total YLL by country, YLL rates with standardized population
# File format is: Country, Date.death, Date.edeath, 
# YLL.gbd.b, YLL.un.b,YLL.gbd.b.ed, YLL.un.b.ed, 
# YLL.gbd.m, YLL.un.m, YLL.gbd.m.ed, YLL.un.m.ed,
# YLL.gbd.f, YLL.un.f,YLL.gbd.f.ed, YLL.un.f.ed

# Similarly for rates

# Columns currently included
colnames.yll.measures<-c('Country', 'Date.death', 'Date.edeath', 
                         'YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed', 
                         'YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed',
                         'YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed')

# Data frame to store results
YLL.measures            <- as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)  <- colnames.yll.measures
Date.death<-Date.edeath <- as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.gbd.b","YLL.un.b","YLL.gbd.b.ed","YLL.un.b.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.m","YLL.un.m","YLL.gbd.m.ed","YLL.un.m.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.f","YLL.un.f","YLL.gbd.f.ed","YLL.un.f.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var],na.rm=TRUE)
}
YLL.measures$Date.death<-Date.death
YLL.measures$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures,
        file = here("Data","transport_YLL_measures.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
}
YLL.measures.rate.esp$Date.death<-Date.death
YLL.measures.rate.esp$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures.rate.esp,
        file = here("Data","transport_YLL_measures_rate_esp.rds"))
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################


colnames.yll.measures.rate.gbd<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
}
YLL.measures.rate.gbd$Date.death<-Date.death
YLL.measures.rate.gbd$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures.rate.gbd,
        file = here("Data", "transport_YLL_measures_rate_gbd.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC 
##  
##
###################################################################################################

dat     <- readRDS(file=here("Data","transport_YLL_measures.rds"))

sumpop  <- tapply(pop$Total,pop$Country,sum)
dat$Pop <- sumpop[which(names(sumpop)%in%dat$Country)]

rates<-dat[,c("YLL.gbd.b","YLL.un.b")]/dat$Pop *100000
yll_rates<-as.data.frame(cbind(dat$Country,dat$Date.death,rates,dat$Pop))
names(yll_rates)<-c("Country","Date"
                    ,"YLL.rate.gbd.b_cpop","YLL.rate.un.b_cpop"
                    ,"Pop")


# saving
saveRDS(yll_rates,
        file = here("Data","transport_YLL_measures_rate_cpop.rds"))


# Other Causes deaths: Heart (Cardiovascular diseases)
###################################################################################################
out<-readRDS(here("Data","other_cause_heart.rds"))

#choose YLL or Deaths
out<-subset(out,Measure=="Deaths")
###################################################################################################

# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
nytexcess <- read.csv(here("Data","NYTExcessDeaths.csv"), sep = ",")
matched<-c("Austria","Belgium","Denmark","France","Germany","Italy","Netherlands","Norway"
           ,"Portugal","South Africa","Spain","Sweden","Switzerland")
nytexcess<-nytexcess[which(nytexcess$Country%in%matched),]

# SLE
###################################################################################################
# GBD life expectancy best case standard
sle.gbd       <- readRDS(here("Data","country_gbd_sle.2016.rds"))
# Country specific life expectancies
sle_both.un   <- readRDS(here("Data","country_both_sle_un.rds"))
sle_male.un   <- readRDS(here("Data","country_male_sle_un.rds"))
sle_female.un <- readRDS(here("Data","country_female_sle_un.rds"))

# Population
###################################################################################################
pop           <- read.csv(here("Data","pop_complete_2017.csv"),row.names = 1)
# List of countries
###################################################################################################
# Country list file with full sample considered
sample.countries <- read.csv(here("Data","full sample list.csv"),
                           header=TRUE)
# Saving them as a list
sample.countries.list<-as.character(data.frame(sample.countries)[,1])
# List of countries with no data
no.data.countries<- c('Northern Ireland')
# Final list 
countries<-sample.countries.list[which(is.element(sample.countries.list,no.data.countries)==FALSE)]
# Match countries with data from Other Cause of Death
countries<-intersect(countries,sort(unique(out$Country)))

# Standard population weights
###################################################################################################
#  European standard

pop_std.gbd <- readRDS(here("Data","pop_std.gbd.RDS"))
pop_std.esp <- readRDS(here("Data","pop_std.esp.RDS"))

###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables

# Which countries have gender 
gender.count<-as.character(unique(out$Country[which(out$Sex=='f')]))
# Coinciding for which we have data already collected, i.e. sample.countries
gender.count.sample<-gender.count[which(is.element(gender.count,countries))]

# Total count in the out file, not necessarily in our sample
total.count<-as.character(unique(out$Country[which(out$Sex=='b')]))

# Data holders
yll.data.both<-vector("list",length=length(countries))
# Pick countries with male / female data
countries_mf<-gender.count.sample
yll.data.male<-yll.data.female<-vector("list",length=length(countries_mf))

##  Both sexes
###################################################################################################
for(i in 1:length(countries)){
  cat("Country=",countries[i],"\n")
  # choose national level data for both genders
  c<-subset(out,Country==countries[i]&Sex=="b")  
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b<-c$val*sle.gbd$sle 
  # number of years lost per age -- country specific
  tmp.sle.b<-subset(sle_both.un,country==countries[i])
  c$YLL.un.b<-c$val*tmp.sle.b$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries[i])]
    #this second part is accessing the country's excess death value in nytexcess, such that it matches the current country from out we're working on atm
  }else{
    c$Excess_Deaths<-c$val #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
  }
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.b.ed<-c$Excess_Deaths*sle.gbd$sle
  # number of years lost per age -- country specific
  c$YLL.un.b.ed<-c$Excess_Deaths*tmp.sle.b$sle
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Total
  # sex
  c$Sex<-c$Sex #? why is this here?
  # computing rates for each measure
  c$YLL.rate.gbd.b<-(c$YLL.gbd.b/tmp.population$Total)*100000 
  c$YLL.rate.un.b<-(c$YLL.un.b/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b.ed<-(c$YLL.gbd.b.ed/tmp.population$Total)*100000 
  c$YLL.rate.un.b.ed<-(c$YLL.un.b.ed/tmp.population$Total)*100000 
  # reordering columns
  yll.data.both[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases, #No cases info for Other Causes of Death
                                 Deaths=c$val,
                                 YLL.gbd.b=c$YLL.gbd.b,
                                 YLL.un.b=c$YLL.un.b, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.b.ed=c$YLL.gbd.b.ed, 
                                 YLL.un.b.ed=c$YLL.un.b.ed,
                                 YLL.rate.gbd.b=c$YLL.rate.gbd.b,
                                 YLL.rate.un.b=c$YLL.rate.un.b,
                                 YLL.rate.gbd.b.ed=c$YLL.rate.gbd.b.ed,
                                 YLL.rate.un.b.ed=c$YLL.rate.un.b.ed
                                 
  )
}
names(yll.data.both)<-countries
yll.data.both.all<-do.call(rbind,yll.data.both)
# ver with countries 
saveRDS(yll.data.both,
        file=here("Data","heart-yll-b-list.rds"))
#this is the ver with all countries in one full dataframe
saveRDS(yll.data.both.all,
        file=here("Data","heart-yll-b.rds"))

##  Male only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="m")  
  c<-c[which(c$Date==max(c$Date)),]
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.m<-c$val*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_male.un,country==countries_mf[i])
  c$YLL.un.m<-c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths<-c$val
  }
  # standard gbd SLE
  c$YLL.gbd.m.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.m.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Male
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.m<-(c$YLL.gbd.m/tmp.population$Male)*100000 
  c$YLL.rate.un.m<-(c$YLL.un.m/tmp.population$Male)*100000 
  c$YLL.rate.gbd.m.ed<-(c$YLL.gbd.m.ed/tmp.population$Male)*100000 
  c$YLL.rate.un.m.ed<-(c$YLL.un.m.ed/tmp.population$Male)*100000 
  # reordering columns
  yll.data.male[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases,
                                 Deaths=c$val,
                                 YLL.gbd.m=c$YLL.gbd.m,
                                 YLL.un.m=c$YLL.un.m, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.m.ed=c$YLL.gbd.m.ed, 
                                 YLL.un.m.ed=c$YLL.un.m.ed,
                                 YLL.rate.gbd.m=c$YLL.rate.gbd.m,
                                 YLL.rate.un.m=c$YLL.rate.un.m,
                                 YLL.rate.gbd.m.ed=c$YLL.rate.gbd.m.ed,
                                 YLL.rate.un.m.ed=c$YLL.rate.un.m.ed
                                 
  )
}
names(yll.data.male)<-countries_mf
yll.data.male.all<-do.call(rbind,yll.data.male)
#ver with countries_mf 
saveRDS(yll.data.male,
        file = here("Data","heart-yll-m-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.male,
        file = here("Data","heart-yll-m.rds"))
##  Female only###################################################################################################
###################################################################################################
##
##  Other Causes YLL
##  Years of life lost
##
###################################################################################################
###################################################################################################

# Description:
# Calculates years of life lost (YLL) and related measures 

# Notes:
# Mind the user defined paths.

###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="f")
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.f<-c$val*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_female.un,country==countries_mf[i])
  c$YLL.un.f<-c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths<-c$val
  }
  # standard gbd SLE
  c$YLL.gbd.f.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.f.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Female
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.f<-(c$YLL.gbd.f/tmp.population$Female)*100000 
  c$YLL.rate.un.f<-(c$YLL.un.f/tmp.population$Female)*100000 
  c$YLL.rate.gbd.f.ed<-(c$YLL.gbd.f.ed/tmp.population$Female)*100000 
  c$YLL.rate.un.f.ed<-(c$YLL.un.f.ed/tmp.population$Female)*100000 
  # reordering columns
  yll.data.female[[i]]<-data.frame(Country=c$Country,
                                   Date=c$Date,
                                   Sex=c$Sex,
                                   Age=c$Age,
                                   Population=c$Population,
                                   #Cases=c$Cases,
                                   Deaths=c$val,
                                   YLL.gbd.f=c$YLL.gbd.f,
                                   YLL.un.f=c$YLL.un.f, 
                                   Excess_Deaths=c$Excess_Deaths,
                                   YLL.gbd.f.ed=c$YLL.gbd.f.ed, 
                                   YLL.un.f.ed=c$YLL.un.f.ed,
                                   YLL.rate.gbd.f=c$YLL.rate.gbd.f,
                                   YLL.rate.un.f=c$YLL.rate.un.f,
                                   YLL.rate.gbd.f.ed=c$YLL.rate.gbd.f.ed,
                                   YLL.rate.un.f.ed=c$YLL.rate.un.f.ed
                                   
  )
}
names(yll.data.female)<-countries_mf
yll.data.female.all<-do.call(rbind,yll.data.female)
#ver with countries_mf 
saveRDS(yll.data.female,
        file=here("Data","heart-yll-f-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.female,
       file=here("Data","heart-yll-f.rds"))
###################################################################################################
##
##  AGGREGATION : YLL ABSOLUTE NUMBERS
##  
##
###################################################################################################

# Total YLL by country, YLL rates with standardized population
# File format is: Country, Date.death, Date.edeath, 
# YLL.gbd.b, YLL.un.b,YLL.gbd.b.ed, YLL.un.b.ed, 
# YLL.gbd.m, YLL.un.m, YLL.gbd.m.ed, YLL.un.m.ed,
# YLL.gbd.f, YLL.un.f,YLL.gbd.f.ed, YLL.un.f.ed

# Similarly for rates

# Columns currently included
colnames.yll.measures<-c('Country', 'Date.death', 'Date.edeath', 
                         'YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed', 
                         'YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed',
                         'YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed')

# Data frame to store results
YLL.measures<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)<-colnames.yll.measures
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.gbd.b","YLL.un.b","YLL.gbd.b.ed","YLL.un.b.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.m","YLL.un.m","YLL.gbd.m.ed","YLL.un.m.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.f","YLL.un.f","YLL.gbd.f.ed","YLL.un.f.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var],na.rm=TRUE)
}
YLL.measures$Date.death<-Date.death
YLL.measures$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures,
        file=here("Data","heart_YLL_measures.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
}
YLL.measures.rate.esp$Date.death<-Date.death
YLL.measures.rate.esp$Date.edeath<-Date.edeath

# saving

saveRDS(YLL.measures.rate.esp,
        file=here("Data","heart_YLL_measures_rate_esp.rds"))
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################


colnames.yll.measures.rate.gbd<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
}
YLL.measures.rate.gbd$Date.death<-Date.death
YLL.measures.rate.gbd$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures.rate.gbd,
        file=here("Data","heart_YLL_measures_rate_gbd.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC
##  
##
###################################################################################################

dat<-readRDS(file=here("Data","heart_YLL_measures.rds"))

sumpop<-tapply(pop$Total,pop$Country,sum)
dat$Pop<-sumpop[which(names(sumpop)%in%dat$Country)]

rates<-dat[,c("YLL.gbd.b","YLL.un.b")]/dat$Pop *100000
yll_rates<-as.data.frame(cbind(dat$Country,dat$Date.death,rates,dat$Pop))
names(yll_rates)<-c("Country","Date"
                    ,"YLL.rate.gbd.b_cpop","YLL.rate.un.b_cpop"
                    ,"Pop")


# saving
saveRDS(yll_rates,
        file=here("Data","heart_YLL_measures_rate_cpop.rds"))

# Other Causes deaths: Substance (Substance abuse)
###################################################################################################
out <- readRDS(here("Data","other_cause_substance.rds"))

#choose YLL or Deaths
out<-subset(out,Measure=="Deaths")
###################################################################################################

# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
nytexcess<-read.csv(here("Data","NYTExcessDeaths.csv"),sep=",")
matched<-c("Austria","Belgium","Denmark","France","Germany","Italy","Netherlands","Norway"
           ,"Portugal","South Africa","Spain","Sweden","Switzerland")
nytexcess<-nytexcess[which(nytexcess$Country%in%matched),]

# SLE
###################################################################################################
# GBD life expectancy best case standard
sle.gbd       <- readRDS(here("Data","country_gbd_sle.2016.rds"))
# Country specific life expectancies
sle_both.un   <- readRDS(here("Data","country_both_sle_un.rds"))
sle_male.un   <- readRDS(here("Data","country_male_sle_un.rds"))
sle_female.un <- readRDS(here("Data","country_female_sle_un.rds"))

# Population
###################################################################################################
pop<-read.csv(here("Data","pop_complete_2017.csv"),
              row.names = 1)

# List of countries
###################################################################################################
# Country list file with full sample considered
sample.countries <- read.csv(here("Data","full sample list.csv"), header=TRUE)
# Saving them as a list
sample.countries.list <- as.character(data.frame(sample.countries)[,1])
# List of countries with no data
no.data.countries<- c('Northern Ireland')
# Final list 
countries<-sample.countries.list[which(is.element(sample.countries.list,no.data.countries)==FALSE)]
# Match countries with data from Other Cause of Death
countries<-intersect(countries,sort(unique(out$Country)))

# Standard population weights
###################################################################################################
#  European standard

pop_std.gbd<-readRDS(here("Data","pop_std.gbd.RDS"))
#  GBD standard
pop_std.esp<-readRDS(here("Data","pop_std.esp.RDS"))

###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables

# Which countries have gender 
gender.count<-as.character(unique(out$Country[which(out$Sex=='f')]))
# Coinciding for which we have data already collected, i.e. sample.countries
gender.count.sample<-gender.count[which(is.element(gender.count,countries))]

# Total count in the out file, not necessarily in our sample
total.count<-as.character(unique(out$Country[which(out$Sex=='b')]))

# Data holders
yll.data.both<-vector("list",length=length(countries))
# Pick countries with male / female data
countries_mf<-gender.count.sample
yll.data.male<-yll.data.female<-vector("list",length=length(countries_mf))

##  Both sexes
###################################################################################################
for(i in 1:length(countries)){
  cat("Country=",countries[i],"\n")
  # choose national level data for both genders
  c<-subset(out,Country==countries[i]&Sex=="b")  
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b<-c$val*sle.gbd$sle 
  # number of years lost per age -- country specific
  tmp.sle.b<-subset(sle_both.un,country==countries[i])
  c$YLL.un.b<-c$val*tmp.sle.b$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries[i])]
    #this second part is accessing the country's excess death value in nytexcess, such that it matches the current country from out we're working on atm
  }else{
    c$Excess_Deaths<-c$val #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
  }
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.b.ed<-c$Excess_Deaths*sle.gbd$sle
  # number of years lost per age -- country specific
  c$YLL.un.b.ed<-c$Excess_Deaths*tmp.sle.b$sle
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Total
  # sex
  c$Sex<-c$Sex #? why is this here?
  # computing rates for each measure
  c$YLL.rate.gbd.b<-(c$YLL.gbd.b/tmp.population$Total)*100000 
  c$YLL.rate.un.b<-(c$YLL.un.b/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b.ed<-(c$YLL.gbd.b.ed/tmp.population$Total)*100000 
  c$YLL.rate.un.b.ed<-(c$YLL.un.b.ed/tmp.population$Total)*100000 
  # reordering columns
  yll.data.both[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases, #No cases info for Other Causes of Death
                                 Deaths=c$val,
                                 YLL.gbd.b=c$YLL.gbd.b,
                                 YLL.un.b=c$YLL.un.b, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.b.ed=c$YLL.gbd.b.ed, 
                                 YLL.un.b.ed=c$YLL.un.b.ed,
                                 YLL.rate.gbd.b=c$YLL.rate.gbd.b,
                                 YLL.rate.un.b=c$YLL.rate.un.b,
                                 YLL.rate.gbd.b.ed=c$YLL.rate.gbd.b.ed,
                                 YLL.rate.un.b.ed=c$YLL.rate.un.b.ed
                                 
  )
}
names(yll.data.both)<-countries
yll.data.both.all<-do.call(rbind,yll.data.both)
# ver with countries 
saveRDS(yll.data.both,
        file=here("Data","substance-yll-b-list.rds"))
#this is the ver with all countries in one full dataframe
saveRDS(yll.data.both.all,
        file=here("Data","substance-yll-b.rds"))

##  Male only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="m")  
  c<-c[which(c$Date==max(c$Date)),]
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.m<-c$val*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_male.un,country==countries_mf[i])
  c$YLL.un.m<-c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths<-c$val
  }
  # standard gbd SLE
  c$YLL.gbd.m.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.m.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Male
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.m<-(c$YLL.gbd.m/tmp.population$Male)*100000 
  c$YLL.rate.un.m<-(c$YLL.un.m/tmp.population$Male)*100000 
  c$YLL.rate.gbd.m.ed<-(c$YLL.gbd.m.ed/tmp.population$Male)*100000 
  c$YLL.rate.un.m.ed<-(c$YLL.un.m.ed/tmp.population$Male)*100000 
  # reordering columns
  yll.data.male[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases,
                                 Deaths=c$val,
                                 YLL.gbd.m=c$YLL.gbd.m,
                                 YLL.un.m=c$YLL.un.m, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.m.ed=c$YLL.gbd.m.ed, 
                                 YLL.un.m.ed=c$YLL.un.m.ed,
                                 YLL.rate.gbd.m=c$YLL.rate.gbd.m,
                                 YLL.rate.un.m=c$YLL.rate.un.m,
                                 YLL.rate.gbd.m.ed=c$YLL.rate.gbd.m.ed,
                                 YLL.rate.un.m.ed=c$YLL.rate.un.m.ed
                                 
  )
}
names(yll.data.male)<-countries_mf
yll.data.male.all<-do.call(rbind,yll.data.male)
#ver with countries_mf 
saveRDS(yll.data.male,
        file=here("Data","substance-yll-m-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.male,
        file=here("Data","substance-yll-m.rds"))
##  Female only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="f")
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.f<-c$val*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_female.un,country==countries_mf[i])
  c$YLL.un.f<-c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths<-c$val
  }
  # standard gbd SLE
  c$YLL.gbd.f.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.f.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Female
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.f<-(c$YLL.gbd.f/tmp.population$Female)*100000 
  c$YLL.rate.un.f<-(c$YLL.un.f/tmp.population$Female)*100000 
  c$YLL.rate.gbd.f.ed<-(c$YLL.gbd.f.ed/tmp.population$Female)*100000 
  c$YLL.rate.un.f.ed<-(c$YLL.un.f.ed/tmp.population$Female)*100000 
  # reordering columns
  yll.data.female[[i]]<-data.frame(Country=c$Country,
                                   Date=c$Date,
                                   Sex=c$Sex,
                                   Age=c$Age,
                                   Population=c$Population,
                                   #Cases=c$Cases,
                                   Deaths=c$val,
                                   YLL.gbd.f=c$YLL.gbd.f,
                                   YLL.un.f=c$YLL.un.f, 
                                   Excess_Deaths=c$Excess_Deaths,
                                   YLL.gbd.f.ed=c$YLL.gbd.f.ed, 
                                   YLL.un.f.ed=c$YLL.un.f.ed,
                                   YLL.rate.gbd.f=c$YLL.rate.gbd.f,
                                   YLL.rate.un.f=c$YLL.rate.un.f,
                                   YLL.rate.gbd.f.ed=c$YLL.rate.gbd.f.ed,
                                   YLL.rate.un.f.ed=c$YLL.rate.un.f.ed
                                   
  )
}
names(yll.data.female)<-countries_mf
yll.data.female.all<-do.call(rbind,yll.data.female)
#ver with countries_mf 
saveRDS(yll.data.female,
        file=here("Data","substance-yll-f-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.female,
        file=here("Data","substance-yll-f.rds"))
###################################################################################################
##
##  AGGREGATION : YLL ABSOLUTE NUMBERS
##  
##
###################################################################################################

# Total YLL by country, YLL rates with standardized population
# File format is: Country, Date.death, Date.edeath, 
# YLL.gbd.b, YLL.un.b,YLL.gbd.b.ed, YLL.un.b.ed, 
# YLL.gbd.m, YLL.un.m, YLL.gbd.m.ed, YLL.un.m.ed,
# YLL.gbd.f, YLL.un.f,YLL.gbd.f.ed, YLL.un.f.ed

# Similarly for rates

# Columns currently included
colnames.yll.measures<-c('Country', 'Date.death', 'Date.edeath', 
                         'YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed', 
                         'YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed',
                         'YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed')

# Data frame to store results
YLL.measures<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)<-colnames.yll.measures
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.gbd.b","YLL.un.b","YLL.gbd.b.ed","YLL.un.b.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.m","YLL.un.m","YLL.gbd.m.ed","YLL.un.m.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.f","YLL.un.f","YLL.gbd.f.ed","YLL.un.f.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var],na.rm=TRUE)
}
YLL.measures$Date.death<-Date.death
YLL.measures$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures,
        file=here("Data","substance_YLL_measures.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
}
YLL.measures.rate.esp$Date.death<-Date.death
YLL.measures.rate.esp$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures.rate.esp,
        file=here("Data","substance_YLL_measures_rate_esp.rds"))
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################


colnames.yll.measures.rate.gbd<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
}
YLL.measures.rate.gbd$Date.death<-Date.death
YLL.measures.rate.gbd$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures.rate.gbd,
        file=here("Data","substance_YLL_measures_rate_gbd.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC
##  
##
###################################################################################################

dat<-readRDS(file=here("Data","substance_YLL_measures.rds"))

sumpop<-tapply(pop$Total,pop$Country,sum)
dat$Pop<-sumpop[which(names(sumpop)%in%dat$Country)]

rates<-dat[,c("YLL.gbd.b","YLL.un.b")]/dat$Pop *100000
yll_rates<-as.data.frame(cbind(dat$Country,dat$Date.death,rates,dat$Pop))
names(yll_rates)<-c("Country","Date"
                    ,"YLL.rate.gbd.b_cpop","YLL.rate.un.b_cpop"
                    ,"Pop")


# saving
saveRDS(yll_rates,
        file=here("Data","substance_YLL_measures_rate_cpop.rds"))

# Other Causes deaths: Drug (drug use disorders)
###################################################################################################
out<-readRDS(here("Data","other_cause_drug.rds"))

#choose YLL or Deaths
out<-subset(out,Measure=="Deaths")
###################################################################################################

# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
nytexcess<-read.csv(here("Data","NYTExcessDeaths.csv"),sep=",")
matched<-c("Austria","Belgium","Denmark","France","Germany","Italy","Netherlands","Norway"
           ,"Portugal","South Africa","Spain","Sweden","Switzerland")
nytexcess<-nytexcess[which(nytexcess$Country%in%matched),]

# SLE
###################################################################################################
# GBD life expectancy best case standard
sle.gbd       <- readRDS(here("Data","country_gbd_sle.2016.rds"))
# Country specific life expectancies
sle_both.un   <- readRDS(here("Data","country_both_sle_un.rds"))
sle_male.un   <- readRDS(here("Data","country_male_sle_un.rds"))
sle_female.un <- readRDS(here("Data","country_female_sle_un.rds"))

# Population
###################################################################################################
pop<-read.csv(here("Data","pop_complete_2017.csv"),
              row.names = 1)

# List of countries
###################################################################################################
# Country list file with full sample considered
sample.countries <- read.csv(here("Data","full sample list.csv"),header=TRUE)
# Saving them as a list
sample.countries.list<-as.character(data.frame(sample.countries)[,1])
# List of countries with no data
no.data.countries<- c('Northern Ireland')
# Final list 
countries<-sample.countries.list[which(is.element(sample.countries.list,no.data.countries)==FALSE)]
# Match countries with data from Other Cause of Death
countries<-intersect(countries,sort(unique(out$Country)))

# Standard population weights
###################################################################################################
#  European standard

pop_std.gbd<-readRDS(here("Data","pop_std.gbd.RDS"))
#  GBD standard
pop_std.esp<-readRDS(here("Data","pop_std.esp.RDS"))

###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables

# Which countries have gender 
gender.count<-as.character(unique(out$Country[which(out$Sex=='f')]))
# Coinciding for which we have data already collected, i.e. sample.countries
gender.count.sample<-gender.count[which(is.element(gender.count,countries))]

# Total count in the out file, not necessarily in our sample
total.count<-as.character(unique(out$Country[which(out$Sex=='b')]))

# Data holders
yll.data.both<-vector("list",length=length(countries))
# Pick countries with male / female data
countries_mf<-gender.count.sample
yll.data.male<-yll.data.female<-vector("list",length=length(countries_mf))

##  Both sexes
###################################################################################################
for(i in 1:length(countries)){
  cat("Country=",countries[i],"\n")
  # choose national level data for both genders
  c<-subset(out,Country==countries[i]&Sex=="b")  
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b<-c$val*sle.gbd$sle 
  # number of years lost per age -- country specific
  tmp.sle.b<-subset(sle_both.un,country==countries[i])
  c$YLL.un.b<-c$val*tmp.sle.b$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries[i])]
    #this second part is accessing the country's excess death value in nytexcess, such that it matches the current country from out we're working on atm
  }else{
    c$Excess_Deaths<-c$val #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
  }
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.b.ed<-c$Excess_Deaths*sle.gbd$sle
  # number of years lost per age -- country specific
  c$YLL.un.b.ed<-c$Excess_Deaths*tmp.sle.b$sle
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Total
  # sex
  c$Sex<-c$Sex #? why is this here?
  # computing rates for each measure
  c$YLL.rate.gbd.b<-(c$YLL.gbd.b/tmp.population$Total)*100000 
  c$YLL.rate.un.b<-(c$YLL.un.b/tmp.population$Total)*100000 
  c$YLL.rate.gbd.b.ed<-(c$YLL.gbd.b.ed/tmp.population$Total)*100000 
  c$YLL.rate.un.b.ed<-(c$YLL.un.b.ed/tmp.population$Total)*100000 
  # reordering columns
  yll.data.both[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases, #No cases info for Other Causes of Death
                                 Deaths=c$val,
                                 YLL.gbd.b=c$YLL.gbd.b,
                                 YLL.un.b=c$YLL.un.b, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.b.ed=c$YLL.gbd.b.ed, 
                                 YLL.un.b.ed=c$YLL.un.b.ed,
                                 YLL.rate.gbd.b=c$YLL.rate.gbd.b,
                                 YLL.rate.un.b=c$YLL.rate.un.b,
                                 YLL.rate.gbd.b.ed=c$YLL.rate.gbd.b.ed,
                                 YLL.rate.un.b.ed=c$YLL.rate.un.b.ed
                                 
  )
}
names(yll.data.both)<-countries
yll.data.both.all<-do.call(rbind,yll.data.both)
# ver with countries
saveRDS(yll.data.both,
        file=here("Data","drug-yll-b-list.rds"))
#this is the ver with all countries in one full dataframe
saveRDS(yll.data.both.all,
        file=here("Data","drug-yll-b.rds"))

##  Male only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="m")  
  c<-c[which(c$Date==max(c$Date)),]
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.m<-c$val*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_male.un,country==countries_mf[i])
  c$YLL.un.m<-c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths<-c$val
  }
  # standard gbd SLE
  c$YLL.gbd.m.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.m.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Male
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.m<-(c$YLL.gbd.m/tmp.population$Male)*100000 
  c$YLL.rate.un.m<-(c$YLL.un.m/tmp.population$Male)*100000 
  c$YLL.rate.gbd.m.ed<-(c$YLL.gbd.m.ed/tmp.population$Male)*100000 
  c$YLL.rate.un.m.ed<-(c$YLL.un.m.ed/tmp.population$Male)*100000 
  # reordering columns
  yll.data.male[[i]]<-data.frame(Country=c$Country,
                                 Date=c$Date,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population=c$Population,
                                 #Cases=c$Cases,
                                 Deaths=c$val,
                                 YLL.gbd.m=c$YLL.gbd.m,
                                 YLL.un.m=c$YLL.un.m, 
                                 Excess_Deaths=c$Excess_Deaths,
                                 YLL.gbd.m.ed=c$YLL.gbd.m.ed, 
                                 YLL.un.m.ed=c$YLL.un.m.ed,
                                 YLL.rate.gbd.m=c$YLL.rate.gbd.m,
                                 YLL.rate.un.m=c$YLL.rate.un.m,
                                 YLL.rate.gbd.m.ed=c$YLL.rate.gbd.m.ed,
                                 YLL.rate.un.m.ed=c$YLL.rate.un.m.ed
                                 
  )
}
names(yll.data.male)<-countries_mf
yll.data.male.all<-do.call(rbind,yll.data.male)
#ver with countries_mf 
saveRDS(yll.data.male,
        file=here("Data","drug-yll-m-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.male,
        file=here("Data","drug-yll-m.rds"))
##  Female only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="f")
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.f<-c$val*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_female.un,country==countries_mf[i])
  c$YLL.un.f<-c$val*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths<-(c$val/sum(c$val))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths<-c$val
  }
  # standard gbd SLE
  c$YLL.gbd.f.ed<-c$Excess_Deaths*sle.gbd$sle
  # country specific SLE
  c$YLL.un.f.ed<-c$Excess_Deaths*tmp.sle$sle_un
  # subsetting to population
  tmp.population<-subset(pop,Country==countries[i])
  # population count
  c$Population<-tmp.population$Female
  # sex
  c$Sex<-c$Sex
  # computing rates for each measure
  c$YLL.rate.gbd.f<-(c$YLL.gbd.f/tmp.population$Female)*100000 
  c$YLL.rate.un.f<-(c$YLL.un.f/tmp.population$Female)*100000 
  c$YLL.rate.gbd.f.ed<-(c$YLL.gbd.f.ed/tmp.population$Female)*100000 
  c$YLL.rate.un.f.ed<-(c$YLL.un.f.ed/tmp.population$Female)*100000 
  # reordering columns
  yll.data.female[[i]]<-data.frame(Country=c$Country,
                                   Date=c$Date,
                                   Sex=c$Sex,
                                   Age=c$Age,
                                   Population=c$Population,
                                   #Cases=c$Cases,
                                   Deaths=c$val,
                                   YLL.gbd.f=c$YLL.gbd.f,
                                   YLL.un.f=c$YLL.un.f, 
                                   Excess_Deaths=c$Excess_Deaths,
                                   YLL.gbd.f.ed=c$YLL.gbd.f.ed, 
                                   YLL.un.f.ed=c$YLL.un.f.ed,
                                   YLL.rate.gbd.f=c$YLL.rate.gbd.f,
                                   YLL.rate.un.f=c$YLL.rate.un.f,
                                   YLL.rate.gbd.f.ed=c$YLL.rate.gbd.f.ed,
                                   YLL.rate.un.f.ed=c$YLL.rate.un.f.ed
                                   
  )
}
names(yll.data.female)<-countries_mf
yll.data.female.all<-do.call(rbind,yll.data.female)
#ver with countries_mf 
saveRDS(yll.data.female,
        file=here("Data","drug-yll-f-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.female,
        file=here("Data","drug-yll-f.rds"))
###################################################################################################
##
##  AGGREGATION : YLL ABSOLUTE NUMBERS
##  
##
###################################################################################################

# Total YLL by country, YLL rates with standardized population
# File format is: Country, Date.death, Date.edeath, 
# YLL.gbd.b, YLL.un.b,YLL.gbd.b.ed, YLL.un.b.ed, 
# YLL.gbd.m, YLL.un.m, YLL.gbd.m.ed, YLL.un.m.ed,
# YLL.gbd.f, YLL.un.f,YLL.gbd.f.ed, YLL.un.f.ed

# Similarly for rates

# Columns currently included
colnames.yll.measures<-c('Country', 'Date.death', 'Date.edeath', 
                         'YLL.gbd.b', 'YLL.un.b','YLL.gbd.b.ed', 'YLL.un.b.ed', 
                         'YLL.gbd.m', 'YLL.un.m', 'YLL.gbd.m.ed', 'YLL.un.m.ed',
                         'YLL.gbd.f', 'YLL.un.f','YLL.gbd.f.ed', 'YLL.un.f.ed')

# Data frame to store results
YLL.measures<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)<-colnames.yll.measures
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.gbd.b","YLL.un.b","YLL.gbd.b.ed","YLL.un.b.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.m","YLL.un.m","YLL.gbd.m.ed","YLL.un.m.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c("YLL.gbd.f","YLL.un.f","YLL.gbd.f.ed","YLL.un.f.ed")
  YLL.measures[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var],na.rm=TRUE)
}
YLL.measures$Date.death<-Date.death
YLL.measures$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures,
        file=here("Data","drug_YLL_measures.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
}
YLL.measures.rate.esp$Date.death<-Date.death
YLL.measures.rate.esp$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures.rate.esp,
        file=here("Data","drug_YLL_measures_rate_esp.rds"))
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################


colnames.yll.measures.rate.gbd<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b', 'YLL.rate.un.b','YLL.rate.gbd.b.ed', 'YLL.rate.un.b.ed', 
                                  'YLL.rate.gbd.m', 'YLL.rate.un.m', 'YLL.rate.gbd.m.ed', 'YLL.rate.un.m.ed',
                                  'YLL.rate.gbd.f', 'YLL.rate.un.f','YLL.rate.gbd.f.ed', 'YLL.rate.un.f.ed')

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
Date.death<-Date.edeath<-as.Date(rep(NA,length(countries)))#dates
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
  Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
  tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
}
YLL.measures.rate.gbd$Date.death<-Date.death
YLL.measures.rate.gbd$Date.edeath<-Date.edeath

# saving
saveRDS(YLL.measures.rate.gbd,
        file=here("Data","drug_YLL_measures_rate_gbd.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC
##  
##
###################################################################################################

dat<-readRDS(file=here("Data","drug_YLL_measures.rds"))

sumpop<-tapply(pop$Total,pop$Country,sum)
dat$Pop<-sumpop[which(names(sumpop)%in%dat$Country)]

rates<-dat[,c("YLL.gbd.b","YLL.un.b")]/dat$Pop *100000
yll_rates<-as.data.frame(cbind(dat$Country,dat$Date.death,rates,dat$Pop))
names(yll_rates)<-c("Country","Date"
                    ,"YLL.rate.gbd.b_cpop","YLL.rate.un.b_cpop"
                    ,"Pop")


# saving
saveRDS(yll_rates,
        file=here("Data","drug_YLL_measures_rate_cpop.rds"))


# Other Causes deaths: Flu 
# (note below diff from above causes bc incorporates med/max values across years)
###################################################################################################
out<-readRDS(here("Data","other_cause_flu.rds"))

#choose YLL or Deaths
out<-subset(out,Measure=="Deaths")

###################################################################################################

# Needs to be able to append well to the file out; just another column of deaths (or 3 if confidence intervals)
nytexcess<-read.csv(here("Data","NYTExcessDeaths.csv"),sep=",")
matched<-c("Austria","Belgium","Denmark","France","Germany","Italy","Netherlands","Norway"
           ,"Portugal","South Africa","Spain","Sweden","Switzerland")
nytexcess<-nytexcess[which(nytexcess$Country%in%matched),]

# SLE
###################################################################################################
# GBD life expectancy best case standard
sle.gbd       <- readRDS(here("Data","country_gbd_sle.2016.rds"))
# Country specific life expectancies
sle_both.un   <- readRDS(here("Data","country_both_sle_un.rds"))
sle_male.un   <- readRDS(here("Data","country_male_sle_un.rds"))
sle_female.un <- readRDS(here("Data","country_female_sle_un.rds"))
# Population
###################################################################################################
pop_med<-read.csv(here("Data","pop_complete_flu_med.csv"),row.names = 1)
pop_max<-read.csv(here("Data","pop_complete_flu_max.csv"),row.names = 1)
#pop_complete_flu_med.csv
#pop_complete_flu_max

# List of countries
###################################################################################################
# Country list file with full sample considered
sample.countries<-read.csv(here("Data","full sample list.csv"),header=TRUE)
# Saving them as a list
sample.countries.list<-as.character(data.frame(sample.countries)[,1])
# List of countries with no data
no.data.countries<- c('Northern Ireland')
# Final list 
countries<-sample.countries.list[which(is.element(sample.countries.list,no.data.countries)==FALSE)]
# Match countries with data from Other Cause of Death
countries<-intersect(countries,sort(unique(out$Country)))

# Standard population weights
###################################################################################################
#  European standard
pop_std.gbd<-readRDS(here("Data","pop_std.gbd.RDS"))
#  GBD standard
pop_std.esp<-readRDS(here("Data","pop_std.esp.RDS"))

###################################################################################################
##
##  YLL and YLL rates
##  
##
###################################################################################################

# Computes YLL with the GBD standard life and with country specific life tables

# Which countries have gender 
gender.count<-as.character(unique(out$Country[which(out$Sex=='f')]))
# Coinciding for which we have data already collected, i.e. sa
print(countries[i])
tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)

print(countries[i])
tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
Date.death[i]<-yll.data.both.all$Date[tmp.ind][1]# Date at which death counts are collected
Date.edeath[i]<-yll.data.both.all$Date[tmp.ind][1]# Need to change this bit when we have extended deaths
tmp.var<-c("YLL.rate.gbd.b","YLL.rate.un.b","YLL.rate.gbd.b.ed","YLL.rate.un.b.ed")
YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
tmp.var<-c("YLL.rate.gbd.m","YLL.rate.un.m","YLL.rate.gbd.m.ed","YLL.rate.un.m.ed")
YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
tmp.var<-c("YLL.rate.gbd.f","YLL.rate.un.f","YLL.rate.gbd.f.ed","YLL.rate.un.f.ed")
YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
mple.countries
gender.count.sample<-gender.count[which(is.element(gender.count,countries))]

# Total count in the out file, not necessarily in our sample
total.count<-as.character(unique(out$Country[which(out$Sex=='b')]))

# Data holders
yll.data.both<-vector("list",length=length(countries))
# Pick countries with male / female data
countries_mf<-gender.count.sample
yll.data.male<-yll.data.female<-vector("list",length=length(countries_mf))

##  Both sexes
###################################################################################################
for(i in 1:length(countries)){
  cat("Country=",countries[i],"\n")
  # choose national level data for both genders
  c<-subset(out,Country==countries[i]&Sex=="b")  
  c<-subset(c,Date==c$Date[1])
  # number of years lost per age -- standard SLE from GBD
  c$YLL.gbd.b_med<-c$val_med*sle.gbd$sle 
  c$YLL.gbd.b_max<-c$val_max*sle.gbd$sle 
  # number of years lost per age -- country specific
  tmp.sle.b<-subset(sle_both.un,country==countries[i])
  c$YLL.un.b_med<-c$val_med*tmp.sle.b$sle_un 
  c$YLL.un.b_max<-c$val_max*tmp.sle.b$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries[i],matched)){
    c$Excess_Deaths_med<-(c$val_med/sum(c$val_med))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries[i])]
    c$Excess_Deaths_max<-(c$val_max/sum(c$val_max))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries[i])]
  }else{
    c$Excess_Deaths_med<-c$val_med #if country isn't matched then use original Deaths data (so doesn't include uncounted covid d)
    c$Excess_Deaths_max<-c$val_max
  }
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.b.ed_med<-c$Excess_Deaths_med*sle.gbd$sle
  c$YLL.gbd.b.ed_max<-c$Excess_Deaths_max*sle.gbd$sle
  # number of years lost per age -- country specific
  c$YLL.un.b.ed_med<-c$Excess_Deaths_med*tmp.sle.b$sle
  c$YLL.un.b.ed_max<-c$Excess_Deaths_max*tmp.sle.b$sle
  # subsetting to population
  tmp.population_med<-subset(pop_med,Country==countries[i])
  tmp.population_max<-subset(pop_max,Country==countries[i])
  # population count
  c$Population_med<-tmp.population_med$Total
  c$Population_max<-tmp.population_max$Total
  # computing rates for each measure
  c$YLL.rate.gbd.b_med<-(c$YLL.gbd.b_med/tmp.population_med$Total)*100000 
  c$YLL.rate.gbd.b_max<-(c$YLL.gbd.b_max/tmp.population_max$Total)*100000 
  
  c$YLL.rate.un.b_med<-(c$YLL.un.b_med/tmp.population_med$Total)*100000 
  c$YLL.rate.un.b_max<-(c$YLL.un.b_max/tmp.population_max$Total)*100000 
  
  c$YLL.rate.gbd.b.ed_med<-(c$YLL.gbd.b.ed_med/tmp.population_med$Total)*100000 
  c$YLL.rate.gbd.b.ed_max<-(c$YLL.gbd.b.ed_max/tmp.population_max$Total)*100000 
  
  c$YLL.rate.un.b.ed_med<-(c$YLL.un.b.ed_med/tmp.population_med$Total)*100000 
  c$YLL.rate.un.b.ed_max<-(c$YLL.un.b.ed_max/tmp.population_max$Total)*100000 
  # reordering columns
  yll.data.both[[i]]<-data.frame(Country=c$Country,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population_med=c$Population_med,Population_max=c$Population_max,
                                 Deaths_med=c$val_med,Deaths_max=c$val_max,
                                 YLL.gbd.b_med=c$YLL.gbd.b_med,YLL.gbd.b_max=c$YLL.gbd.b_max,
                                 YLL.un.b_med=c$YLL.un.b_med,YLL.un.b_max=c$YLL.un.b_max, 
                                 Excess_Deaths_med=c$Excess_Deaths_med,Excess_Deaths_max=c$Excess_Deaths_max,
                                 YLL.gbd.b.ed_med=c$YLL.gbd.b.ed_med,YLL.gbd.b.ed_max=c$YLL.gbd.b.ed_max, 
                                 YLL.un.b.ed_med=c$YLL.un.b.ed_med,YLL.un.b.ed_max=c$YLL.un.b.ed_max,
                                 YLL.rate.gbd.b_med=c$YLL.rate.gbd.b_med,YLL.rate.gbd.b_max=c$YLL.rate.gbd.b_max,
                                 YLL.rate.un.b_med=c$YLL.rate.un.b_med,YLL.rate.un.b_max=c$YLL.rate.un.b_max,
                                 YLL.rate.gbd.b.ed_med=c$YLL.rate.gbd.b.ed_med,YLL.rate.gbd.b.ed_max=c$YLL.rate.gbd.b.ed_max,
                                 YLL.rate.un.b.ed_med=c$YLL.rate.un.b.ed_med,YLL.rate.un.b.ed_max=c$YLL.rate.un.b.ed_max
                                 
  )
}
names(yll.data.both)<-countries
yll.data.both.all<-do.call(rbind,yll.data.both)
# ver with countries 
saveRDS(yll.data.both,file=here("Data","flu-yll-b-list.rds"))
#this is the ver with all countries in one full dataframe
saveRDS(yll.data.both.all,file=here("Data","flu-yll-b.rds"))

##  Male only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="m")  
  c<-subset(c,Date==c$Date[1])
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.m_med<-c$val_med*sle.gbd$sle
  c$YLL.gbd.m_max<-c$val_max*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_male.un,country==countries_mf[i])
  c$YLL.un.m_med<-c$val_med*tmp.sle$sle_un 
  c$YLL.un.m_max<-c$val_max*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths_med<-(c$val_med/sum(c$val_med))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
    c$Excess_Deaths_max<-(c$val_max/sum(c$val_max))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths_med<-c$val_med
    c$Excess_Deaths_max<-c$val_max
  }
  # standard gbd SLE
  c$YLL.gbd.m.ed_med<-c$Excess_Deaths_med*sle.gbd$sle
  c$YLL.gbd.m.ed_max<-c$Excess_Deaths_max*sle.gbd$sle
  # country specific SLE
  c$YLL.un.m.ed_med<-c$Excess_Deaths_med*tmp.sle$sle_un
  c$YLL.un.m.ed_max<-c$Excess_Deaths_max*tmp.sle$sle_un
  # subsetting to population
  tmp.population_med<-subset(pop_med,Country==countries[i])
  tmp.population_max<-subset(pop_max,Country==countries[i])
  # population count
  c$Population_med<-tmp.population_med$Male
  c$Population_max<-tmp.population_max$Male
  # computing rates for each measure
  c$YLL.rate.gbd.m_med<-(c$YLL.gbd.m_med/tmp.population_med$Male)*100000 
  c$YLL.rate.gbd.m_max<-(c$YLL.gbd.m_max/tmp.population_max$Male)*100000 
  c$YLL.rate.un.m_med<-(c$YLL.un.m_med/tmp.population_med$Male)*100000 
  c$YLL.rate.un.m_max<-(c$YLL.un.m_max/tmp.population_max$Male)*100000 
  c$YLL.rate.gbd.m.ed_med<-(c$YLL.gbd.m.ed_med/tmp.population_med$Male)*100000 
  c$YLL.rate.gbd.m.ed_max<-(c$YLL.gbd.m.ed_max/tmp.population_max$Male)*100000 
  c$YLL.rate.un.m.ed_med<-(c$YLL.un.m.ed_med/tmp.population_med$Male)*100000 
  c$YLL.rate.un.m.ed_max<-(c$YLL.un.m.ed_max/tmp.population_max$Male)*100000 
  # reordering columns
  yll.data.male[[i]]<-data.frame(Country=c$Country,
                                 Sex=c$Sex,
                                 Age=c$Age,
                                 Population_med=c$Population_med,Population_max=c$Population_max,
                                 Deaths_med=c$val_med,Deaths_max=c$val_max,
                                 YLL.gbd.m_med=c$YLL.gbd.m_med,YLL.gbd.m_max=c$YLL.gbd.m_max,
                                 YLL.un.m_med=c$YLL.un.m_med,YLL.un.m_max=c$YLL.un.m_max, 
                                 Excess_Deaths_med=c$Excess_Deaths_med,Excess_Deaths_max=c$Excess_Deaths_max,
                                 YLL.gbd.m.ed_med=c$YLL.gbd.m.ed_med, YLL.gbd.m.ed_max=c$YLL.gbd.m.ed_max,
                                 YLL.un.m.ed_med=c$YLL.un.m.ed_med,YLL.un.m.ed_max=c$YLL.un.m.ed_max,
                                 YLL.rate.gbd.m_med=c$YLL.rate.gbd.m_med,YLL.rate.gbd.m_max=c$YLL.rate.gbd.m_max,
                                 YLL.rate.un.m_med=c$YLL.rate.un.m_med,YLL.rate.un.m_max=c$YLL.rate.un.m_max,
                                 YLL.rate.gbd.m.ed_med=c$YLL.rate.gbd.m.ed_med,YLL.rate.gbd.m.ed_max=c$YLL.rate.gbd.m.ed_max,
                                 YLL.rate.un.m.ed_med=c$YLL.rate.un.m.ed_med,YLL.rate.un.m.ed_max=c$YLL.rate.un.m.ed_max
  )
}
names(yll.data.male)<-countries_mf
yll.data.male.all<-do.call(rbind,yll.data.male)
#ver with countries_mf 
saveRDS(yll.data.male,
        file=here("Data","flu-yll-m-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.male,
        file=here("Data","flu-yll-m.rds"))
##  Female only
###################################################################################################
for(i in 1:length(countries_mf)){
  cat("Country=",countries_mf[i],"\n")
  c<-subset(out,Country==countries_mf[i]&Sex=="f")
  c<-subset(c,Date==c$Date[1])
  # number of years lost with excess deaths -- standard SLE from GBD
  c$YLL.gbd.f_med<-c$val_med*sle.gbd$sle
  c$YLL.gbd.f_max<-c$val_max*sle.gbd$sle
  # using country specific data
  tmp.sle<-subset(sle_female.un,country==countries_mf[i])
  c$YLL.un.f_med<-c$val_med*tmp.sle$sle_un 
  c$YLL.un.f_max<-c$val_max*tmp.sle$sle_un 
  #create Excess_Deaths variable if in the list of names that match
  if(is.element(countries_mf[i],matched)){
    c$Excess_Deaths_med<-(c$val_med/sum(c$val_med))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
    c$Excess_Deaths_max<-(c$val_max/sum(c$val_max))*nytexcess$ExcessDeaths[which(as.character(nytexcess$Country)==countries_mf[i])]
  }else{
    c$Excess_Deaths_med<-c$val_med
    c$Excess_Deaths_max<-c$val_max
  }
  # standard gbd SLE
  c$YLL.gbd.f.ed_med<-c$Excess_Deaths_med*sle.gbd$sle
  c$YLL.gbd.f.ed_max<-c$Excess_Deaths_max*sle.gbd$sle
  # country specific SLE
  c$YLL.un.f.ed_med<-c$Excess_Deaths_med*tmp.sle$sle_un
  c$YLL.un.f.ed_max<-c$Excess_Deaths_max*tmp.sle$sle_un
  # subsetting to population
  tmp.population_med<-subset(pop_med,Country==countries[i])
  tmp.population_max<-subset(pop_max,Country==countries[i])
  # population count
  c$Population_med<-tmp./population_med$Female
  c$Population_max<-tmp.population_max$Female
  # computing rates for each measure
  c$YLL.rate.gbd.f_med<-(c$YLL.gbd.f_med/tmp.population_med$Female)*100000 
  c$YLL.rate.gbd.f_max<-(c$YLL.gbd.f_max/tmp.population_max$Female)*100000 
  c$YLL.rate.un.f_med<-(c$YLL.un.f_med/tmp.population_med$Female)*100000 
  c$YLL.rate.un.f_max<-(c$YLL.un.f_max/tmp.population_max$Female)*100000 
  c$YLL.rate.gbd.f.ed_med<-(c$YLL.gbd.f.ed_med/tmp.population_med$Female)*100000 
  c$YLL.rate.gbd.f.ed_max<-(c$YLL.gbd.f.ed_max/tmp.population_max$Female)*100000 
  c$YLL.rate.un.f.ed_med<-(c$YLL.un.f.ed_med/tmp.population_med$Female)*100000 
  c$YLL.rate.un.f.ed_max<-(c$YLL.un.f.ed_max/tmp.population_max$Female)*100000 
  # reordering columns
  yll.data.female[[i]]<-data.frame(Country=c$Country,
                                   Sex=c$Sex,
                                   Age=c$Age,
                                   Population_med=c$Population_med,Population_max=c$Population_max,
                                   Deaths_med=c$val_med,Deaths_max=c$val_max,
                                   YLL.gbd.f_med=c$YLL.gbd.f_med,YLL.gbd.f_max=c$YLL.gbd.f_max,
                                   YLL.un.f_med=c$YLL.un.f_med, YLL.un.f_max=c$YLL.un.f_max,
                                   Excess_Deaths_med=c$Excess_Deaths_med,Excess_Deaths_max=c$Excess_Deaths_max,
                                   YLL.gbd.f.ed_med=c$YLL.gbd.f.ed_med,YLL.gbd.f.ed_max=c$YLL.gbd.f.ed_max, 
                                   YLL.un.f.ed_med=c$YLL.un.f.ed_med,YLL.un.f.ed_max=c$YLL.un.f.ed_max,
                                   YLL.rate.gbd.f_med=c$YLL.rate.gbd.f_med,YLL.rate.gbd.f_max=c$YLL.rate.gbd.f_max,
                                   YLL.rate.un.f_med=c$YLL.rate.un.f_med,YLL.rate.un.f_max=c$YLL.rate.un.f_max,
                                   YLL.rate.gbd.f.ed_med=c$YLL.rate.gbd.f.ed_med,YLL.rate.gbd.f.ed_max=c$YLL.rate.gbd.f.ed_max,
                                   YLL.rate.un.f.ed_med=c$YLL.rate.un.f.ed_med,YLL.rate.un.f.ed_max=c$YLL.rate.un.f.ed_max
                                   
  )
}
names(yll.data.female)<-countries_mf
yll.data.female.all<-do.call(rbind,yll.data.female)
#ver with countries_mf 
saveRDS(yll.data.female,
        file=here("Data","flu-yll-f-list.rds"))
#this is the ver with all countries_mf in one full dataframe
saveRDS(yll.data.female,
        file=here("Data","flu-yll-f.rds"))

###################################################################################################
##
##  AGGREGATION : YLL ABSOLUTE NUMBERS
##  
##
###################################################################################################

# Total YLL by country, YLL rates with standardized population
# File format is: Country, Date.death, Date.edeath, 
# YLL.gbd.b, YLL.un.b,YLL.gbd.b.ed, YLL.un.b.ed, 
# YLL.gbd.m, YLL.un.m, YLL.gbd.m.ed, YLL.un.m.ed,
# YLL.gbd.f, YLL.un.f,YLL.gbd.f.ed, YLL.un.f.ed

# Similarly for rates

# Columns currently included
colnames.yll.measures<-c('Country',"Population",
                         'YLL.gbd.b_med', 'YLL.gbd.b_max','YLL.un.b_med','YLL.un.b_max',
                         'YLL.gbd.b.ed_med','YLL.gbd.b.ed_max', 'YLL.un.b.ed_med', 'YLL.un.b.ed_max',
                         'YLL.gbd.m_med','YLL.gbd.m_max', 'YLL.un.m_med', 'YLL.un.m_max',
                         'YLL.gbd.m.ed_med','YLL.gbd.m.ed_max', 'YLL.un.m.ed_med','YLL.un.m.ed_max',
                         'YLL.gbd.f_med','YLL.gbd.f_max', 'YLL.un.f_med','YLL.un.f_max',
                         'YLL.gbd.f.ed_med','YLL.gbd.f.ed_max', 'YLL.un.f.ed_med', 'YLL.un.f.ed_max')

# Data frame to store results
YLL.measures<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures)))
colnames(YLL.measures)<-colnames.yll.measures
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures$Country[i]))
  tmp.var<-c('YLL.gbd.b_med', 'YLL.gbd.b_max','YLL.un.b_med','YLL.un.b_max',
             'YLL.gbd.b.ed_med','YLL.gbd.b.ed_max', 'YLL.un.b.ed_med', 'YLL.un.b.ed_max')
  YLL.measures[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c('YLL.gbd.m_med','YLL.gbd.m_max', 'YLL.un.m_med', 'YLL.un.m_max',
             'YLL.gbd.m.ed_med','YLL.gbd.m.ed_max', 'YLL.un.m.ed_med','YLL.un.m.ed_max')
  YLL.measures[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var],na.rm=TRUE)
  tmp.var<-c('YLL.gbd.f_med','YLL.gbd.f_max', 'YLL.un.f_med','YLL.un.f_max',
             'YLL.gbd.f.ed_med','YLL.gbd.f.ed_max', 'YLL.un.f.ed_med', 'YLL.un.f.ed_max')
  YLL.measures[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var],na.rm=TRUE)
}

# saving
saveRDS(YLL.measures,
         file=here("Data","flu_YLL_measures.rds"))

###################################################################################################
##
##  AGGREGATION : YLL RATES ESP
##  
##
###################################################################################################

#   
colnames.yll.measures.rate.esp<-c('Country', 
                                  'YLL.rate.gbd.b_med','YLL.rate.gbd.b_max', 'YLL.rate.un.b_med','YLL.rate.un.b_max',
                                  'YLL.rate.gbd.b.ed_med','YLL.rate.gbd.b.ed_max', 'YLL.rate.un.b.ed_med','YLL.rate.un.b.ed_max', 
                                  'YLL.rate.gbd.m_med','YLL.rate.gbd.m_max', 'YLL.rate.un.m_med','YLL.rate.un.m_max',
                                  'YLL.rate.gbd.m.ed_med','YLL.rate.gbd.m.ed_max', 'YLL.rate.un.m.ed_med','YLL.rate.un.m.ed_max',
                                  'YLL.rate.gbd.f_med','YLL.rate.gbd.f_max', 'YLL.rate.un.f_med','YLL.rate.un.f_max',
                                  'YLL.rate.gbd.f.ed_med','YLL.rate.gbd.f.ed_max', 'YLL.rate.un.f.ed_med','YLL.rate.un.f.ed_max')
# Data frame to store results
YLL.measures.rate.esp<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.esp)))
colnames(YLL.measures.rate.esp)<-colnames.yll.measures.rate.esp
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.esp$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.esp$Country[i]))
  tmp.var<-c('YLL.rate.gbd.b_med','YLL.rate.gbd.b_max', 'YLL.rate.un.b_med','YLL.rate.un.b_max',
             'YLL.rate.gbd.b.ed_med','YLL.rate.gbd.b.ed_max', 'YLL.rate.un.b.ed_med','YLL.rate.un.b.ed_max')
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c('YLL.rate.gbd.m_med','YLL.rate.gbd.m_max', 'YLL.rate.un.m_med','YLL.rate.un.m_max',
             'YLL.rate.gbd.m.ed_med','YLL.rate.gbd.m.ed_max', 'YLL.rate.un.m.ed_med','YLL.rate.un.m.ed_max')
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
  tmp.var<-c('YLL.rate.gbd.f_med','YLL.rate.gbd.f_max', 'YLL.rate.un.f_med','YLL.rate.un.f_max',
             'YLL.rate.gbd.f.ed_med','YLL.rate.gbd.f.ed_max', 'YLL.rate.un.f.ed_med','YLL.rate.un.f.ed_max')
  YLL.measures.rate.esp[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*pop_std.esp$percent_pop,na.rm=TRUE)
}

# saving
saveRDS(YLL.measures.rate.esp,
        file=here("Data","flu_YLL_measures_rate_esp.rds"))
###################################################################################################
##
##  AGGREGATION : YLL RATES GBD
##  
##
###################################################################################################


colnames.yll.measures.rate.gbd<-c('Country', 'Date.death', 'Date.edeath', 
                                  'YLL.rate.gbd.b_med','YLL.rate.gbd.b_max', 'YLL.rate.un.b_med','YLL.rate.un.b_max',
                                  'YLL.rate.gbd.b.ed_med','YLL.rate.gbd.b.ed_max','YLL.rate.un.b.ed_med','YLL.rate.un.b.ed_max', 
                                  'YLL.rate.gbd.m_med','YLL.rate.gbd.m_max', 'YLL.rate.un.m_med','YLL.rate.un.m_max',
                                  'YLL.rate.gbd.m.ed_med','YLL.rate.gbd.m.ed_max', 'YLL.rate.un.m.ed_med','YLL.rate.un.m.ed_max',
                                  'YLL.rate.gbd.f_med','YLL.rate.gbd.f_max','YLL.rate.un.f_med','YLL.rate.un.f_max',
                                  'YLL.rate.gbd.f.ed_med','YLL.rate.gbd.f.ed_max','YLL.rate.un.f.ed_med','YLL.rate.un.f.ed_max')

# Data frame to store results
YLL.measures.rate.gbd<-as.data.frame(matrix(NA,nrow=length(countries),ncol=length(colnames.yll.measures.rate.gbd)))
colnames(YLL.measures.rate.gbd)<-colnames.yll.measures.rate.gbd
## Framework -- out of the loop
###################################################################################################
# Countries  
YLL.measures.rate.gbd$Country<-countries
for (i in 1:length(countries)){
  print(countries[i])
  tmp.ind<-which(as.character(yll.data.both.all$Country)==as.character(YLL.measures.rate.gbd$Country[i]))
  tmp.var<-c('YLL.rate.gbd.b_med','YLL.rate.gbd.b_max', 'YLL.rate.un.b_med','YLL.rate.un.b_max',
             'YLL.rate.gbd.b.ed_med','YLL.rate.gbd.b.ed_max','YLL.rate.un.b.ed_med','YLL.rate.un.b.ed_max')
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.both.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c('YLL.rate.gbd.m_med','YLL.rate.gbd.m_max', 'YLL.rate.un.m_med','YLL.rate.un.m_max',
             'YLL.rate.gbd.m.ed_med','YLL.rate.gbd.m.ed_max', 'YLL.rate.un.m.ed_med','YLL.rate.un.m.ed_max')
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.male.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
  tmp.var<-c('YLL.rate.gbd.f_med','YLL.rate.gbd.f_max','YLL.rate.un.f_med','YLL.rate.un.f_max',
             'YLL.rate.gbd.f.ed_med','YLL.rate.gbd.f.ed_max','YLL.rate.un.f.ed_med','YLL.rate.un.f.ed_max')
  YLL.measures.rate.gbd[i,tmp.var]<-colSums(yll.data.female.all[tmp.ind, tmp.var]*(pop_std.gbd$percent_pop/100),na.rm=TRUE)
}

# saving
saveRDS(YLL.measures.rate.gbd,
        file=here("Data","flu_YLL_measures_rate_gbd.rds"))


###################################################################################################
##
##  AGGREGATION : YLL RATES COUNTRY-SPECIFIC
##  
##
###################################################################################################

dat<-readRDS(file=here("Data","flu_YLL_measures.rds"))

sumpop<-tapply(pop_med$Total,pop_med$Country,sum)
dat$Population_med<-sumpop[which(names(sumpop)%in%dat$Country)]
sumpop<-tapply(pop_max$Total,pop_max$Country,sum)
dat$Population_max<-sumpop[which(names(sumpop)%in%dat$Country)]

rates_med<-dat[,c('YLL.gbd.b_med', 'YLL.un.b_med')]/dat$Population_med *100000
rates_max<-dat[,c('YLL.gbd.b_max','YLL.un.b_max')]/dat$Population_max *100000
yll_rates<-as.data.frame(cbind(dat$Country,rates_med,rates_max,dat$Population_med,dat$Population_max))
names(yll_rates)<-c("Country"
                    ,'YLL.gbd.b_med_cpop', 'YLL.un.b_med_cpop','YLL.gbd.b_max_cpop','YLL.un.b_max_cpop'
                    ,"Pop_med","Pop_max")


# saving
saveRDS(yll_rates,
        file=here("Data","flu_YLL_measures_rate_cpop.rds"))

