library(tidyverse)
library(readr)
library(ggplot2)
library(countrycode)
library(paletteer) 
library(grid)
library(viridis)
library(scales) # to access break formatting functions
#grid stuff
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(ggExtra)
#tables
library(xtable)
#colors
plot_col<-viridis(n=5,alpha=1,begin=0,end=1,option="E")
plot_col2<-viridis(n=5,alpha=1,begin=0,end=1,option="D")

tmp_file<-"../Data/Final results/table_covid_other.csv"
## Fig 1 data
Other   <- read_csv(paste(tmp_file)) %>% 
  pivot_longer(4:11, 
               names_to = "YLL_type",
               values_to = "YLL") %>% 
  mutate(YLL_name = 
           case_when(YLL_type == "YLL_rates_covid" ~ "COVID",
                     YLL_type == "YLL_rates_heart" ~ "Heart",
                     YLL_type == "YLL_rates_drug" ~ "Drug",
                     YLL_type == "YLL_rates_transport" ~ "Transport",
                     YLL_type == "YLL_rates_flu_max" ~ "Flu (max)",
                     YLL_type == "YLL_rates_flu_med" ~ "Flu (med)",
                     YLL_type == "YLL_excess_ratio" ~ "Excess",
                     YLL_type == "YLL_projection_ratio" ~ "Projection"))

Base <- 
  Other %>% 
  pivot_wider(id_cols = c("Country"),
              names_from = "YLL_name", 
              values_from = "YLL") %>% 
  mutate(COVmean = mean(COVID))
## Create proportions: COVID/Other --> Other
Base$Heart<-Base$COVID/Base$Heart
Base$Drug<-Base$COVID/Base$Drug
Base$Transport<-Base$COVID/Base$Transport
Base$`Flu (max)`<-Base$COVID/Base$`Flu (max)`
Base$`Flu (med)`<-Base$COVID/Base$`Flu (med)`
Base$Excess<-1/Base$Excess
Base$Country<-factor(Base$Country,levels=Base$Country)
##Pull in days since first covid data -- need to update always to latest dates manually
death_dates<-readRDS("../Data/Final results/table_deaths_dates.rds")
days<-read.csv("../Data/days_since_first_covid_data.csv")
matched<-match(as.character(Base$Country),as.character(days$Country))
Base$Diff.Days<-days$Diff.Days[matched[which(!is.na(matched))]]
##
Base2<-Base
Base2<-Base2[order(Base2$`Flu (med)`,Base2$Transport,Base2$Heart,Base2$Excess,decreasing=FALSE),]
Base2$Country<-factor(as.character(Base2$Country),levels=as.character(Base2$Country))
Base2$Country_day<- paste(Base2$Country," (",Base2$Diff.Days,")",sep="") #variable with Country followed by day in parentheses
Base2$Country_day<-factor(as.character(Base2$Country_day),levels=as.character(Base2$Country_day))
all_countries<-sort(unique(Base$Country))
### Fig 2 data
gender<-readRDS("../Data/Final results/YLL_covid19_complete.rds")
gender<-gender[which(gender$Country!="Cameroon"),]#subset out NA
gender$Continent<-countrycode(as.character(gender$Country),origin='country.name',destination='continent')
gender$Continent[gender$Country=="England"]<-"Europe"
gender$Continent_col[gender$Continent=="Africa"]<-plot_col2[1]
gender$Continent_col[gender$Continent=="Americas"]<-plot_col2[2]
gender$Continent_col[gender$Continent=="Asia"]<-plot_col2[3]
gender$Continent_col[gender$Continent=="Europe"]<-plot_col2[4]
gender$Continent_col[gender$Continent=="Oceania"]<-plot_col2[5]
gender<-gender[order(gender$Continent,gender$Country,decreasing=TRUE),]
gender$Country<-factor(gender$Country,levels=gender$Country)
gender$Rate<-gender$YLL_rates_male_to_female
gender2<-gender
gender2<-gender2[order(gender2$Rate,gender2$Country,decreasing=FALSE,na.last=FALSE),]
gender2$Country<-factor(as.character(gender2$Country),levels=as.character(gender2$Country))
gender2$Rate_color<-ifelse(gender2$Rate>1,plot_col[4],ifelse(gender2$Rate<1,plot_col[1],"white"))
#add global average to bottom 
#create population weights for weighted average
pop<-read.csv("../Data/Processed/pop_complete.csv")
pop<-data.frame(Country=names(tapply(pop$Total,pop$Country,sum)),Population=tapply(pop$Total,pop$Country,sum))
gender2$pop<-pop$Population[match(gender2$Country,pop$Country)]
global_gender<-data.frame(Country=c("","WEIGHTED AVERAGE","AVERAGE"),Date=rep(NA,3),YLL.b_abs=rep(NA,3)
                          ,YLL.b_rates=rep(NA,3),YLL.m_rates=rep(NA,3),YLL.f_rates=rep(NA,3),Deaths=rep(NA,3)
                          ,Deaths_m=rep(NA,3),Deaths_f=rep(NA,3),YLL_per_death_b=rep(NA,3)
                          ,YLL_per_death_males=rep(NA,3),YLL_per_death_females=rep(NA,3)
                          ,YLL_rates_male_to_female=rep(NA,3),Continent=rep(NA,3),Continent_col=rep(NA,3)
                          ,Rate=c(NA
                                  ,sum(gender2$Deaths/sum(gender2$Deaths,na.rm=TRUE)*gender2$YLL_rates_male_to_female,na.rm=TRUE) #weighted
                                  #,sum(gender2$Rate*gender2$pop,na.rm=TRUE)/sum(gender2$pop,na.rm=TRUE)
                                  ,mean(gender2$YLL.m_rates,na.rm=TRUE)/mean(gender2$YLL.f_rates,na.rm=TRUE) #avg
                                  #,mean(gender2$Rate,na.rm=TRUE)
                          ) #NA, weighted avg, avg
                          ,Rate_color=c(NA,plot_col[5],plot_col[5]), pop=rep(NA,3))
gender2<-rbind(gender2,global_gender)
gender2$Country<-factor(as.character(gender2$Country),levels=as.character(gender2$Country))
age<-readRDS("../Data/Final results/table_YLL_age_cut_offs.rds")
data<-data.frame(Country=as.character(rep(age$Country,each=3))
                 ,Age=rep(c("Under 55","55-75","75+"),length(unique(age$Country)))
                 ,Proportion=c(t(age[,2:4]))#row unlist data
)
data<-data %>% mutate(Age = fct_relevel(Age,"75+","55-75","Under 55"))
data$Continent<-countrycode(as.character(data$Country),origin='country.name',destination='continent')
data$Continent[data$Country=="England"]<-"Europe"
data$Continent_col[data$Continent=="Africa"]<-plot_col2[1]
data$Continent_col[data$Continent=="Americas"]<-plot_col2[2]
data$Continent_col[data$Continent=="Asia"]<-plot_col2[3]
data$Continent_col[data$Continent=="Europe"]<-plot_col2[4]
data$Continent_col[data$Continent=="Oceania"]<-plot_col2[5]
data<-data[order(data$Continent,data$Country,decreasing=TRUE),]
data$Country<-as.character(data$Country)
data$Country<-factor(data$Country,levels=unique(data$Country))
age<-data[order(data$Proportion,decreasing=FALSE),]
#age2$Age<-factor(age2$Age, levels = c("Under 55", "55-75", "75+"))
#order by under 55 proportion countries
tmp<-subset(age,Age=="Under 55")
tmp<-tmp[order(tmp$Proportion,decreasing=FALSE),]
age$Country<-factor(as.character(age$Country),levels=as.character(tmp$Country))
#add global average at the bottom so it translates to top (above Bangladesh)
global_age<-data.frame(Country=rep(c("","GLOBAL AVERAGE"),each=3),Age=rep(c("Under 55","55-75","75+"),2)
                       ,Proportion=c(NA,NA,NA,0.3017,0.4487,0.2496),Continent=rep(NA,6),Continent_col=rep(NA,6)) #Globally, 44.8\% of the total YLL can be attributed to the deaths of individuals between 55 and 75 years old, 30.1\% to younger than 55, and 25.1\% to those older than 75. -- from TEXT
#pulled from Table S7 (created below)

age2<-rbind(age,global_age)

#for SI plots the first data YLL_covid19_complete, so just sync up days since into it
si1<-readRDS("../Data/Final results/YLL_covid19_complete.rds")
matched<-match(as.character(si1$Country),as.character(days$Country))
si1$Diff.Days<-days$Diff.Days[matched[which(!is.na(matched))]]
## Figure 1:

### Flu Panel
flu_base_plot <-ggplot(Base2, aes(x=`Flu (med)`, y=Country)) +
  geom_point(shape=15,color=plot_col2[2], size=3.5, alpha=1) +
  geom_point(aes(x=`Flu (max)`,y=Country),shape=15,color=plot_col2[3],size=3.5) +
  #relabel y-axis tick marks
  scale_y_discrete(labels=paste(c(as.character(Base2$Country_day)))) + 
  #legend
  geom_point(data=data.frame(x=c(9.5,9.5),y=c(5,3))
             ,aes(x=x,y=y),shape=c(15,15),color=c(plot_col2[2:3]),size=3.5) +
  ggplot2::annotate(geom="text",x=c(10,10),y=c(4,2),label=c("Median influenza year" ,"Maximum influenza year"),size=3,col=c("gray20")) +
  ggplot2::annotate(geom="text",x=(mean(Base2$`Flu (med)`)+0.5),y=78,label=paste(round(mean(Base2$`Flu (med)`),3)),size=3.5,col=plot_col2[2])+
  ggplot2::annotate(geom="text",x=(mean(Base2$`Flu (max)`)-0.5),y=78,label=paste(round(mean(Base2$`Flu (max)`),3)),size=3.5,col=plot_col2[3])+
  #average lines
  geom_vline(xintercept=mean(Base2$`Flu (med)`) ,linetype="solid", color=plot_col2[2], 
             size=1.25, alpha = 0.65)+
  geom_vline(xintercept=mean(Base2$`Flu (max)`), linetype="solid", color=plot_col2[3], 
             size=1.25, alpha = 0.65)+
  theme_light() + ylab("") + 
  #xlab("Ratio of years of life lost due to COVID-19 vs seasonal influenza") +
  xlab("A. Ratio of years of life lost due to COVID-19 vs seasonal influenza") +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        , axis.text.y=element_text(colour="gray20"))
flu_base_plot
### Transport Panel
transport_base_plot <-ggplot(Base2, aes(x=Transport, y=Country)) +
  geom_point(shape=17,color=plot_col2[1], size=3.5, alpha=1) +
  #relabel y-axis tick marks
  scale_y_discrete(labels=paste(c(as.character(Base2$Country_day)))) + 
  #average lines
  geom_vline(xintercept=mean(Base2$Transport) ,linetype="solid", color=plot_col2[1], 
             size=1.25, alpha = 0.65) +
  ggplot2::annotate(geom="text",x=(mean(Base2$Transport)+0.5),y=78,label=paste(round(mean(Base2$Transport),3)),size=3.5,col=plot_col2[1])+
  theme_light() + ylab("")+ 
  #xlab("Ratio of years of life lost due to COVID-19 vs transport") +
  xlab("B. Ratio of years of life lost due to COVID-19 vs transport") +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        , axis.text.y=element_text(colour="gray20"))
transport_base_plot
### Heart Panel
heart_base_plot <-ggplot(Base2, aes(x=Heart, y=Country)) +
  geom_point(shape=16,color=plot_col2[4], size=3.5, alpha=1) +
  #relabel y-axis tick marks
  scale_y_discrete(labels=paste(c(as.character(Base2$Country_day)))) + 
  #average lines
  geom_vline(xintercept=mean(Base2$Heart) ,linetype="solid", color=plot_col2[4], 
             size=1.25, alpha = 0.65) +
  ggplot2::annotate(geom="text",x=mean(Base2$Heart)-0.1,y=78,label=paste(round(mean(Base2$Heart),3)),size=3.5,col=plot_col2[4])+
  theme_light() + ylab("")+ 
  #xlab("Ratio of years of life lost due to COVID-19 vs heart diseases") +
  xlab("C. Ratio of years of life lost due to COVID-19 vs heart diseases") +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        , axis.text.y=element_text(colour="gray20"))
heart_base_plot
### Excess Panel
excess_base_plot <-ggplot(Base2, aes(x=Excess, y=Country)) +
  geom_point(shape=25,fill=plot_col2[5],color=plot_col2[5], size=3.5, alpha=1) +
  #relabel y-axis tick marks
  xlim(0,1) +
  scale_y_discrete(labels=paste(c(as.character(Base2$Country_day)))) + 
  #average lines
  geom_vline(xintercept=mean(Base2$Excess,na.rm=TRUE) ,linetype="solid", color=plot_col2[5], 
             size=1.25, alpha = 0.65) +
  ggplot2::annotate(geom="text",x=mean(Base2$Excess,na.rm=TRUE)+0.05,y=78,label=paste(round(mean(Base2$Excess,na.rm=TRUE),3)),size=3.5,col="goldenrod3")+ #color too light as yellow#plot_col2[5])+
  theme_light() + ylab("") + 
  #xlab("Ratio of years of life lost due to COVID-19 vs excess mortality") +
  xlab("D. Ratio of years of life lost due to COVID-19 vs excess mortality") +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        , axis.text.y=element_text(colour="gray20"))
excess_base_plot
### Panels 1-4
grid.arrange(flu_base_plot,transport_base_plot,heart_base_plot,excess_base_plot, ncol=2) 
## Fig 1 Deaths-weighted averages in a table for SI
matched<-match(as.character(si1$Country),as.character(Base2$Country))
Base2$Deaths<-si1$Deaths[matched]
fig1_weight<-data.frame(Ratio=c("Ratio COVID YLL to Flu (median year) YLL","Ratio COVID YLL to Flu (max year) YLL"
                                ,"Ratio COVID YLL to Transport YLL","Ratio COVID YLL to Heart disease YLL"
                                ,"Ratio COVID YLL to Excess mortality YLL")
                        ,`Deaths Weighted Average`=c(sum(Base2$Deaths*Base2$`Flu (med)`/sum(Base2$Deaths))
                                                     ,sum(Base2$Deaths*Base2$`Flu (max)`/sum(Base2$Deaths))
                                                     ,sum(Base2$Deaths*Base2$Transport/sum(Base2$Deaths))
                                                     ,sum(Base2$Deaths*Base2$Heart/sum(Base2$Deaths))
                                                     ,sum(Base2$Deaths*Base2$Excess/sum(Base2$Deaths),na.rm=TRUE)) )
x<-xtable(fig1_weight, caption = "Weighted Averages for Figure 1: Ratios of COVID YLL to Other Causes of Mortality",digits=c(3))
x
## Figure 2:
### Age Panel
age55_base_plot<-
  filter(age2,Country!="Finland"&Country!="Austria") %>%  #Austria/Finland have NAs, but we want some space between global avg
  ggplot(aes(fill=Age, y=Proportion, x=Country)) + 
  geom_bar(position="stack", stat="identity") + coord_flip() +
  scale_fill_viridis(discrete = T,option = "E", alpha=0.85) +
  #Add global average values
  ggplot2::annotate(geom="text",x=rep(81,3),y=c(0.16,0.58,0.9)
                    ,label=age2$Proportion[which(age2$Country=="GLOBAL AVERAGE")]
                    ,size=3.5,col=c("gray50","white","white")) + #from S7 Table weighted proportions for Figure 2
  xlab("") + 
  #ylab("Proportion of years of life lost by age group") +
  ylab("A.  Proportion of years of life lost by age group") +
  theme_bw() +
  ggtitle(" ") +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        , axis.text.y=element_text(colour="gray20")) +
  guides(fill = guide_legend(reverse=TRUE)) #-55 first
age55_base_plot
### Gender Panel
tmp_mean<-mean(gender2$YLL.m_rates,na.rm=TRUE)/mean(gender2$YLL.f_rates,na.rm=TRUE)
tmp_mean_weight<-sum(gender2$Deaths/sum(gender2$Deaths,na.rm=TRUE)*gender2$YLL_rates_male_to_female,na.rm=TRUE)
## Running through examples of log-axes
gender3<-filter(gender2,Country!="Algeria"&Country!="Burkina Faso"&Country!="Sierra Leone"&Country!="South Africa"&Country!="Costa Rica"&Country!="Haiti"&Country!="Jamaica"&Country!="Nicaragua"&Country!="Suriname"&Country!="Bangladesh"&Country!="China"&Country!="Indonesia"&Country!="South Korea"&Country!="Albania"&Country!="New Zealand")   #remove countries with NAs
gender_base_plot<- ggplot(gender3, aes(y=Country, x=Rate)) +
  geom_vline(xintercept=1,linetype="solid" ,color=viridis(n=1,alpha=0.5,begin=0.25,end=1,option="E"),size=1.25) +
  geom_point(color=gender3$Rate_color, size=4) +
  geom_segment(aes(y=Country, yend=Country, x=1, xend=Rate), color=gender3$Rate_color)+
  #add in global and weighted avg values
  ggplot2::annotate(geom="text",x=c(2.3,2.2,0.55,1.4),y=c(68,67,7,7) ### FIX THIS!!
                    ,label=c(paste(round(tmp_mean,3)),paste(round(tmp_mean_weight,3))
                             ,"Women\n more\n affected","Men\n more\n affected"),size=c(rep(3.5,2),3.3,3.3),col="gray20")+ 
  theme_bw() + xlab("B.  Ratio of male to female years of life lost") + ylab("") + 
  #theme_bw() + xlab("Ratio of male to female years of life lost") + ylab("") + 
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        ,axis.text.y=element_text(colour="gray20")
        ,plot.title = element_text(face="bold",colour="gray20", size=10))
gender_natural<-gender_base_plot + 
  scale_x_continuous(lim=c(0.5,6.6),breaks = c(0,0.5,1,2,3,4,5,6), labels = c("0","0.5","1","2","3","4","5","6")) +
  theme(axis.text.x = element_text(color = rep("gray50",8)),
        axis.ticks.x = element_line(color = rep("gray90",8),
                                    size = rep(.5,8)))
gender_base_plot2<-ggplot(gender3, aes(y=Country, x=Rate)) +
  geom_vline(xintercept=1,linetype="solid"
             ,color=viridis(n=1,alpha=0.5,begin=0.25,end=1,option="E"),size=1.25) +
  geom_point(color=gender3$Rate_color, size=4) +
  geom_segment(aes(y=Country, yend=Country, x=1, xend=Rate),
               color=gender3$Rate_color)+
  
  #add in global and weighted avg values
  ggplot2::annotate(geom="text",x=c(1.7,1.6,0.75,1.2),y=c(68,67,7,7)
                    ,label=c(paste(round(tmp_mean,3)),paste(round(tmp_mean_weight,3)),"Women\n more\n affected","Men\n more\n affected"),size=c(rep(3.5,2),3.2,3.2),col="gray20")+ 
  theme_bw() + xlab("B.  Ratio of male to female years of life lost") + ylab("") + 
  #theme_bw() + xlab("Ratio of male to female years of life lost") + ylab("") + 
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        , axis.text.y=element_text(colour="gray20")
        ,plot.title = element_text(face="bold",colour="gray20", size=10))
gender_log<-gender_base_plot2 + scale_x_continuous(trans='log10',breaks = c(0.5,1,2,3,4)) + removeGridX()
grid.arrange(age55_base_plot,gender_natural,ncol=2)#Fig2
grid.arrange(age55_base_plot,gender_log,ncol=2) #SFig9_Fig2_log_gender
## Fig 2 Deaths-weighted averages in a table for SI
tmp<-read.csv("../Data/Final results/table_YLL_age.csv")[,-1]
countries<-as.character(sort(unique(Base$Country)))
tmp<-subset(tmp,Country%in%countries)
total_YLL<-sum(tmp$YLL_b)
total_YLL_55<-sum(tmp$YLL_b[which(tmp$Age<55)])/total_YLL
total_YLL_55_75<-sum(tmp$YLL_b[which(tmp$Age>=55&tmp$Age<75)])/total_YLL ### 0-4 --> 0, 5-9--> 5... 70-74->70
total_YLL_75<-sum(tmp$YLL_b[which(tmp$Age>=75)])/total_YLL
total_YLL_by_age<-data.frame(Age=c("Under 55","55-75","75+"),Weighted_Avg=c(total_YLL_55,total_YLL_55_75,total_YLL_75))

x<-xtable(total_YLL_by_age, caption = "Weighted Proportions for Figure 2: Proportion of total global YLL by age",digits=c(4))
x
## SI figures

### YLL and the ratio of YLL to other causes of death both grow nonlinearly/exponentially(?) with days since
check_si1<-subset(si1,select=c(Country,Diff.Days,YLL.b_abs))
si1_base_plot <- ggplot(data=si1,aes(x=Diff.Days,y=YLL.b_abs)) + geom_point(alpha=0.5) + xlim((min(si1$Diff.Days)-5),(max(si1$Diff.Days)+5)) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[1], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID-19 case") 

si1_plot<-si1_base_plot + ylab("Years of life lost") +
  geom_text(data=si1 %>% filter(Country=="USA"|Country=="Brazil"|Country=="Mexico"|Country=="India"| #top
                                  Country=="Chile"|Country=="Italy"|Country=="Spain"|Country=="England"|Country=="Algeria"|
                                  #Country=="France"|
                                  Country=="Sweden"|Country=="Canada")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.1,height=0.1))
si1_log_plot<- si1_base_plot + scale_y_continuous(trans='log10') + ylab("Log scale Years of life lost") +
  geom_text(data=si1 %>% filter(Country=="USA"|Country=="Brazil"|Country=="Algeria"|
                                  Country=="Chile"|Country=="Italy"|Country=="Norway"|Country=="Romania"|
                                  #Country=="France"|
                                  Country=="Mexico"|Country=="Sweden"|Country=="Canada"|
                                  Country=="Taiwan"|Country=="South Africa"|Country=="Japan")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.05,height=0.05))
grid.arrange(si1_plot,si1_log_plot,ncol=2)
si_alpha<-0.5
##FLU
si2_flu_base <- ggplot(data=Base2,aes(x=Diff.Days,y=`Flu (med)`)) + 
  xlim((min(si1$Diff.Days)-5),(max(si1$Diff.Days)+5)) +
  geom_point(color=plot_col2[3], alpha=si_alpha) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID case") 
si2_flu<- si2_flu_base + ylab("Ratio COVID YLL/Flu YLL") +  ggtitle("FLU") +
  geom_text(data=Base2 %>% filter(Country=="Austria"|Country=="Italy"|Country=="USA"|Country=="Spain"| #Top
                                    Country=="Costa Rica"|Country=="Brazil"|
                                    Country=="Chile"|Country=="England"|Country=="Algeria"|
                                    Country=="Nigeria"|Country=="Sweden"|Country=="Taiwan"| #lowest
                                    Country=="Canada") 
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.2,height=0.2))
##Transport
si2_transport_base <- ggplot(data=Base2,aes(x=Diff.Days,y=Transport)) + 
  xlim((min(si1$Diff.Days)-5),(max(si1$Diff.Days)+5)) +
  geom_point(color=plot_col2[1], alpha=si_alpha) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID case") 
si2_transport<- si2_transport_base + ylab("Ratio COVID YLL/Transport YLL") + 
  ggtitle("TRANSPORT") +
  geom_text(data=Base2 %>% filter(Country=="United Kingdom"|Country=="Spain"|Country=="USA"| #top
                                    Country=="Chile"|Country=="Algeria"|
                                    Country=="Mexico"|Country=="Sweden"|Country=="Canada"|
                                    Country=="Taiwan"|Country=="Cuba"|Country=="New Zealand") #low
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.2,height=0.2))
##Heart
si2_heart_base <- ggplot(data=Base2,aes(x=Diff.Days,y=Heart)) + 
  xlim((min(si1$Diff.Days)-5),(max(si1$Diff.Days)+5)) +
  geom_point(color=plot_col2[4], alpha=si_alpha) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID case") 
si2_heart<- si2_heart_base + ylab("Ratio COVID YLL/Heart disease YLL") + 
  ggtitle("HEART DISEASE") +
  geom_text(data=Base2 %>% filter(Country=="Mexico"|Country=="USA"|Country=="United Kingdom"| #top
                                    Country=="Chile"|Country=="Italy"|#Country=="New Zealand"|
                                    Country=="Mexico"|Country=="Canada"|Country=="Malta"|Country=="Bangladesh"|
                                    Country=="Taiwan"|Country=="Cuba") #bottom
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.01,height=0.01))
##Excess
si_excess_data<-Base2[which(!is.na(Base2$Excess)),]
si_excess_data$Country<-factor(as.character(si_excess_data$Country))
si2_excess_base <- ggplot(data=si_excess_data,aes(x=Diff.Days,y=Excess)) + 
  xlim((min(si_excess_data$Diff.Days)-5),(max(si_excess_data$Diff.Days)+7)) +
  geom_point(color=plot_col2[5], alpha=1) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID case") 
si2_excess<- si2_excess_base + ylab("Ratio COVID YLL/Excess mortality YLL") + 
  ggtitle("EXCESS MORTALITY") +
  geom_text(data=si_excess_data %>% filter(Country!="Germany" & Country!="Belgium")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.01,height=0.01))
##FLU
si2_logflu<- si2_flu_base + scale_y_continuous(trans='log10') + ylab("Log scale ratio COVID YLL/Flu YLL") +
  ggtitle("FLU") +
  geom_text(data=Base2 %>% filter(Country=="USA"|Country=="Brazil"|
                                    Country=="Chile"|Country=="Italy"|Country=="Norway"|Country=="Romania"|Country=="Algeria"|
                                    Country=="Portugal"|Country=="Mexico"#|Country=="Sweden"
                                  |Country=="Canada"|
                                    Country=="Taiwan"|Country=="South Africa"|Country=="Japan")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.1,height=0.1))
##Transport
si2_logtransport<- si2_transport_base + scale_y_continuous(trans='log10') + ylab("Log scale ratio COVID YLL/Transport YLL") +
  ggtitle("TRANSPORT") +
  geom_text(data=Base2 %>% filter(Country=="USA"|Country=="New Zealand"|#|Country=="Algeria"
                                    Country=="Chile"|Country=="Italy"|
                                    Country=="Bangladesh"|Country=="Sweden"|Country=="Canada"|
                                    Country=="Taiwan"|Country=="South Africa"|Country=="Japan")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.1,height=0.1))
##Heart
si2_logheart<- si2_heart_base + scale_y_continuous(trans='log10') + ylab("Log scale ratio COVID YLL/Heart YLL") +
  ggtitle("HEART DISEASE") +
  geom_text(data=Base2 %>% filter(Country=="USA"|#Country=="United Kingdom"|
                                    Country=="Chile"|Country=="Italy"|Country=="Malta"|Country=="Bangladesh"|
                                    Country=="Mexico"| Country=="New Zealand"| Country=="Canada"|
                                    Country=="Taiwan"|Country=="South Africa"|Country=="Japan")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.1,height=0.1))
##Excess
si2_logexcess<- si2_excess_base + scale_y_continuous(trans='log10') + ylab("Log scale ratio COVID YLL/Excess mortality YLL") +
  ggtitle("EXCESS MORTALITY") +
  geom_text(data=si_excess_data %>% filter(Country!="Germany" & Country!="Belgium")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.1,height=0.1))
##Flu
grid.arrange(si2_flu,si2_logflu,ncol=2)
##Transport
grid.arrange(si2_transport,si2_logtransport,ncol=2)
##Heart
grid.arrange(si2_heart,si2_logheart,ncol=2)
##Excess
grid.arrange(si2_excess,si2_logexcess,ncol=2)

grid.arrange(si2_flu,si2_logflu,si2_transport,si2_logtransport,si2_heart,si2_logheart,si2_excess,si2_logexcess,ncol=2) 

### F2-A SI plots
age2_55<-subset(age2,Age=="Under 55"&(Country!="GLOBAL AVERAGE"&Country!=""))
matched<-match(as.character(age2_55$Country),as.character(days$Country))
age2_55$Diff.Days<-days$Diff.Days[matched[which(!is.na(matched))]]
age2_75<-subset(age2,Age=="75+"&(Country!="GLOBAL AVERAGE"&Country!=""))
matched<-match(as.character(age2_75$Country),as.character(days$Country))
age2_75$Diff.Days<-days$Diff.Days[matched[which(!is.na(matched))]]
si3_55_base<-ggplot(data=age2_55,aes(x=Diff.Days,y=Proportion)) + 
  geom_point(color=plot_col[1], alpha=si_alpha) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID-19 case") 
si3_55 <- si3_55_base + ylab("Proportion of YLL for Age under 55") + ggtitle("Age under 55") +
  geom_text(data=age2_55 %>% filter(Country=="USA"|Country=="Nigeria"|Country=="Sierra Leone"|#high
                                      Country=="Chile"|Country=="Italy"|Country=="Spain"|Country=="United Kingdom"|
                                      #Country=="France"|
                                      Country=="South Africa"|Country=="Canada"|#|Country=="Bangladesh"
                                      Country=="Luxembourg"|Country=="El Salvador"|Country=="Ethiopia") #low
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.01,height=0.01))
si3_75_base<-ggplot(data=age2_75,aes(x=Diff.Days,y=Proportion)) + 
  geom_point(color=plot_col[3], alpha=si_alpha) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID-19 case") 
si3_75 <- si3_75_base + ylab("Proportion of YLL for Age over 75") + ggtitle("Age over 75") +
  geom_text(data=age2_75 %>% filter(Country=="USA"|Country=="Brazil"|
                                      Country=="Chile"|Country=="Italy"|Country=="Spain"|#Country=="Australia"|
                                      Country=="France"|Country=="Bangladesh"|Country=="South Africa"|Country=="Canada"
                                    |Country=="Togo"|Country=="Malawi"|Country=="Iraq") #low
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.01,height=0.01))
legend_val<-c("Under 55" = plot_col[3], "75+" = plot_col[1])
si3_base <-ggplot(data=age2_55,aes(x=Diff.Days,y=Proportion,group = 1)) + 
  geom_point(color=plot_col[1], alpha=si_alpha) +
  stat_smooth(data=age2_55,aes(x=Diff.Days,y=Proportion,color="Under 55"),method = 'lm'
              , formula = y ~ poly(x,2),  se= TRUE) +
  geom_point(data=age2_75,aes(x=Diff.Days,y=Proportion),color=plot_col[3], alpha=si_alpha) +
  stat_smooth(data=age2_75,aes(x=Diff.Days,y=Proportion, color="75+"),method = 'lm'
              , formula = y ~ poly(x,2), se= TRUE) +
  scale_colour_manual(values = legend_val) +
  theme_bw()+ labs(x="Days since first COVID-19 case",y="Proportion of YLL",color="Legend") +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        ,legend.position = "top",legend.justification = c(0, 1)
        , axis.text.y=element_text(colour="gray20")
        ,plot.title = element_text(face="bold",colour="gray20", size=10))
## F2-B SI plots
gender3<-subset(gender2,Country!="WEIGHTED AVERAGE"&Country!="AVERAGE"&Country!="")
matched<-match(as.character(gender3$Country),as.character(days$Country))
gender3$Diff.Days<-days$Diff.Days[matched[which(!is.na(matched))]]
si4_male_base<-ggplot(data=gender3,aes(x=Diff.Days,y=YLL.m_rates)) + 
  geom_point(color=plot_col[4], alpha=si_alpha) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID-19 case") 
si4_male <- si4_male_base + ylab("YLL rates for men") + ggtitle("Male YLL rates over time") +
  geom_text(data=gender3 %>%filter(Country=="USA"|Country=="Slovenia"|Country=="Argentina"| #high
                                     Country=="Spain"|Country=="Italy"|Country=="Switzerland"|
                                     Country=="Estonia"|Country=="Japan"|Country=="Pakistan"|Country=="Portugal"|Country=="Bolivia") #low
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.01,height=0.01))
si4_female_base<-ggplot(data=gender3,aes(x=Diff.Days,y=YLL.f_rates)) + 
  geom_point(color=plot_col[4], alpha=si_alpha) +
  stat_smooth(method = 'lm', formula = y ~ poly(x,2), color=plot_col[3], se= TRUE) +
  theme_bw()+ xlab("Days since first COVID-19 case") 
si4_female <- si4_female_base + ylab("YLL rates for women") + ggtitle("Female YLL rates over time") +
  geom_text(data=gender3 %>% filter(Country=="USA"|Country=="Colombia"|Country=="Argentina"| #high
                                      Country=="Spain"|Country=="Italy"|Country=="Switzerland"|
                                      Country=="Estonia"|Country=="Japan"|Country=="Pakistan"|Country=="Portugal"|Country=="Bolivia")
            ,aes(label=Country),hjust=-0.0, vjust=0.0
            ,position = position_jitter(width=0.02,height=0.02))
legend_val<-c("Male" = plot_col[3], "Female" = plot_col[1])
si4<-ggplot(data=gender3,aes(x=Diff.Days,y=YLL.m_rates)) + 
  geom_point(color=plot_col[3], alpha=si_alpha) +
  stat_smooth(aes(x=Diff.Days,y=YLL.m_rates,color="Male"),method = 'lm', formula = y ~ poly(x,2), se=TRUE) +
  geom_point(aes(x=Diff.Days,y=YLL.f_rates),color=plot_col[1], alpha=si_alpha) +
  stat_smooth(aes(x=Diff.Days,y=YLL.f_rates,color="Female"), method = 'lm', formula = y ~ poly(x,2),se=TRUE) +
  theme_bw()+ labs(x="Days since first COVID-19 case",y="YLL Rates",color="Legend") +
  scale_colour_manual(values = legend_val) +
  theme(panel.border = element_blank(), axis.ticks.y = element_blank()
        ,legend.position = "top",legend.justification = c(0, 1)
        , axis.text.y=element_text(colour="gray20")
        ,plot.title = element_text(face="bold",colour="gray20", size=10))


