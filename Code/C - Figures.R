


tmp_file<-here("Data","table_covid_other.csv")
Other   <- read_csv(paste(tmp_file)) %>% 
  select(-Date,-X1) %>% 
  pivot_longer(2:9, 
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
continent_colors = paletteer_d("yarrr::basel") %>% as.character() %>% '['(1:5)
names(continent_colors) <- unique(Other$Continent)
## Subset to countries
sample_affected<-readRDS(here("Data","Final results/sample_affected.rds"))
Other<-subset(Other,Country%in%sample_affected$Country)
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
Base$Continent<-countrycode(as.character(Base$Country),origin='country.name',destination='continent')
Base$Continent[Base$Country=="England"]<-"Europe"
Base$Continent_col[Base$Continent=="Africa"]<-plot_col2[1]
Base$Continent_col[Base$Continent=="Americas"]<-plot_col2[2]
Base$Continent_col[Base$Continent=="Asia"]<-plot_col2[3]
Base$Continent_col[Base$Continent=="Europe"]<-plot_col2[4]
Base$Continent_col[Base$Continent=="Oceania"]<-plot_col2[5]
#Base<-Base[order(Base$Continent,Base$Country,decreasing=TRUE),]
Base$Country<-factor(Base$Country,levels=Base$Country)
#colors
plot_col<-viridis(n=5,alpha=1,begin=0,end=1,option="E")
plot_col2<-viridis(n=5,alpha=1,begin=0,end=1,option="D")


## Figs Ordered by Value
## Figs include all countries
## Fig 1: order Flu, Transport, Heart, Excess
## Fig 2: Use Log rates
all_countries<-sort(unique(Base$Country))
## Figure 1:
Base3<-Base
Base3<-Base3[order(Base3$`Flu (med)`,Base3$Transport,Base3$Heart,Base3$Excess,decreasing=FALSE),]
Base3$Country<-factor(as.character(Base3$Country),levels=as.character(Base3$Country))
  ## Option A: multipanel
{
  # Transport
  p1<-ggplot(Base3, aes(x=Transport, y=Country)) +
    geom_point(shape=17,color=plot_col2[1], size=3.5, alpha=1) +
    theme_light() +
    ylab("")+ xlab("B. Transport") +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour= "gray20")
    )
  # Flu
  p2<-ggplot(Base3, aes(x=`Flu (med)`, y=Country)) +
    geom_point(shape=15,color=plot_col2[2], size=3.5, alpha=1) +
    geom_point(aes(x=`Flu (max)`,y=Country),shape=15,color=plot_col2[3],size=3.5) +
    #legend
    geom_point(data=data.frame(x=c(5.3,5.3),y=c(5,3))
               ,aes(x=x,y=y),shape=c(15,15),color=c(plot_col2[2:3]),size=3.5) +
    annotate(geom="text",x=c(5.2,5.2),y=c(4,2),label=c("Median yr","Max yr"),size=3,col="gray20") +
    theme_light() +
    ylab("")+ xlab("A. Flu") +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20")
    )
  # Heart
  p3<-ggplot(Base3, aes(x=Heart, y=Country)) +
    geom_point(shape=16,color=plot_col2[4], size=3.5, alpha=1) +
    theme_light() +
    ylab("")+ xlab("C. Heart") +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20")
    )
  # Excess
  p4<-ggplot(Base3, aes(x=Excess, y=Country)) +
    geom_point(shape=25,fill=plot_col2[5],color=plot_col2[5], size=3.5, alpha=1) +
    theme_light() +
    ylab("")+ xlab("D. Excess Mortality") +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20")
    )
  grid.arrange(p2,p1,p3,p4, ncol=4)
}

  ## Option B: single panel (too busy/smushed but for comparison)
{
  my_shape<-c(15,15,17,16,25) #Flu_med, Flu_max, Transport, Heart, Excess
  ggplot(Base3, aes(x=`Flu (med)`, y=Country)) +# Flu_med
    geom_point(shape=my_shape[1],color=plot_col2[1], size=3.5, alpha=1) +
    geom_point(aes(x=`Flu (max)`,y=Country),shape=my_shape[2],color=plot_col2[2],size=3.5) + #Flu_max
    geom_point(aes(x=Transport,y=Country),shape=my_shape[3],color=plot_col2[3], size=3.5, alpha=1) +#Transport
    geom_point(aes(x=Heart,y=Country),shape=my_shape[4],color=plot_col2[4], size=3.5, alpha=1) +#Heart
    geom_point(aes(x=Excess,y=Country),shape=my_shape[5],fill=plot_col2[5],color=plot_col2[5], size=3.5, alpha=1) +#Excess
    theme_light() +
    ylab("")+ xlab("Rates") +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20")) +
    #legend
    geom_point(data=data.frame(x=rep(5,5),y=seq(10,2,-2))
               ,aes(x=x,y=y),shape=my_shape,color=plot_col2,fill=plot_col2,size=3.5) +
    annotate(geom="text",x=rep(5.4,5),y=seq(10,2,-2)
             ,label=c("Flu (median yr)","Flu (max yr)","Transport","Heart","Excess"),size=3,col="gray20")
  
}

## Figure 2:
  ## GENDER
gender2<-gender
gender2<-gender2[order(gender2$Rate,gender2$Country,decreasing=FALSE),]
gender2$Country<-factor(as.character(gender2$Country),levels=as.character(gender2$Country))
#fill in with NAs missing countries
tmp<-data.frame(Country=all_countries[!all_countries%in%gender2$Country],Rate=rep(NA,13)
                ,Continent=c("Oceania",rep("Europe",5),rep("Asia",2),rep("Americas",2),rep("Africa",3))
                ,Continent_col=rep(NA,13))
gender2<-rbind(tmp,gender2) #ordered so NA is top with lowest rates
gender2$Country<-factor(as.character(gender2$Country),levels=as.character(gender2$Country))
gender2$LRate<-log(gender2$Rate)
gender2$LRate_color<-ifelse(gender2$LRate>0,plot_col[4],ifelse(gender2$LRate<0,plot_col[1],"white"))
  ## AGE
age2<-data[order(data$Proportion,decreasing=FALSE),]
  #order by under 55 proportion countries
tmp<-subset(age2,Age=="Under 55")
tmp<-tmp[order(tmp$Proportion,decreasing=FALSE),]
age2$Country<-factor(as.character(age2$Country),levels=as.character(tmp$Country))
  #order by 75+ proportion countries
tmp<-subset(age2,Age=="75+")
tmp<-tmp[order(tmp$Proportion,decreasing=FALSE),]
age3<-age2
age3$Country<-factor(as.character(age3$Country),levels=as.character(tmp$Country))
  ## Option A
{
  ##Gender##
    ## regular rate
  plot1_reg <- ggplot(gender2, aes(y=Country, x=Rate)) +
    geom_segment( aes(y=Country, yend=Country, x=1, xend=Rate), color=viridis(n=1,alpha=1,begin=0.5,end=1,option="E")) +
    #geom_point( color=viridis(n=1,alpha=1,begin=0.25,end=1,option="E"), size=4) +
    geom_point( color=gender2$LRate_color, size=4) +
    xlim(-2,4.5) + 
    geom_vline(xintercept=1,linetype="solid"
               ,color=viridis(n=1,alpha=1,begin=0.25,end=1,option="E"),size=1.25) +
    theme_bw() + xlab("B. Gender YLL Rate") + ylab("") + ggtitle("B")+
    annotate("text", x = c(-0.75,3), y = rep(10,2)
             , label=c("Women More Affected","Men More Affected"), alpha=0.5, cex=5) +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20")
    )
    ## log rate
  plot1_log <- ggplot(gender2, aes(y=Country, x=LRate)) +
    geom_segment( aes(y=Country, yend=Country, x=0, xend=LRate)
                  , color=viridis(n=1,alpha=1,begin=0.5,end=1,option="E")) +
    geom_point( color=gender2$LRate_color, size=4) +
    xlim(-1,1.5) + 
    geom_vline(xintercept=0,linetype="solid"
               ,color=viridis(n=1,alpha=1,begin=0.25,end=1,option="E"),size=1.25) +
    theme_bw() + xlab("Gender YLL Log Rate") + ylab("") + ggtitle("B")+
    annotate("text", x = c(-0.65,0.65), y = rep(3,2)
             , label=c("Women More Affected","Men More Affected"), alpha=0.5, cex=4) +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20")
    )
  ## Double panel: Part B, Age ##
  unique_data<-unique(age2[c("Country", "Continent","Continent_col")])
    ##ordered by 55
  plot2_55<-ggplot(age2, aes(fill=Age, y=Proportion, x=Country)) + 
    geom_bar(position="stack", stat="identity") + coord_flip() +
    scale_fill_viridis(discrete = T,option = "E", alpha=0.85) +
    xlab("")+ylab("Proportion of Total YLL")+ ggtitle("A")+
    theme_bw()+
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20"))
  ##ordered by 75+
  plot2_75<-ggplot(age3, aes(fill=Age, y=Proportion, x=Country)) + 
    geom_bar(position="stack", stat="identity") + coord_flip() +
    scale_fill_viridis(discrete = T,option = "E", alpha=0.85) +
    xlab("")+ylab("Proportion of Total YLL")+ ggtitle("A")+
    theme_bw()+
    theme(panel.border = element_blank(), axis.ticks.y = element_blank()
          , axis.text.y=element_text(colour="gray20"))
  ##arrange ver A1: gender rate reg, age 55
  grid.arrange(plot2_55, plot1_reg, ncol=2)
  ##arrange ver B1: gender rate log, age 55
  grid.arrange(plot2_55, plot1_log, ncol=2)
  ##arrange ver A2: gender rate reg, age 75
  grid.arrange(plot2_75, plot1_reg, ncol=2)
  ##arrange ver B2: gender rate log, age 75
  grid.arrange(plot2_75, plot1_log, ncol=2)
}