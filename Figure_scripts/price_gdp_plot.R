library(dplyr)
library(ggplot2)

# get percent price increases and join with GDP data

price<-readRDS('ag-prices.rds')


gdp<-read.csv('query_data/gdp.csv')


price %>% mutate(percdiff=(diff/Base_Value)*100)->price



gdp %>% mutate(value=value*2.09)->gdp
price %>% left_join(gdp,by=c("region","year","SSP"))->dat


dat %>% filter(year==2100)->dat


dat %>% filter(region=="Brazil"|region=="USA"|region=="Pakistan"|region=="India"|region=="Africa_Eastern")->reg


crops=c("Corn","FiberCrop","MiscCrop","OilCrop","OtherGrain","PalmFruit",
        "Rice","RootTuber","SugarCrop","Wheat")

# only look at food crops. average across all dimensions but those plotted

reg %>% filter(sector %in% crops) %>%
  group_by(SSP,region,Policy) %>% summarize(value=mean(value,na.rm=T),
                                                 percdiff=mean(percdiff,na.rm=T)) %>%
  mutate(SSP=substr(SSP,4,4),value=value/1e6)->polmean


ggplot(polmean,aes(value,percdiff,col=region,shape=SSP))+theme_bw()+
  geom_point(size=2.5)+ylab("% Change in Price")+xlab("GDP (trillion 2021 USD)")+
  facet_wrap(~factor(Policy,levels=c("Baseline","All Biofuels","Second Gen. Biofuels","Land Conservation",
                                     "All Biofuels and Land Conservation","Second Gen. Biofuels and Land Conservation")),
             scales="free",labeller =label_wrap_gen(width = 25, multi_line = TRUE))+
  scale_color_brewer(palette="Dark2")+
  theme(axis.text.x=element_text(angle=90),axis.title=element_text(size=14),
        legend.position = c(.85,.22),legend.box="horizontal")


ggsave("price_gdp.png",dpi=300)

####################################################################################################################################################
############################################## SI Price GDP plot ###################################################################################
####################################################################################################################################################

# get protected land change ratio

# land conservation land allocation

land90<-readRDS('query_data/LandCons_land-allocation.rds')


# baseline land allocation

base<-readRDS('query_data/Baseline_land-allocation.rds')

group=names(land90)[1:9]

land90 %>% group_by(Units,SSP,RCP,CropModel,ESM,Socioeconomics,agLU,region,year)%>%summarise(value=sum(value,na.rm=T))->all90

# find protected fraction for conservation

land90 %>% mutate(Protected=ifelse(grepl("Protected",crop),TRUE,FALSE)) %>% filter(Protected==TRUE) %>% 
  group_by(Units,SSP,RCP,CropModel,ESM,Socioeconomics,agLU,region,year) %>% summarize(value=sum(value,na.rm=T)) -> p90


p90 %>% left_join(all90,by=group) %>% 
  mutate(fracProtected=value.x/value.y)->frac90

# find protected fraction for base

base %>% group_by(Units,SSP,RCP,CropModel,ESM,Socioeconomics,agLU,region,year)%>%summarise(value=sum(value,na.rm=T))->allbase


base %>% mutate(Protected=ifelse(grepl("Protected",crop),TRUE,FALSE)) %>% filter(Protected==TRUE) %>% 
  group_by(Units,SSP,RCP,CropModel,ESM,Socioeconomics,agLU,region,year) %>% summarize(value=sum(value,na.rm=T)) -> pbase


pbase %>% left_join(allbase,by=group) %>% 
  mutate(fracProtected=value.x/value.y)->fracbase


frac90 %>% left_join(fracbase,by=group) %>% mutate(diff=fracProtected.x-fracProtected.y) ->land

land %>% mutate(percLandChange=(diff)*100)->land

land %>% group_by(region,SSP) %>% summarize(diff=mean(percLandChange,na.rm=T))->landmean

polmean %>% left_join(landmean,by=c("SSP","region")) %>% mutate(NormalizedPercentPriceChange=percdiff/diff)->landdat

landdat %>% mutate(value=value/1e6)->landdat
landdat %>% filter(Policy == "Land Conservation" | Policy=="All Biofuels and Land Conservation"|
                     Policy == "Second Gen. Biofuels and Land Conservation")->landdat

ggplot(landdat,aes(value,NormalizedPercentPriceChange,col=region,shape=SSP))+theme_bw()+
  geom_point(size=2.5)+ylab("% Change in Price per % Change in Protected Land")+xlab("GDP (trillion 2021 USD)")+
  facet_wrap(~factor(Policy,levels=c("Baseline","All Biofuels","Second Gen. Biofuels","Land Conservation","All Biofuels and Land Conservation","Second Gen. Biofuels and Land Conservation")),scales="free",labeller =label_wrap_gen(width = 25, multi_line = TRUE),ncol=1)+
  scale_color_brewer(palette="Dark2")+
  theme(axis.text.x=element_text(angle=90),axis.title=element_text(size=14),
        legend.box="horizontal")

ggsave("price_gdp_normalized_byland_SI.png",dpi=300)
