# tradeoff plot
library(GGally)
library(ggplot2)
library(viridis)
library(dplyr)


# read in metric dat and summarize by scenario

price<-readRDS('query_data/ag-prices.rds')



price %>% dplyr::mutate(perc=(diff/Base_Value)*100) %>% dplyr::group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  dplyr::summarise(diff=mean(perc,na.rm=T)) -> price1

price1$Metric="Crop Price"



prod<-readRDS('query_data/ag-production.rds')


prod %>% dplyr::mutate(perc=(diff/Base_Value)*100) %>%group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=mean(perc,na.rm=T)) -> prod1


prod1$Metric="Crop Production"


water<-readRDS('query_data/water.rds')

water %>% dplyr::mutate(perc=(diff/Base_Value)*100)%>%group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=mean(perc,na.rm=T)) -> water1



water1$Metric="Water Withdrawals"


luce<-readRDS('query_data/LUC-emissions.rds')


luce %>%group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=sum(diff,na.rm=T),Base_Value=sum(Base_Value,na.rm=T)) -> luce1


c<-readRDS('query_data/carbon.rds')



c %>%group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=sum(diff,na.rm=T),Base_Value=sum(Base_Value,na.rm=T)) -> c1



c1 %>% mutate(SSP=substr(SSP,4,4)) %>%
  left_join(luce1,by=c("SSP","Socioeconomics","agLU","RCP","ESM","CropModel","Policy","region")) %>%
  mutate(diff=((diff.x+diff.y)/(Base_Value.x+Base_Value.y))*100) -> allc1

allc1$Metric="Carbon Emissions"

na.omit(allc1)->allc1

### bind everything

price1 %>% mutate(SSP=substr(SSP,4,4)) %>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region) %>% mutate(diff=diff*-1)->price1
prod1 %>% mutate(SSP=substr(SSP,4,4))%>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region) -> prod1
water1 %>% mutate(SSP=substr(SSP,4,4))%>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region)%>%
  mutate(diff=-diff)->water1
allc1 %>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region) %>%
  mutate(diff=-diff)-> allc1

rbind(price1,prod1,water1,allc1) -> env1


# filter out specific regions to plot

env1 %>% filter(region=="Brazil"|region=="USA"|region=="Pakistan"|region=="India"|region=="Africa_Eastern")->env2

# Average all dimensions but plotted ones

env2 %>% group_by(SSP,Policy,Metric,region) %>% summarize(diff=mean(diff,na.rm=T))->pl

# get data into format needed by ggparcoord



pl %>% mutate(diff=sign(diff)*log10(abs(diff)+1))%>% tidyr::spread(region,diff)->pltest


pltest %>% tidyr::gather(region,diff,Africa_Eastern:USA)->pltest2
pltest2 %>% tidyr::spread(Metric,diff)-> pl2
pl2 %>% ungroup() ->pl2

pl2$SSP=as.factor(pl2$SSP)

pl2 %>% filter(Policy !="Baseline")->pl3

pl3 %>% rename(Water=`Water Withdrawals`,Carbon=`Carbon Emissions`,Price=`Crop Price`,Production=`Crop Production`)->pl3

# plot

p2<-ggparcoord(pl3,mapping=aes(linetype=as.factor(SSP)),columns = 4:7,groupColumn = "region", scale="globalminmax",order=c(5,4,6,7))+theme_bw()+
  scale_x_discrete(expand=c(0,0))+ 
  facet_wrap(~factor(Policy,levels=c("All Biofuels","All Biofuels and Land Conservation","Second Gen. Biofuels","Second Gen. Biofuels and Land Conservation","Land Conservation")),ncol=2)+   # Adjust alpha argument
  guides(color = guide_legend(override.aes = list(alpha = 1)))+
  ylab("Log Modulus of % Change")+xlab("Metric")+labs(linetype="SSP")+
  scale_color_brewer(palette = "Dark2")+
  guides(colour = guide_legend(ncol = 2),linetype = guide_legend(ncol =2))+
  theme(axis.title=element_text(size=18),axis.text.y=element_text(size=12),axis.text.x=element_text(hjust=0.4,vjust=.5,size=12,angle=90),legend.position = c(.76,0.075),legend.box="horizontal")
p2$data %>% mutate(Policy=plyr::mapvalues(Policy,1:5,unique(pl3$Policy)))->p2$data

p2


ggsave("tradeoffs.png",p2,dpi=300)
