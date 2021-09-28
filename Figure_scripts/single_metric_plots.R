# single metric plots

luce<-readRDS('LUC-emissions.rds')

luce %>% tidyr::gather(ChartType,value,Policy_Value,Base_Value) -> luce

luce %>% mutate(Policy=ifelse(ChartType=="Base_Value","Baseline",Policy)) %>%
  mutate(diff=ifelse(Policy=="Baseline",value,diff))->luce

luce %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,year)%>%
  summarize(diff=sum(diff,na.rm=T)) ->luce1


carbon<-readRDS('carbon.rds')

carbon %>% tidyr::gather(ChartType,value,Policy_Value,Base_Value) -> c

c %>% mutate(Policy=ifelse(ChartType=="Base_Value","Baseline",Policy)) %>%
  mutate(diff=ifelse(Policy=="Baseline",value,diff))->c

c %>% mutate(diff=diff*1e6)->c

c %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,year)%>%
  summarize(diff=sum(diff,na.rm=T)) ->c1 



c1 %>% mutate(SSP=substr(SSP,4,4)) %>%
  left_join(luce1,by=c("SSP","Socioeconomics","agLU","RCP","ESM","CropModel","Policy","year")) %>%
  mutate(emissions=diff.x+(diff.y)) -> allc1


allc1 %>% tidyr::unite(scenario,SSP,Socioeconomics,agLU,RCP,ESM,CropModel,sep="_")%>%
  mutate(SSP=substr(scenario,1,1))->all2





ggplot(all2)+
  geom_smooth(stat = 'summary', alpha = 0.2, aes(x=year,y=emissions,fill = SSP, color = SSP),
              fun.data = median_hilow, fun.args = list(conf.int = 1)) + 
  facet_wrap(~factor(Policy,levels=c("Baseline","All Biofuels","Second Gen. Biofuels","Land Conservation","All Biofuels and Land Conservation","Second Gen. Biofuels and Land Conservation")),scales="free") +
  theme_bw()+scale_x_continuous(expand=c(0,0))+scale_color_viridis(option="C",discrete=TRUE)+
  scale_fill_viridis(discrete = TRUE,option="C")+
  ylab(bquote("Baseline and Change in Carbon Emissions (MTC)"))+theme(axis.title=element_text(size=16))

ggsave("carbon.png",dpi=300)
##
# water

water<-readRDS('query_data/water.rds')

water %>% tidyr::gather(ChartType,value,Policy_Value,Base_Value) -> water

water %>% mutate(Policy=ifelse(ChartType=="Base_Value","Baseline",Policy)) %>%
  mutate(diff=ifelse(Policy=="Baseline",value,diff))->water

water %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,year)%>%
  summarize(diff=sum(diff,na.rm=T)) -> water1

water1 %>% tidyr::unite(scenario,SSP,Socioeconomics,agLU,RCP,ESM,CropModel,sep="_")%>%
  mutate(SSP=substr(scenario,4,4))->water2

water2 %>% group_by(year,Policy,SSP)%>%summarize(mean=mean(diff,na.rm=T))->mean

water2 %>% left_join(mean,by=c("Policy","year","SSP"))->water3




ggplot(water3)+
  geom_smooth(stat = 'summary', alpha = 0.2, aes(x=year,y=diff,fill = SSP, color = SSP),
              fun.data = median_hilow, fun.args = list(conf.int = 1)) + 
  facet_wrap(~factor(Policy,levels=c("Baseline","All Biofuels","Second Gen. Biofuels","Land Conservation","All Biofuels and Land Conservation","Second Gen. Biofuels and Land Conservation")),scales="free") +
  theme_bw()+scale_x_continuous(expand=c(0,0))+scale_color_viridis(discrete=TRUE,option="C")+
  scale_fill_viridis(discrete = TRUE,option="C")+
  ylab(bquote("Baseline and Change in Water Withdrawals"~(km^3)))+theme(axis.title=element_text(size=16))
       
ggsave("water.png",dpi=300)       
####
# ag prices


price<-readRDS('query_data/ag-prices.rds')

crops=c("Corn","FiberCrop","MiscCrop","OilCrop","OtherGrain","PalmFruit","Rice","RootTuber","SugarCrop","Wheat")

price %>% filter(sector %in% crops)->price

price %>% mutate(perc=(diff/Base_Value)*100)->price
price %>% tidyr::gather(ChartType,value,Policy_Value,Base_Value) -> price

price %>% mutate(Policy=ifelse(ChartType=="Base_Value","Baseline",Policy)) %>%
  mutate(diff=ifelse(Policy=="Baseline",value,diff))->price


price %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,year)%>%
  summarize(perc=mean(perc,na.rm=T)) -> price1


price1 %>% tidyr::unite(scenario,SSP,Socioeconomics,agLU,RCP,ESM,CropModel,sep="_")%>%
  mutate(SSP=substr(scenario,4,4))->price2


ggplot(price2)+
  geom_smooth(stat = 'summary', alpha = 0.2, aes(x=year,y=perc,fill = SSP, color = SSP),
              fun.data = median_hilow, fun.args = list(conf.int = 1)) + 
  #stat_summary(geom="ribbon",fun.data="mean_cl_boot",fun.args=list(conf.int=0.1),aes(x=year,y=perc,fill=SSP),alpha=0.5)+
  facet_wrap(~factor(Policy,levels=c("Baseline","All Biofuels","Second Gen. Biofuels","Land Conservation","All Biofuels and Land Conservation","Second Gen. Biofuels and Land Conservation")),scales="free") +
  theme_bw()+scale_x_continuous(expand=c(0,0))+scale_fill_viridis(discrete = TRUE,option = "C")+
  scale_color_viridis(discrete = TRUE,option = "C")+
  ylab(bquote("Baseline and Change in Crop Prices (%)"))+theme(axis.title=element_text(size=16))

ggsave("agprices_c.png",dpi=300)
#### production

prod<-readRDS('query_data/ag-production.rds')

crops=c("Corn","FiberCrop","MiscCrop","OilCrop","OtherGrain","PalmFruit","Rice","RootTuber","SugarCrop","Wheat")

prod %>% filter(sector %in% crops)->prod
prod %>% tidyr::gather(ChartType,value,Policy_Value,Base_Value) -> prod


prod %>% mutate(Policy=ifelse(ChartType=="Base_Value","Baseline",Policy)) %>%
  mutate(diff=ifelse(Policy=="Baseline",value,diff))->prod


prod %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,year)%>%
  summarize(diff=sum(diff,na.rm=T)) -> prod1


prod1 %>% tidyr::unite(scenario,SSP,Socioeconomics,agLU,RCP,ESM,CropModel,sep="_")%>%
  mutate(SSP=substr(scenario,4,4))->prod2



ggplot(prod2)+
  geom_smooth(stat = 'summary', alpha = 0.2, aes(x=year,y=diff,fill = SSP, color = SSP),
              fun.data = median_hilow, fun.args = list(conf.int = 1)) + 
  #stat_summary(geom="ribbon",fun.data="mean_cl_boot",fun.args=list(conf.int=0.95),aes(x=year,y=diff,fill=SSP),alpha=0.5)+
  #geom_line(aes(year,mean,col=SSP))+
  facet_wrap(~factor(Policy,levels=c("Baseline","All Biofuels","Second Gen. Biofuels","Land Conservation","All Biofuels and Land Conservation","Second Gen. Biofuels and Land Conservation")),scales="free") +
  theme_bw()+scale_x_continuous(expand=c(0,0))+scale_fill_viridis(discrete = TRUE,option = "C")+
  scale_color_viridis(discrete = TRUE,option = "C")+
  ylab(bquote("Baseline and Change in Crop Production (Mt)"))+theme(axis.title=element_text(size=16))

ggsave("ag_production.png",dpi=300)
