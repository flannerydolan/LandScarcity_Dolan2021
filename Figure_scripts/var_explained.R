# variance explained plot


library(viridis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

# read in metric dat and summarize by scenario

price<-readRDS('query_data/ag-prices.rds')

price %>% mutate(perc=diff/Base_Value) %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(perc=mean(perc,na.rm=T)) -> price1

price1$diff=price1$perc
price1$Metric="Crop Price"

prod<-readRDS('query_data/ag-production.rds')

prod %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=sum(diff,na.rm=T)) -> prod1


prod1$Metric="Crop Production"

water<-readRDS('query_data/water.rds')

water %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=sum(diff,na.rm=T)) -> water1



water1$Metric="Water Withdrawals"


luce<-readRDS('query_data/LUC-emissions.rds')

luce %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=sum(diff,na.rm=T)) -> luce1


c<-readRDS('query_data/carbon.rds')


c %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,region)%>%
  summarize(diff=sum(diff,na.rm=T)) -> c1



c1 %>% mutate(SSP=substr(SSP,4,4),diff=diff*1e6) %>%
  left_join(luce1,by=c("SSP","Socioeconomics","agLU","RCP","ESM","CropModel","Policy","region")) %>%
  mutate(diff=diff.x+diff.y) -> allc1

allc1$Metric="Carbon Emissions"


### bind everything

price1 %>% mutate(SSP=substr(SSP,4,4)) %>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region) %>% mutate(diff=diff*-1)->price1
prod1 %>% mutate(SSP=substr(SSP,4,4))%>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region) -> prod1
water1 %>% mutate(SSP=substr(SSP,4,4))%>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region)%>%
  mutate(diff=-diff)->water1
allc1 %>% select(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,diff,Metric,region) %>%
  mutate(diff=-diff)-> allc1

rbind(price1,prod1,water1,allc1) -> env1




env1 %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,Metric) %>%
  summarize(diff=sum(diff,na.rm=T))->sum

unique(sum$Policy)->pol
unique(sum$Metric)->met

P=c()

for (p in pol){
  for (m in met){
    
    sum %>% filter(Policy==p,Metric==m)->dat
    dat$agLU = paste0(dat$agLU, '_ag')
    dat$Socioeconomics = paste0(dat$Socioeconomics, '_econ')
    dat$SSP = paste0(dat$SSP, '_NoAgEcon')
    
    anova<-aov(diff~SSP+Socioeconomics+agLU+RCP+ESM+CropModel,data=dat)
    
    broom::tidy(anova)  %>% mutate(varExp=sumsq/sum(sumsq))->varexp
    
    varexp$Metric=m
    varexp$Constraint=p
    
    P=rbind(P,varexp)
  }
}


P1=c()

for (m in met){
  
  sum %>% filter(Metric==m)->dat
  
  dat$agLU = paste0(dat$agLU, '_ag')
  dat$Socioeconomics = paste0(dat$Socioeconomics, '_econ')
  dat$SSP = paste0(dat$SSP, '_NoAgEcon')
  anova<-aov(diff~SSP+Socioeconomics+agLU+RCP+ESM+CropModel+Policy,data=dat)
  
  broom::tidy(anova)  %>% mutate(varExp=sumsq/sum(sumsq))->varexp1
  
  varexp1$Metric=m
  varexp1$Constraint="All"
  
  P1=rbind(P1,varexp1)
  
}

rbind(P,P1)->all

all$Variable=all$term

all %>% mutate(Variable=ifelse(Variable=="Policy","Constraint",Variable))->all

ggplot(all,aes(Metric,varExp,fill=Variable))+geom_col(stat="identity")+theme_bw()+
  facet_wrap(~factor(Constraint,levels=c("All","All Biofuels","Second Gen. Biofuels","Land Conservation","All Biofuels and Land Conservation","Second Gen. Biofuels and Land Conservation")),labeller =label_wrap_gen(width = 25, multi_line = TRUE))+
  #scale_fill_viridis(discrete=TRUE,option="B")+
  scale_fill_brewer(palette = "Accent")+
  ylab("Variance Explained")+
  theme(legend.text=element_text(size=10),legend.title = element_text(size=14),axis.text=element_text(size=10),axis.title = element_text(size=16),strip.text.x = element_text(size = 10),axis.text.x=element_text(angle = 90))

ggsave("varexplained_anova.png",dpi=300)
