# carbon cdf


luce<-readRDS("query_data/LUC-emissions.rds")
carbon <-readRDS("query_data/carbon.rds")



luce %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,year)%>%
  summarize(diff=sum(diff,na.rm=T),Base_Value=sum(Base_Value,na.rm=T)) ->luce1

# tonnes to Mt


carbon %>% group_by(SSP,Socioeconomics,agLU,RCP,ESM,CropModel,Policy,year)%>%
  summarize(diff=sum(diff,na.rm=T),Base_Value=sum(Base_Value,na.rm=T)) ->c1 


c1 %>% mutate(SSP=substr(SSP,4,4)) %>%
  left_join(luce1,by=c("SSP","Socioeconomics","agLU","RCP","ESM","CropModel","Policy","year"))  %>% 
  mutate(diff=diff.x+diff.y,percdiff=(diff/(Base_Value.x+Base_Value.y)*100))-> allc1

luce1$Source="LUC"

c1$Source="FFI"

allc1$Source="All"


rbind(c1,luce1,allc1)->all

all %>% rename(Mt=diff,Constraint=Policy)->all


col_vector=c("purple","red","blue")

ggplot(all,aes(Mt,col=Source,linetype=Constraint))+stat_ecdf(geom="step",size=1)+theme_bw()+
  ylab("f(\u0394 MtC)")+xlab("\u0394 MtC")+scale_color_manual(values = col_vector)+
  geom_vline(xintercept =0,col="black")+
  theme(axis.text=element_text(size=10),axis.title = element_text(size=14))

ggsave('carbon_ecdf.png',dpi=300)


