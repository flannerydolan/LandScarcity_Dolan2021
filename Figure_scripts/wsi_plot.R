# water scarcity plot


water<-readRDS("query_data/water.rds")

run<-readRDS("query_data/Baseline_runoff.rds")

run %>% filter(value !=0)-> run

water %>% left_join(run,by=c("SSP","Socioeconomics","agLU","RCP","CropModel","ESM","region","year"))%>%
  mutate(scarcity=diff/value) ->scarcity

scarcity %>% filter(Policy=="Land Conservation"|Policy=="All Biofuels"|Policy=="Second Gen. Biofuels")->scarcity



ggplot(scarcity,aes(region,scarcity))+geom_boxplot(outlier.shape=NA)+theme_bw()+
  theme(axis.text.x=element_text(angle=90),axis.title=element_text(size=14))+
  ylab(expression(Delta*WSI))+facet_wrap(~Policy,ncol=1)+coord_cartesian(ylim=c(-.25,.45))

ggsave("wsi.png",dpi=300)
