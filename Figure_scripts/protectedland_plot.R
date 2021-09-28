library(dplyr)
library(cowplot)


basin<-read.csv('mapping/gcam_basin_ids.csv')

glu<-read.csv('mapping/iso_GCAM_regID.csv',skip=5)


glu %>% rename(ISO=iso) %>% mutate(ISO=toupper(ISO)) %>%
  left_join(basin,by="ISO") %>% select(GCAM_basin_ID,GLU_name,GCAM_region_ID)->comb


# baseline land allocation

base<-readRDS('query_data/Baseline_land-allocation_db_ssp2_4p5_gepic_gfdl-esm2m.rds')
base %>% tidyr::separate(landleaf,into=c("name","crop","landregion","mgmt","water"),sep=",")%>%
  tidyr::separate(name,into=c("crop2","GLU_name","irr","mgmt"),sep="_") ->base #%>%


base %>% left_join(comb,by="GLU_name")->baseland

baseland %>% filter(year==2020) %>% group_by(GCAM_basin_ID,GCAM_region_ID) %>%
  summarise(value=sum(value,na.rm=T))->all


baseland %>% filter(year==2020) %>%group_by(GCAM_basin_ID,GCAM_region_ID) %>%
  mutate(Protected=ifelse(grepl("Protected",crop),TRUE,FALSE)) %>%
  filter(Protected==TRUE) %>%
  summarize(value=sum(value,na.rm=T)) -> p

p %>% left_join(all,by=c("GCAM_basin_ID","GCAM_region_ID")) %>%
  mutate(fracProtected=value.x/value.y)->frac

library(gcammaptools)
library(gridExtra)
library(rgdal)
library(viridis)
library(ggplot2)

##################################################
# read in basin shapefile (base)

shp<-readOGR('mapping/shp','reg_glu_boundaries_moirai_combined_3p1_0p5arcmin')
shp_df<-fortify(shp)

as.numeric(shp_df$id)->shp_df$id

shp_df %>% mutate(id=id+1)->shp_df

xl<-readxl::read_excel('mapping/glu_reg_TableToExcel.xls')

xl %>% mutate(id=FID+1)->xl

shp_df %>% left_join(xl,by="id") %>% rename(GCAM_basin_ID=glu_id,GCAM_region_ID=reg_id)%>%
  left_join(frac,by=c("GCAM_region_ID","GCAM_basin_ID"))->shp_j


P1<-ggplot()+ theme_bw()+theme(axis.title= element_text(size=14),axis.text = element_text(size=12))+ylab('Latitude')+xlab('Longitude')+
  geom_polygon(data=shp_j,aes(x=long,y=lat,group=group,fill=fracProtected))+scale_fill_viridis(limits=c(0,1))

P1$labels$fill<-"Fraction Protected Land"

### master

master<-readRDS('query_data/LandCons_land-allocation_db_ssp2_4p5_gepic_gfdl-esm2m.rds')

master %>% tidyr::separate(landleaf,into=c("name","crop","landregion","mgmt","water"),sep=",")%>%
  tidyr::separate(name,into=c("crop2","GLU_name","irr","mgmt"),sep="_") ->master #%>%


master %>% left_join(comb,by="GLU_name")->masterland

masterland %>% filter(year==2020) %>% group_by(GCAM_basin_ID,GCAM_region_ID) %>%
  summarise(value=sum(value,na.rm=T))->all1


masterland %>% filter(year==2020) %>%group_by(GCAM_basin_ID,GCAM_region_ID) %>%
  mutate(Protected=ifelse(grepl("Protected",crop),TRUE,FALSE)) %>%
  filter(Protected==TRUE) %>%
  summarize(value=sum(value,na.rm=T)) -> p1

p1 %>% left_join(all1,by=c("GCAM_basin_ID","GCAM_region_ID")) %>%
  mutate(fracProtected=value.x/value.y)->frac1

shp_df %>% left_join(xl,by="id") %>% rename(GCAM_basin_ID=glu_id,GCAM_region_ID=reg_id)%>%
  left_join(frac1,by=c("GCAM_region_ID","GCAM_basin_ID"))->shp_j1

P2<-ggplot()+ theme_bw()+theme(axis.title= element_text(size=14),axis.text = element_text(size=12))+ylab('Latitude')+xlab('Longitude')+
  geom_polygon(data=shp_j1,aes(x=long,y=lat,group=group,fill=fracProtected))+scale_fill_viridis(limits=c(0,1))

P2$labels$fill<-"Fraction Protected Land"

##########################

year=seq(2005,2100,5)

# mandate

bio<-c(0.0,	0.0,	0.0,	31,	64,	80,	98,	106,	116,	122,	130,	138,	146,	154,	162,	170,	178,	186,	194,	202)


# queried from the GCAM model interface (purpose grown, residue, municipal solid waste)

pg<-c(0.0,	2,4,5.43,	21.46,	35.56,	50.98,	55.50,	61.73,	65.56,	71.01,	77.33,	83.95,	90.75,	97.75,	104.88,	112.32,	119.88,	127.44,	135.35)
res<-c(15.3,18.8,22,20.5,37.8,39.3,41.4,44.5,47.8,49.6,51.8,53.1,54.1,54.9,55.6,56,56.2,56.3,56.4,56.2)
msw<-c(3,3.4,3.8,4.1,4.7,5.1,5.6,6,6.4,6.8,7.1,7.5,7.9,8.3,8.7,9,9.4,8.8,10.1,10.5)

cbind(year,bio,pg,res,msw)->biomass
biomass=as.data.frame(biomass)

biomass %>% tidyr::gather(Source,value,bio,pg,res,msw)->bio1
bio1$Policy="Second Generation"

# queried from the GCAM model interface (purpose grown, residue, municipal solid waste)

pg<-c(0.0,2,4,5.43,	20.26,33.49,47.56,50.56,54.41,57.06,60.8,66,71.3,77.3,83.3,88.94,94.9,101.2,107.58,114.4)
res<-c(15.37,18.82,22.03,20.48,36.98,38.1,40.12,42.75,46.2,47.47,50.34,52.26,54,55.1,56,56.85,57.35,57.7,58,57.95)
msw<-c(2.97,	3.40,	3.8,	4.1,	4.68,	5.15,	5.58,	6,	6.4,	6.78,	7.15,	7.53,	7.9,	8.3,	8.7,	9,	9.4,	9.76,	10.1,	10.5)


cbind(year,bio,pg,res,msw)->biomass2
biomass2=as.data.frame(biomass2)

biomass2 %>% tidyr::gather(Source,value,bio,pg,res,msw)->bio2
bio2$Policy="All"

rbind(bio1,bio2)->all


P3<-ggplot(all,aes(year,value,col=Source,linetype=Policy))+geom_line(size=1)+
  scale_color_viridis(discrete=TRUE,labels=c("Mandate","MSW","Purpose Grown","Residue"))+
  theme_bw()+ylab("Biomass Production (EJ)")+theme(axis.title= element_text(size=12),axis.text = element_text(size=12))


plot_grid(P1,P2,P3,labels="auto",ncol=1)
ggsave("methods.png",dpi=300)

