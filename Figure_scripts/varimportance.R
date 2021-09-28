library(rpart)
library(rpart.plot)
library(dplyr)


##########################################
# try with anova
unique(sum$Policy)->pol
unique(sum$Metric)->met

P=c()

for (p in pol){
  for (m in met){
    
    sum %>% filter(Policy==p,Metric==m)->dat
    
    anova<-aov(diff~SSP+Socioeconomics+agLU+RCP+ESM+CropModel,data=dat)
    
    broom::tidy(anova)  %>% mutate(varExp=sumsq/sum(sumsq))->varexp
    
    varexp$Metric=m
    varexp$Constraint=p
    
    P=rbind(P,varexp)
  }
}

ggplot(P,aes(Metric,varExp,fill=term))+geom_col(stat="identity")+facet_wrap(~Policy)

P1=c()

for (m in met){
  
  sum %>% filter(Metric==m)->dat
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
  scale_fill_viridis(discrete=TRUE)+ylab("Variance Explained")+
  theme(legend.text=element_text(size=10),legend.title = element_text(size=14),axis.text=element_text(size=10),axis.title = element_text(size=16),strip.text.x = element_text(size = 10),axis.text.x=element_text(angle = 90))

ggsave("varexplained_anova.png",dpi=300)

