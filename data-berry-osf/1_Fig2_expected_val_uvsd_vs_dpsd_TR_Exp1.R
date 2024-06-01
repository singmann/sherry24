# Chris Berry 2023
# SDT plot for UVSD and DPSD
# With Expected values for HCM and HCCR
# 2 x 2 multipanel plot

rm(list=ls())

library(tidyverse)
library(truncnorm)
library(gridExtra)

# get model fits (need to have fit the models first in script 3_)
UVSD <- read_csv(paste0("3_uvsd_results_T&R_2017_e1_agg.csv"))
DPSD <- read_csv(paste0("3_dpsd_results_T&R_2017_e1_agg.csv"))

eUVSD <- 
  tibble(mun    = 0,
         muo    = mean(UVSD$muo,na.rm=T),
         sigman = 1,
         sigmao = mean(UVSD$sigo,na.rm=T),
         c1 = mean(UVSD$c1,na.rm=T),
         c2 = mean(UVSD$c2,na.rm=T),
         c3 = mean(UVSD$c3,na.rm=T),
         c4 = mean(UVSD$c4,na.rm=T),
         c5 = mean(UVSD$c5,na.rm=T),
         # expected vals
         eN1 = etruncnorm(a = -Inf, b = c1, mean = mun, sd = sigman),
         eO1 = etruncnorm(a = -Inf, b = c1, mean = muo, sd = sigmao),
         eN2 = etruncnorm(a = c1, b = c2, mean = mun, sd = sigman),
         eO2 = etruncnorm(a = c1, b = c2, mean = muo, sd = sigmao),
         eN3 = etruncnorm(a = c2, b = c3, mean = mun, sd = sigman),
         eO3 = etruncnorm(a = c2, b = c3, mean = muo, sd = sigmao),
         eN4 = etruncnorm(a = c3, b = c4, mean = mun, sd = sigman),
         eO4 = etruncnorm(a = c3, b = c4, mean = muo, sd = sigmao),
         eN5 = etruncnorm(a = c4, b = c5, mean = mun, sd = sigman),
         eO5 = etruncnorm(a = c4, b = c5, mean = muo, sd = sigmao),
         eN6 = etruncnorm(a = c5, b = Inf, mean = mun, sd = sigman),
         eO6 = etruncnorm(a = c5, b = Inf, mean = muo, sd = sigmao),
         # diff in expected vals
         d1 = eO1 - eN1,
         d2 = eO2 - eN2,
         d3 = eO3 - eN3,
         d4 = eO4 - eN4,
         d5 = eO5 - eN5,
         d6 = eO6 - eN6)

# for plotting
fUVSD <- 
  tibble(x = seq(-4.5,7,0.1),
         new = dnorm(x,mean = eUVSD$mun, sd = eUVSD$sigman),
         old = dnorm(x,mean = eUVSD$muo, sd = eUVSD$sigmao)) 



# SDT representation
pUVSD <- 
  fUVSD %>% 
  pivot_longer(cols=c(new,old),values_to="Density",names_to="Stimulus") %>% 
  ggplot(aes(x=x,y=Density,group=Stimulus)) +
  geom_line(aes(linetype=Stimulus),linewidth=0.5) +
  scale_linetype_manual(values=c("solid", "dashed"))+
  geom_ribbon(aes(x=x,ymax=Density,group=Stimulus),ymin=0,alpha=0.3,fill=c("#21618C")) +
  #scale_fill_manual(name='', values=c("orange","lightgrey")) +
  theme_classic() +
  xlab("Strength") +
  ylab("Probability Density") +
  ylim(0,0.5) +
  geom_segment(aes(x = eUVSD$c1, y = 0, xend = eUVSD$c1, yend = 0.42))+
  geom_segment(aes(x = eUVSD$c2, y = 0, xend = eUVSD$c2, yend = 0.42))+
  geom_segment(aes(x = eUVSD$c3, y = 0, xend = eUVSD$c3, yend = 0.42))+
  geom_segment(aes(x = eUVSD$c4, y = 0, xend = eUVSD$c4, yend = 0.42))+
  geom_segment(aes(x = eUVSD$c5, y = 0, xend = eUVSD$c5, yend = 0.42))+
  #geom_text(x = eUVSD$c1, y = max(fUVSD$new) + 0.01, label = "c[1]",parse=TRUE) +
  #geom_text(x = eUVSD$c2, y = max(fUVSD$new) + 0.01, label = "c[2]",parse=TRUE) +
  #geom_text(x = eUVSD$c3, y = max(fUVSD$new) + 0.01, label = "c[3]",parse=TRUE) +
  #geom_text(x = eUVSD$c4, y = max(fUVSD$new) + 0.01, label = "c[4]",parse=TRUE) +
  #geom_text(x = eUVSD$c5, y = max(fUVSD$new) + 0.01, label = "c[5]",parse=TRUE) +
  geom_text(x = eUVSD$c1 - 0.3, y = 0.43, label = "'1'",size = 3) +
  geom_text(x = eUVSD$c2-(eUVSD$c2 - eUVSD$c1)/2, y = 0.43, label = "'2'",size = 3) +
  geom_text(x = eUVSD$c3-(eUVSD$c3 - eUVSD$c2)/2, y = 0.43, label = "'3'",size = 3) +
  geom_text(x = eUVSD$c4-(eUVSD$c4 - eUVSD$c3)/2, y = 0.43, label = "'4'",size = 3) +
  geom_text(x = eUVSD$c5-(eUVSD$c5 - eUVSD$c4)/2, y = 0.43, label = "'5'",size = 3) +
  geom_text(x = eUVSD$c5 + 0.3, y = 0.43, label = "'6'",size = 3) +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.position = c(0.8, 0.8),
        legend.title=element_blank()) +
  annotate("text", x = 1, y = 0.5, label = 'bold("UVSD")',size=4,parse=TRUE)


pUVSD

arrows <- 
  tibble(
    x1 = c(-1.3,-1.3),
    x2 = c(-2.5,-3.5),
    y1 = c(0.11,0.01), 
    y2 = c(0.16,0.08)
  )


pUVSD_curve <-
  pUVSD + 
  geom_curve(
    data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
    arrow = arrow(length = unit(0.08, "inch")), linewidth = 1,
    color = "darkslategray", curvature = 0,inherit.aes=F) +
  annotate("text", x = -3.2, y = 0.18, label = "HCCRs",size=3) +
  annotate("text", x = -4.2, y = 0.1, label = "HCMs",size=3)
  

pUVSD_curve


df <- 
  tibble(resp=c("HCM","HCCR"),expec=c(eUVSD$eO1,eUVSD$eN1)) %>% 
  mutate(resp=factor(resp,levels=c("HCM","HCCR"))) 

line_UVSD <- 
  df %>% 
    ggplot(aes(x=resp,y=expec))+
    geom_line(group=1,color=c("#21618C"))+
    geom_point(size=3,color=c("#1B4F72"))+
    ylab("Expected Strength") +
    xlab("")+
    ylim(c(-1.6,-1.40))+
    theme_classic()+
    theme(axis.text = element_text(size=11),
          axis.title = element_text(size=12))

  


# DPSD - fam proc
eDPSD <- 
  tibble(mun    = 0,
         muo    = mean(DPSD$dpri,na.rm=T),
         sigman = 1,
         sigmao = 1,
         c1 = mean(DPSD$c1,na.rm=T),
         c2 = mean(DPSD$c2,na.rm=T),
         c3 = mean(DPSD$c3,na.rm=T),
         c4 = mean(DPSD$c4,na.rm=T),
         c5 = mean(DPSD$c5,na.rm=T),
         # expected vals
         eN1 = etruncnorm(a = -Inf, b = c1, mean = mun, sd = sigman),
         eO1 = etruncnorm(a = -Inf, b = c1, mean = muo, sd = sigmao),
         eN2 = etruncnorm(a = c1, b = c2, mean = mun, sd = sigman),
         eO2 = etruncnorm(a = c1, b = c2, mean = muo, sd = sigmao),
         eN3 = etruncnorm(a = c2, b = c3, mean = mun, sd = sigman),
         eO3 = etruncnorm(a = c2, b = c3, mean = muo, sd = sigmao),
         eN4 = etruncnorm(a = c3, b = c4, mean = mun, sd = sigman),
         eO4 = etruncnorm(a = c3, b = c4, mean = muo, sd = sigmao),
         eN5 = etruncnorm(a = c4, b = c5, mean = mun, sd = sigman),
         eO5 = etruncnorm(a = c4, b = c5, mean = muo, sd = sigmao),
         eN6 = etruncnorm(a = c5, b = Inf, mean = mun, sd = sigman),
         eO6 = etruncnorm(a = c5, b = Inf, mean = muo, sd = sigmao),
         # diff in expected vals
         d1 = eO1 - eN1,
         d2 = eO2 - eN2,
         d3 = eO3 - eN3,
         d4 = eO4 - eN4,
         d5 = eO5 - eN5,
         d6 = eO6 - eN6)

# for plotting
fDPSD <- 
  tibble(x = seq(-4.5,7,0.1),
         new = dnorm(x,mean = eDPSD$mun, sd = eDPSD$sigman),
         old = dnorm(x,mean = eDPSD$muo, sd = eDPSD$sigmao)) 




# SDT representation
pDPSD <- 
  fDPSD %>% 
  pivot_longer(cols=c(new,old),values_to="Density",names_to="Stimulus") %>% 
  ggplot(aes(x=x,y=Density,group=Stimulus)) +
  geom_line(aes(linetype=Stimulus),linewidth=0.5) +
  scale_linetype_manual(values=c("solid", "dashed"))+
  geom_ribbon(aes(x=x,ymax=Density),ymin=0,alpha=0.3,fill=c("#17A589")) +
  #scale_fill_manual(name='', values=c("green4","red")) +
  theme_classic() +
  xlab("Familiarity") +
  ylab("Probability Density") +
  ylim(0,0.5) +
  geom_segment(aes(x = eDPSD$c1, y = 0, xend = eDPSD$c1, yend = 0.42))+
  geom_segment(aes(x = eDPSD$c2, y = 0, xend = eDPSD$c2, yend = 0.42))+
  geom_segment(aes(x = eDPSD$c3, y = 0, xend = eDPSD$c3, yend = 0.42))+
  geom_segment(aes(x = eDPSD$c4, y = 0, xend = eDPSD$c4, yend = 0.42))+
  geom_segment(aes(x = eDPSD$c5, y = 0, xend = eDPSD$c5, yend = 0.42))+
  #geom_text(x = eDPSD$c1, y = max(fDPSD$new) + 0.01, label = "c[1]",parse=TRUE) +
  #geom_text(x = eDPSD$c2, y = max(fDPSD$new) + 0.01, label = "c[2]",parse=TRUE) +
  #geom_text(x = eDPSD$c3, y = max(fDPSD$new) + 0.01, label = "c[3]",parse=TRUE) +
  #geom_text(x = eDPSD$c4, y = max(fDPSD$new) + 0.01, label = "c[4]",parse=TRUE) +
  #geom_text(x = eDPSD$c5, y = max(fDPSD$new) + 0.01, label = "c[5]",parse=TRUE) +
  geom_text(x = eDPSD$c1 - 0.3, y = 0.43, label = "'1'",size = 3) +
  geom_text(x = eDPSD$c2-(eDPSD$c2 - eDPSD$c1)/2, y = 0.43, label = "'2'",size = 3) +
  geom_text(x = eDPSD$c3-(eDPSD$c3 - eDPSD$c2)/2, y = 0.43, label = "'3'",size = 3) +
  geom_text(x = eDPSD$c4-(eDPSD$c4 - eDPSD$c3)/2, y = 0.43, label = "'4'",size = 3) +
  geom_text(x = eDPSD$c5-(eDPSD$c5 - eDPSD$c4)/2, y = 0.43, label = "'5'",size = 3) +
  geom_text(x = eDPSD$c5 + 0.3, y = 0.43, label = "'6'",size = 3) +
  theme(axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.position = 'none',
        legend.title=element_blank()) +
  annotate("text", x = 1, y = 0.5, label ='bold("DPSD")', size=4,parse=T)



pDPSD


df2 <- 
  tibble(resp=c("HCM","HCCR"),expec=c(eDPSD$eO1,eDPSD$eN1)) %>% 
  mutate(resp=factor(resp,levels=c("HCM","HCCR"))) 

line_DPSD <- 
  df2 %>% 
    ggplot(aes(x=resp,y=expec))+
    geom_line(group=1,color="#148F77")+
    geom_point(size=3,color=c("#117864"))+
    ylab("Expected Familiarity") +
    xlab("")+
    ylim(c(-1.6,-1.40))+
    theme_classic() +
    theme(axis.text = element_text(size=11),
          axis.title = element_text(size=12))

fig_pan <-
  grid.arrange(pUVSD_curve,
               pDPSD,
               line_UVSD,
               line_DPSD,
               ncol=2,nrow=2)

ggsave(paste0("1_Figure_2_UVSD_vs_DPSD_expect_TRExp1_2017.png"),fig_pan,width=7.5,height=6.5)

