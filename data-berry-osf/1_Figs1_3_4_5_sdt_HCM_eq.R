# Chris Berry 2023
# HCMs and FC accuracy in UVSD and DPSD
# How do expected values and accuracy of 1-1 2AFC trials vary with d/d' and C1?


rm(list=ls())

library(tidyverse)
library(truncnorm)

# Create parameter values for UVSD
set.seed(1) # for consistency in c2
uvsd_pars <- 
  expand_grid(c1=seq(-2,2,0.1),
              sigo=seq(1,2.25,0.25),
              d=seq(0,2.5,0.5)) %>% 
  mutate(c2 = c1 + runif(n(),0.001,3),
         cdiff = c2 - c1)

# Create parameter values for DPSD
set.seed(1) # so c2 is same
dpsd_pars <-
  expand_grid(c1=seq(-2,1,0.1),
              Ro=seq(0,1,0.2),
              dpri=seq(0,2.5,0.5)) %>% 
  mutate(c2 = c1 + runif(n(),0.001,3),
         cdiff = c2 - c1)


## FC -------------------------------------------------------------------------

# function to derive prob FC trial using integration
intFCpcorr_evsd <- function(f,a,b,dpri){
  FCacc <- 
    dnorm(f,dpri,1) / (pnorm(b,dpri,1) - pnorm(a,dpri,1)) *
    (pnorm(f,0,1) - pnorm(a,0,1)) / (pnorm(b,0,1) - pnorm(a,0,1))
  return(FCacc)
}

# function to derive prob FC trial using integration
intFCpcorr_uvsd <- function(f,a,b,Mo,SDo){
  FCacc <- 
    dnorm(f,Mo,SDo) / (pnorm(b,Mo,SDo) - pnorm(a,Mo,SDo)) *
    (pnorm(f,0,1) - pnorm(a,0,1)) / (pnorm(b,0,1) - pnorm(a,0,1))
  return(FCacc)
}

FCacc_uvsd <- function(lower,upper,Mo,SDo,a,b){
  integrate(intFCpcorr_uvsd,lower=lower,
            upper=upper,Mo=Mo,SDo=SDo,a=a,b=b)$value}

v.FCacc_uvsd <- Vectorize(FCacc_uvsd)


FCacc_evsd <- function(lower,upper,dpri,a,b){
  integrate(intFCpcorr_evsd,lower=lower,
            upper=upper,dpri=dpri,a=a,b=b)$value}

v.FCacc_evsd <- Vectorize(FCacc_evsd)


eStrengthJn <- function(Mo,SDo,b){
  eS <- Mo - SDo*((dnorm((b-Mo)/SDo))/(pnorm((b-Mo)/SDo)))
  # returns same thing as etruncnorm(a=-Inf,b=b,mean=Mo,sd=SDo)
  return(eS)
}




# eStrength(HCM minus HCCR)
uvsd_pars %>% 
  mutate(eStrengthMiss = eStrengthJn(Mo=d,SDo=sigo,b=c1),
         eStrengthCR = eStrengthJn(Mo=0,SDo=1,b=c1),
         eStrengthDiffHminF = eStrengthMiss - eStrengthCR,
         d = factor(d)) %>% 
  ggplot(aes(x=c1,y=eStrengthDiffHminF,group=d))+
  geom_hline(yintercept=0,lty=2,colour="darkgray")+
  #geom_line(aes(linetype=d),linewidth=0.5)+
  geom_line(aes(color=d),linewidth=0.5)+
  scale_colour_manual(values=c("#1B4F72","#2874A6","#2E86C1","#3498DB","#85C1E9","#AED6F1"))+
  facet_wrap(~sigo,labeller=label_bquote(sigma[o]==.(sprintf("%0.2f", round(sigo, digits = 2)))),scales="free")+
  ylab("Expected strength (HCM - HCCR)")+
  #ggtitle("Figure X. UVSD model: Expected Strength (HCM - HCCR) as a \n function of criterion and d")+
  theme_bw()+
  xlab(expression(italic(C)[1]))+
  ylim(-1,1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=11),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line = element_line(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        #legend.position = c(0.5,0.5),
        #legend.spacing.y = unit(0.2,'cm'),
        legend.position = "none",
        panel.spacing.y = unit(1.5, "lines"),
        panel.spacing.x = unit(1, "lines")) 

ggsave("1_Figure_1_eDiffMissMinCR_UVSD.png",width=5.5,height=5,dpi=320)  



# eStrength(HCM minus HCCR)
dpsd_pars %>% 
  mutate(eStrengthMiss = eStrengthJn(Mo=dpri,SDo=1,b=c1),
         eStrengthCR = eStrengthJn(Mo=0,SDo=1,b=c1),
         eStrengthDiffHminF = eStrengthMiss - eStrengthCR,
         dpri = factor(dpri)) %>% 
  ggplot(aes(x=c1,y=eStrengthDiffHminF,group=dpri))+
  geom_hline(yintercept=0,lty=2,colour="darkgrey")+  
  geom_line(aes(colour=dpri),linewidth=0.5)+
  scale_colour_manual(values=c("#0E6251","#148F77","#17A589","#1ABC9C","#48C9B0","#A3E4D7"))+
  facet_wrap(~Ro,labeller=label_bquote(R[o]==.(sprintf("%0.2f", round(Ro, digits = 2)))),scales="free")+
  ylab("Expected strength (HCM - HCCR)")+
  #ggtitle("Figure X. DPSD model: Expected Strength (HCM - HCCR) as a \n function of criterion and dpri")+
  theme_bw()+
  ylim(-1,1)+
  xlab(expression(italic(C)[1]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.title = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=11),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line = element_line(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.position = "none",
        panel.spacing.y = unit(2, "lines"))

ggsave("1_Figure_3_eDiffMissMinCR_DPSD.png",width=5.5,height=5,dpi=320)  



## UVSD - FC accuracy
# 1-1
uvsd_pars %>% 
  mutate(pC2AFC = v.FCacc_uvsd(lower=-Inf,upper=c1,Mo=d,SDo=sigo,a=-Inf,b=c1)*100,
         d = factor(d)) %>% 
  ggplot(aes(x=c1,y=pC2AFC,group=d))+
  geom_hline(yintercept=50,lty=2,colour="darkgray")+
  geom_line(aes(color=d),linewidth=0.5)+
  scale_colour_manual(values=c("#1B4F72","#2874A6","#2E86C1","#3498DB","#85C1E9","#AED6F1"))+
  facet_wrap(~sigo,labeller=label_bquote(sigma[o]==.(sprintf("%0.2f", round(sigo, digits = 2)))),scales="free")+
  ylab("Percent correct")+
  #ggtitle("Figure X. UVSD model: 1-1 FC Accuracy as a function of \n criterion and d")+
  theme_bw()+
  xlab(expression(italic(C)[1]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = seq(0,100,25),limits = c(0,100))+
  theme(legend.title = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=11),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line = element_line(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.position = "none",
        panel.spacing.y = unit(1.5, "lines"))

ggsave("1_Figure_4_FCacc_1-1_UVSD.png",width=5.5,height=5,dpi=320) 



## DPSD - FC accuracy-----------------------------------------------------------
# 1-1
dpsd_pars %>% 
  mutate(pC2AFC = v.FCacc_evsd(lower=-Inf,upper=c1,dpri=dpri,a=-Inf,b=c1)*100,
         dpri = factor(dpri)) %>% 
  ggplot(aes(x=c1,y=pC2AFC,group=dpri))+
  geom_hline(yintercept=50,lty=2,colour="darkgrey")+
  geom_line(aes(colour=dpri),linewidth=0.5)+
  scale_colour_manual(values=c("#0E6251","#148F77","#17A589","#1ABC9C","#48C9B0","#A3E4D7"))+
  facet_wrap(~Ro,labeller=label_bquote(R[o]==.(sprintf("%0.2f", round(Ro, digits = 2)))),scales="free")+
  ylab("Percent correct")+
  #ggtitle("Figure X. DPSD model: 1-1 FC Accuracy as a function of \n criterion and dpri")+
  theme_bw()+
  xlab(expression(italic(C)[1]))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = seq(0,100,25),limits = c(0,100)) + 
  guides(linetype=guide_legend("d'"))+
  theme(legend.title = element_text(face = "italic"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size=11),
        panel.border = element_rect(fill = NA, colour = "white"),
        axis.line = element_line(),
        axis.text = element_text(size=11),
        axis.title = element_text(size=12),
        legend.position = "none",
        panel.spacing.y = unit(1.5, "lines"))

ggsave("1_Figure_5_FCacc_1-1_DPSD.png",width=5.5,height=5,dpi=320)  








