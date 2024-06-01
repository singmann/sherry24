# Chris Berry 2023
# Read in model fits of the experiments
# Make plots
# Expected HCMs and HCCR and FC acc derived via *simulation* or formula (2HT)
# Jan/Feb 2024: added exponential and gamma uvsd

rm(list=ls())

library(tidyverse)
library(truncnorm)
library(ggbeeswarm)

# User input options, fitted data: 
# "simulated"  = simulated model data
# "exp1a_agg"  = fit exp1a aggregate data
# "exp1a_ind"  = fit exp1a individual data
# "exp1b_agg"  = fit exp1b aggregate data
# "exp1b_ind"  = fit exp1b individual data
# "exp2_agg"   = fit exp2 aggregate data
# "exp2_ind"   = fit exp2 individual data
# "exp3_agg"   = fit exp3 aggregate data
# "exp3_ind"   = fit exp3 individual data
# "T&R_2017_e1_agg" = fit aggregate data from T&R Exp 2
# "T&R_2017_e2_agg" = fit aggregate data from T&R Exp 2
#
dofits <- c( "simulated",
             "exp1a_agg","exp1a_ind",
             "exp1b_agg","exp1b_ind",
             "exp2_agg","exp2_ind",
             "exp3_agg","exp3_ind",
             "T&R_2017_e1_agg","T&R_2017_e2_agg")
#dofits <- c("exp1a_ind")

# User input options, model type: 
# "evsd" = fitted EVSD model to 6-point YN
# "uvsd" = fitted UVSD model to 6-point YN
# "dpsd" = fitted DPSD model to 6-point YN
# "2ht"  = fitted 2ht model to 6-point YN
# "msd"  = fitted msd model to 6-point YN
# "gumbel" = fitted gumbel model to 6-point YN
# "logistic" = fit logistic-uvsd to 6-point YN 
# "weibull" = fit weibull
# "lognorm" = fit log-normal
# "expo" = exponential
domodels <- c( "evsd", "uvsd", "dpsd",
               "2ht", "msd", "gumbel",
               "logistic", "weibull", "lognorm","expo","gamma")
#domodels <- c("gamma")


cols <- tibble(linecol="black",pointcol="black",datcol="darkgrey")


for(fits in dofits){
  
  for(model in domodels){
    
    # remove 3 most extreme c5 ppts if dpsd
    # spec nonfittable
    if(model == "dpsd"){
      exp1a_ind_nonfittable <- c(4,7) # most extreme c5 ppts in dpsd
      exp1b_ind_nonfittable <- c(49) # most extreme c5 ppts in dpsd
      exp2_ind_nonfittable <- c(43,68) 
      exp3_ind_nonfittable <- c(24) 
    }else {
      exp1a_ind_nonfittable <- c()
      exp1b_ind_nonfittable <- c()
      exp2_ind_nonfittable <- c(43,68) 
      exp3_ind_nonfittable <- c(24) }
    
    cols <- 
      cols %>% 
      mutate(linecol= case_when(model=="evsd" ~ "grey",
                                model=="uvsd" ~ "#21618C",
                                model=="dpsd" ~ "#148F77",
                                model=="2ht" ~ "#6C3483",
                                model=="msd" ~ "#B03A2E" ,
                                model=="gumbel" ~ "#21618C",
                                model=="logistic" ~ "#21618C",
                                model=="weibull" ~ "#21618C",
                                model=="lognorm" ~ "#21618C",
                                model=="expo" ~ "#21618C",
                                model=="gamma"~ "#21618C")) %>% 
      mutate(pointcol=case_when(model=="evsd" ~ "darkgrey",
                                model=="uvsd" ~ "#1B4F72",
                                model=="dpsd" ~ "#117864",
                                model=="2ht" ~ "#512E5F",
                                model=="msd" ~ "#78281F",
                                model=="gumbel" ~ "#1B4F72",
                                model=="logistic" ~ "#1B4F72",
                                model=="weibull" ~ "#1B4F72",
                                model=="lognorm" ~ "#1B4F72",
                                model=="expo" ~ "#1B4F72",
                                model=="gamma"~ "#21618C")) %>% 
      mutate(datcol=  case_when(model=="evsd" ~ "lightgrey",
                                model=="uvsd" ~ "#AED6F1",
                                model=="dpsd" ~ "#A3E4D7",
                                model=="2ht" ~ "#D2B4DE",
                                model=="msd" ~ "#F5B7B1",
                                model=="gumbel" ~ "#AED6F1",
                                model=="logistic" ~ "#AED6F1",
                                model=="weibull" ~ "#AED6F1",
                                model=="lognorm" ~ "#AED6F1",
                                model=="expo" ~ "#AED6F1",
                                model=="gamma" ~ "#AED6F1"))
    
    # Figure 1 title
    if(fits=="exp1a_ind"){fig1_title <- paste0(toupper(model)," (Exp. 1a)")}else
      if(fits=="exp1b_ind"){fig1_title <- paste0(toupper(model)," (Exp. 1b)")}else
        if(fits=="exp2_ind"){fig1_title <- paste0(toupper(model)," (Exp. 2)")}else
        {fig1_title <- paste0(toupper(model), " ", fits)}
    
    
    # get model fits
    dat <- read_csv(paste0("3_",model,"_results_",fits,".csv"),show_col_types=F)
  
    # get IDs of any nonfittable
    if(str_detect(fits,"ind")==TRUE){ 
      nonfittable <- get(paste0(fits,"_nonfittable")) 
    }else
    {nonfittable <- c()}
    
    # remove non-fittable
    res <- dat %>%  filter(!(ppt %in% nonfittable))
    
    
    panel_lab <- c("A","B")
  
    # Exp 1a, 1b, 2 - eFC accuracy
    if(!(fits %in% c("exp3_agg","exp3_ind","simulated"))==TRUE){
      
      if(model!="2ht"){
        plot_acc <- 
          res %>% 
          select(ppt,sim_eFCpcor1,sim_eFCpcor2,sim_eFCpcor3,
                 sim_eFCpcor4,sim_eFCpcor5,sim_eFCpcor6) 
          }else
        if(model=="2ht"){ # expected values not via simulation
          plot_acc <- 
            res %>% 
            select(ppt,eFCpcor1,eFCpcor2,eFCpcor3,
                   eFCpcor4,eFCpcor5,eFCpcor6)
          }
      

      
      p <- 
        plot_acc %>% 
        pivot_longer(cols=!ppt,
                     names_prefix="eFCpcor",
                     names_to=c('rating16'),
                     values_to="pCorrect") %>% 
        mutate(pCorrect = pCorrect * 100) %>% 
        ggplot() + 
        geom_quasirandom(aes(x=rating16,y=pCorrect,group=ppt,alpha=0.1),
                         dodge.width = 0.4, varwidth = TRUE,alpha=0.1,
                         groupOnX=TRUE,colour=cols$datcol,na.rm=T)+
        geom_hline(yintercept = 50,lty=2,colour="black")+
        scale_x_discrete(labels = c("1-1","2-2","3-3","4-4","5-5","6-6"))+
        scale_y_continuous(breaks = seq(0,100,25),limits = c(0,100))+
        xlab("2AFC condition") +
        ylab("Percent Correct")+
        theme_classic() +
        theme(legend.position="none") +
        stat_summary(aes(x=rating16,y=pCorrect,group=1), fun.data = "mean_cl_boot",colour=cols$linecol,linewidth=0.5,geom="line",na.rm=T) +
        ggtitle(fig1_title) +
        theme(axis.line = element_line(),
              axis.text.y = element_text(size=13),
              axis.text.x = element_text(size=12),
              axis.title = element_text(size=14),
              legend.position = "none",
              panel.spacing.y = unit(1.5, "lines"),
              plot.title = element_text(hjust = 0.5,face="bold"),
              plot.margin = unit(c(t=0,r=1.5,b=1.75,l=0), "lines"))
      
      if(str_detect(fits,"ind")==TRUE){ 
        # add points and CIs
        p <- p + stat_summary(aes(x=rating16,y=pCorrect,group=1), fun.data = "mean_cl_boot",colour=cols$pointcol,fill="white",size=0.65,na.rm=T)
      }else
        if(str_detect(fits,"agg")==TRUE){ 
          # add points
          p <- p + geom_point(aes(x=rating16,y=pCorrect,group=1),colour=cols$pointcol,fill="white",size=2.5,na.rm=T)
          }
        
      print(p)
      
      
      if(str_detect(fits,"ind")==TRUE & model!="2ht"){ 
        cat(paste0("fit = ", fits, "  model = ",model,"\n"))
        print(t.test(res$sim_eFCpcor1,mu=.5)) 
        }         
      
      ggsave(paste0("5_",fits,"_Figure_FCacc_x_rating","_Model_",model,".png"),p,width=2.5,height=3.21)
      
      # save for use in multipanel plot later on
      save(p, file = paste0("5_",fits,"_Figure_FCacc_x_rating","_Model_",model,".rdata"))
      
    }else
      # Exp 3 - eHCM and eHCCR
      if((str_detect(fits,"exp3")==TRUE & model!="2ht")){
        res %>% filter(O1>0) %>% count() # check N=22
        
        # Expected difference in strength for HCM and HCCR
        p_diff <- 
          res %>% 
          filter(O1>0) %>% # remove with no HCMs for figure (22 ppts)
          select(ppt,
                 eStrengthO1,
                 eStrengthN1) %>% 
          mutate(d_eStrength=eStrengthO1 - eStrengthN1) %>% 
          select(ppt, d_eStrength) %>% 
          ggplot() + 
          geom_quasirandom(aes(x=1,y=d_eStrength),
                           alpha=0.2,width=0.1,
                           colour=cols$datcol,na.rm=T) +
          ylim(c(-0.6,0.6))+  
          xlab(toupper(model)) +
          ylab("Expected Strength \n (HCM - HCCR)")+
          theme_classic() +
          theme(axis.text.x = element_blank())+
          theme(axis.ticks.x = element_blank())+
          theme(legend.position="none") +
          geom_hline(yintercept=0,lty=2)+
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank(),
                plot.margin = unit(c(t=0,r=1.5,b=0,l=0), "lines"))
        
        if(str_detect(fits,"ind")==TRUE){ 
          # points and CI
          p_diff <- p_diff + stat_summary(aes(x=1,y=d_eStrength,group=1), fun.data = "mean_cl_boot",colour=cols$pointcol,fill="white",size=0.35,na.rm=T)
        }else 
          if(str_detect(fits,"agg")==TRUE){
            # points
            p_diff <-  p_diff + geom_point(aes(x=1,y=d_eStrength,group=1), colour=cols$pointcol,fill="white",size=2.5)
            }
          
        print(p_diff)
        
        ggsave(paste0("5_",fits,"_Figure_eDiffStrength_stimulus","_Model_",model,".png"),
               p_diff,width=1.55,height=3.21)
        
        # save for use in multipanel plot later on
        save(p_diff, file = paste0("5_",fits,"_Figure_eDiffStrength_stimulus","_Model_",model,".rdata"))
        
        if(str_detect(fits,"ind")==TRUE){ 
          cat(paste0("fit = ", fits, "  model = ",model,"\n"))
          print(t.test(res$eStrengthO1,res$eStrengthN1,paired=TRUE)) 
          }
        
      }
    
    
    
    # Look at differences in expected values---------------------------------------
    if(model!="2ht" & (str_detect(fits,"exp3")==FALSE)){
      plot_EV <- 
        res %>% 
        select(ppt,
               d_eStrength1,d_eStrength2,d_eStrength3,
               d_eStrength4,d_eStrength5,d_eStrength6) 
      
      pEV <- 
        plot_EV %>% 
        pivot_longer(cols=!ppt,
                     names_prefix="d_e",
                     names_to=c('rating16'),
                     values_to="dExVal") %>% 
        ggplot() + 
        geom_quasirandom(aes(x=rating16,y=dExVal,group=ppt,alpha=0.1),
                         dodge.width = 0.4, varwidth = TRUE,alpha=0.1,
                         groupOnX=TRUE,colour=cols$datcol,na.rm=T)+
        scale_x_discrete(labels = c("1-1","2-2","3-3","4-4","5-5","6-6"))+
        xlab("2AFC condition") +
        ylab("Difference in Expected \n Value (old - new)")+
        theme_classic() +
        theme(legend.position="none") +
        stat_summary(aes(x=rating16,y=dExVal,group=1), fun.data = "mean_cl_boot",colour=cols$linecol,linewidth=0.5,geom="line",na.rm=T) +
        ggtitle(paste0("Figure X. ", fits ,"\n", " Model = ",model)) 
      
      
      if(str_detect(fits,"ind")==TRUE){ 
        # add points and CIs
        pEV <- pEV + stat_summary(aes(x=rating16,y=dExVal,group=1), fun.data = "mean_cl_boot",colour=cols$pointcol,fill="white",size=0.65,na.rm=T)
      }else
        if(str_detect(fits,"agg")==TRUE){ 
          # add points
          pEV <- pEV + geom_point(aes(x=rating16,y=dExVal,group=1),colour=cols$pointcol,fill="white",size=2.5,na.rm=T)
        }
      
      
      print(pEV)
      
      ggsave(paste0("5_",fits,"_Figure_diffEV_x_rating","_Model_",model,".png"),pEV,width=2.5,height=3.21)
    }  
    
  
    
    
    
    #---------------------------------------------------------------------------
    # Check the estrength(O1) < estrength(N1) prediction in UVSD  by simulation
    # use aggregated paras
    
    if(str_detect(fits,"agg") & model=="uvsd"){
      nitems<-10000
      sim_agg_uvsd_dat <- 
        tibble(
          Stim= c(rep(1,nitems),rep(0,nitems)),
          f = c(rnorm(nitems,mean=res$muo,sd=res$sigo),rnorm(nitems,mean=0,sd=1))) %>% 
          mutate(J = case_when(f<res$c1~1,
                               f>=res$c1&f<res$c2~2,
                               f>=res$c2&f<res$c3~3,
                               f>=res$c3&f<res$c4~4,
                               f>=res$c4&f<res$c5~5,
                               f>=res$c5~6))
      
      # P(O1) < P(N1)
      sim_agg_uvsd_dat %>% 
        group_by(Stim,J) %>% 
        summarise(nJ=n(),
                  pJ=nJ/nitems)
           
      # BUT e(O1) < e(N1)
      sim_agg_uvsd_dat %>% 
        group_by(Stim,J) %>% 
        summarise(mean(f))
      
      # confirm p(N1) > p(O1) - yes
      pnorm(res$c1, mean=0, sd=1) # new
      pnorm(res$c1, mean=res$muo, sd=res$sigo) # old
      
      # confirm e(O1) < e(N1) - yes
      etruncnorm(a=-Inf, b=res$c1, mean=0, sd=1) # new
      etruncnorm(a=-Inf, b=res$c1, mean=res$muo, sd=res$sigo) # old
      
      # check by equation
      eStrengthJn <- function(Mo,SDo,b){
        eS <- Mo - SDo*((dnorm((b-Mo)/SDo))/(pnorm((b-Mo)/SDo)))
        # returns same thing as etruncnorm(a=-Inf,b=b,mean=Mo,sd=SDo)
        return(eS)
      }
      
      eStrengthJn(b=res$c1, Mo=0, SDo=1) # new
      eStrengthJn(b=res$c1, Mo=res$muo, SDo=res$sigo) # old
      
      # Therefore, expected values of "1" ratings need not follow proportion of "1" 
      # That is, expected strength of O1 items can be LESS than that of N1 items, 
      # even though P(O1) < P(N1)
      
      sim_agg_uvsd_dat %>% 
        ggplot(aes(f)) +
        geom_density() +
        facet_wrap(~Stim*J)
    }

  }
}

