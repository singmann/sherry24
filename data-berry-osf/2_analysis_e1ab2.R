# Chris Berry 2023
# Exps 1a, 1b, 2
# Analysis


rm(list = ls())

library(tidyverse)
library(BayesFactor)
library(effsize)
library(lmerTest)
library(lme4)
library(emmeans)
library(DHARMa)
library(kableExtra)
library(ggbeeswarm)


# user input
to_analyse  <- c("e1a","e1b","e2")  # Input combination of "e1a", "e1b", or "e2" 
#to_analyse <- "e2"

for(doexp in to_analyse){
  
  Nexp1a <- 73 # N exp 1a
  Nexp1b <- 73 # N exp 1b
  Nexp2  <- 73 # N exp 2
  
  
  
  # load relevant variables
  expID <- c("e1a","e1b","e2")
  if(doexp=="e1a"){Ns <- Nexp1a}else
    if(doexp=="e1b"){Ns <- Nexp1b}else
      if(doexp=="e2"){Ns <- Nexp2}
  
  # Figure title
  if(doexp=="e1a"){fig1_title <- "(Exp. 1a)"}else
    if(doexp=="e1b"){fig1_title <- "(Exp. 1b)"}else
      if(doexp=="e2"){fig1_title <- "(Exp. 2)"}
  
  # Figure_FCconf title
  if(doexp=="e1a"){fig2_title <- "Exp. 1a"}else
    if(doexp=="e1b"){fig2_title <- "Exp. 1b"}else
      if(doexp=="e2"){fig2_title <- "Exp. 2"}
  
  
  # load data
  study_data <- read_csv(paste0("1_",doexp,"_study_data_","N",Ns,".csv"))
  recog_data <- read_csv(paste0("1_",doexp,"_recog_data_","N",Ns,".csv"),
                         col_types = cols(sex_kp_mfor=col_character())) %>% mutate(idx=factor(idx))
  fc_data    <- read_csv(paste0("1_",doexp,"_fc_data_","N",Ns,".csv"),
                         col_types = cols(sex_kp_mfor=col_character())) %>% mutate(idx=factor(idx))
  demog_data <- read_csv(paste0("1_",doexp,"_demog_data_","N",Ns,".csv"),
                         col_types = cols(sex_kp_mfor=col_character())) %>% mutate(idx=factor(idx))
  
  # single-item recognition: H and FA rates
  recog_hfa_summary <- 
    recog_data %>% 
    group_by(idx) %>%
    summarise(pH = sum(hmfc == "hit") / (sum(hmfc == "hit") + sum(hmfc == "miss")),
              pF = sum(hmfc == "fa") / (sum(hmfc == "fa") + sum(hmfc == "cr")),
              HminF = pH - pF,
              dpri = qnorm(pH) - qnorm(pF))
  
  recog_hfa_summary %>% as.data.frame()
  
  # show ppts at floor
  print(recog_hfa_summary %>% filter(HminF < 0.05))
  
  # user input: idx of any ppts to exclude on basis of pH-pF< 0.05
  exc_1a <- c(65) # idx 65 had a H min F rate of 0.02 and was replaced
  exc_1b <- c(17) # idx 17 had a negative H min F rate and was replaced
  exc_2  <- c(30) # idx 30 responded "old" to every item and was replaced
  
  # store to-be-excluded ids
  if(doexp=="e1a"){exclude_ppt <- exc_1a}else
    if(doexp=="e1b"){exclude_ppt <- exc_1b}else
      if(doexp=="e2"){exclude_ppt <- exc_2}
  
  
  # Exclude ppts in exclude_ppt
  demog_data <- demog_data %>% filter(!(idx %in% exclude_ppt))
  study_data <- study_data %>% filter(!(idx %in% exclude_ppt))
  recog_data <- recog_data %>% filter(!(idx %in% exclude_ppt))
  fc_data    <- fc_data %>% filter(!(idx %in% exclude_ppt))
  recog_hfa_summary <- recog_hfa_summary %>% filter(!(idx %in% exclude_ppt))
  
  N <- length(unique(demog_data$idx))
  
  
  # save final data for use in later scripts
  write_csv(study_data,paste0("2_",doexp,"_study_data_","N",N,".csv"))
  write_csv(recog_data,paste0("2_",doexp,"_recog_data_","N",N,".csv"))
  write_csv(fc_data,paste0("2_",doexp,"_fc_data_","N",N,".csv"))
  write_csv(demog_data,paste0("2_",doexp,"_demog_data_","N",N,".csv"))
                         
  
  # Check exp ID
  unique(demog_data$exp)
  unique(fc_data$exp)
  unique(recog_data$exp)
  
  
  
  # Get demographic details: n, age
  print(paste0(demog_data$exp[1], "-------------------------------------------------------"))
  print(paste0("N = ", length(unique(demog_data$idx)) ))
  print(paste0("M age = ", round(mean(demog_data$age_kp),2)))
  print(paste0("SD age = ", round(sd(demog_data$age_kp),2)))

  # Get n in each counterbalance condition (randomly assigned by program)
  demog_data %>% 
    group_by(cbcond) %>% 
    count()
  
  
  
  # study_data -------------------------------------------------------------------
  
  if(doexp=="e2"){
    # check ns
    study_data %>% 
      group_by(idx) %>% 
      count() %>% as.data.frame()
    
    M_ppt_stud_responses<- 
      study_data %>% 
      group_by(idx) %>% 
      summarise(nresp=sum(studresp=="q"|studresp=="p")/n(),
                nKPolder=sum(stud_jud=="older",na.rm=T)/n(),
                nKPyounger=sum(stud_jud=="younger",na.rm=T)/n()) %>% 
      as.data.frame()
    
    table_stud_KPs <- 
      M_ppt_stud_responses %>% 
        summarise(M_nresp=mean(nresp),
                  sd_nresp=sd(nresp),
                  M_nKPolder=mean(nKPolder),
                  sd_nKPolder=sd(nKPolder),
                  M_nKPyounger=mean(nKPyounger),
                  sd_nKPyounger=sd(nKPyounger)) %>% 
        kable(caption = paste0("Table X. ",doexp," Study responses"),digits=2) %>% 
        kable_classic_2()
    
    print(table_stud_KPs)
    
    # check for ppts with zero or one-type of response
    M_ppt_stud_responses %>% 
      summarise(sum(nresp==0))
    
    M_ppt_stud_responses %>% 
      summarise(sum(nKPolder==0))
    
    M_ppt_stud_responses %>% 
      summarise(sum(nKPyounger==0))
  }
  
  ## Tables HMFA------------------------------------------------------------------
  #
  # Use same row order as R&T2020 for table
  row_order <- c("hit","miss","fa","cr")
  
  # Create table like R&T2020 Table 1
  table_hmfc_x_conf <- 
    recog_data %>% 
      group_by(hmfc) %>% 
      summarise(n1_2 = sum(KP_conf==1 | KP_conf == 2),
                per1_2 = n1_2/n()*100,
                n3 = sum(KP_conf==3),
                per3 = n3/n()*100,
                n4 = sum(KP_conf==4),
                per4 = n4/n()*100,
                total = n()) %>% 
    slice(match(row_order,hmfc))
  
  
  # Print table using kable
  table_HCM <- 
    table_hmfc_x_conf %>% 
      mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                            hmfc=="miss"~"Miss",
                            hmfc=="fa"~"FA",
                            hmfc=="cr"~"CR")) %>% 
      rename(Response=hmfc) %>% 
      kable(caption = paste0("Table X. ",doexp," Response x rating (aggregated across ppts)", " ,N = ",N),
            digits=2,
            col.names = c("Response",
                          "n 1-2",
                          "% 1-2",
                          "n 3",
                          "% 3",
                          "n 4",
                          "% 4",
                          "total")) %>% 
      kable_classic_2()
  
  # show table with HCMs
  print(table_HCM)
  
  
  # Check equivalent table, but averaged across ppts, rather than aggregated
  table_hmfc_x_conf_ppts <- 
    recog_data %>% 
    group_by(idx,hmfc) %>% 
    summarise(n1_2 = sum(KP_conf==1 | KP_conf == 2),
              per1_2 = n1_2/n()*100,
              n3 = sum(KP_conf==3),
              per3 = n3/n()*100,
              n4 = sum(KP_conf==4),
              per4 = n4/n()*100,
              total = n()) %>% 
    ungroup() %>% 
    select(-idx) %>% 
    group_by(hmfc) %>% 
    mutate(across(.cols = everything(),
                  .fns = list(mean=mean,sd=sd),
                  .names = "{.col}.{.fn}")) %>% 
    ungroup() %>% 
    select(-c(2:8)) %>% 
    slice(match(row_order,hmfc))
  
  
  # Print table using kable
  table_hmfc_x_conf_ppts %>% 
    mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                          hmfc=="miss"~"Miss",
                          hmfc=="fa"~"FA",
                          hmfc=="cr"~"CR")) %>% 
    rename(Response=hmfc) %>% 
    kable(caption = paste0("Table X. ",doexp," Response x rating (mean across ppts), N = ",N),
          digits=2,
          col.names = c("Response",
                        "M n 1-2",
                        "SD n 1-2",
                        "M % 1-2",
                        "SD % 1-2",
                        "M n 3",
                        "SD n 3",
                        "M % 3",
                        "SD % 3",
                        "M n 4",
                        "SD n 4",
                        "M % 4",
                        "SD % 4",
                        "M total",
                        "SD total")) %>% 
    kable_classic_2()
  
  # Exp 2. Re-calculate the proportion of HCMs once old items that had no study phase 
  # decision (i.e., studresp == "None") are filtered out
  # Create table like R&T2020 Table 1
  if(doexp=="e2"){
    table_hmfc_x_conf2 <- 
      recog_data %>% 
      filter(studresp=="q"|studresp=="p"|is.na(studresp)==1) %>% 
      group_by(hmfc) %>% 
      summarise(n1_2 = sum(KP_conf==1 | KP_conf == 2),
                per1_2 = n1_2/n()*100,
                n3 = sum(KP_conf==3),
                per3 = n3/n()*100,
                n4 = sum(KP_conf==4),
                per4 = n4/n()*100,
                total = n()) %>% 
      slice(match(row_order,hmfc))
    
    
    # Make table using kable
    table_hmfc_x_conf2 %>% 
      mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                            hmfc=="miss"~"Miss",
                            hmfc=="fa"~"FA",
                            hmfc=="cr"~"CR")) %>% 
      rename(Response=hmfc) %>% 
      kable(caption = "Table X. NO NA STUDY RESPS. Response x rating (aggregated across ppts)",
            digits=2,
            col.names = c("Response",
                          "n 1-2",
                          "% 1-2",
                          "n 3",
                          "% 3",
                          "n 4",
                          "% 4",
                          "total")) %>% 
      kable_classic_2()
    
    
    # Check equivalent table, but averaged across ppts, rather than aggregated
    table_hmfc_x_conf_ppts <- 
      recog_data %>% 
      filter(studresp=="q"|studresp=="p"|is.na(studresp)==1) %>% 
      group_by(idx,hmfc) %>% 
      summarise(n1_2 = sum(KP_conf==1 | KP_conf == 2),
                per1_2 = n1_2/n()*100,
                n3 = sum(KP_conf==3),
                per3 = n3/n()*100,
                n4 = sum(KP_conf==4),
                per4 = n4/n()*100,
                total = n()) %>% 
      ungroup() %>% 
      select(-idx) %>% 
      group_by(hmfc) %>% 
      mutate(across(.cols = everything(),
                    .fns = list(mean=mean,sd=sd),
                    .names = "{.col}.{.fn}")) %>% 
      ungroup() %>% 
      select(-c(2:8)) %>% 
      slice(match(row_order,hmfc))
    
    
    # Make table using kable
    table_hmfc_x_conf_ppts %>% 
      mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                            hmfc=="miss"~"Miss",
                            hmfc=="fa"~"FA",
                            hmfc=="cr"~"CR")) %>% 
      rename(Response=hmfc) %>% 
      kable(caption = "Table X. NO NA STUDY RESPS. Response x rating (mean across ppts)",
            digits=2,
            col.names = c("Response",
                          "M n 1-2",
                          "SD n 1-2",
                          "M % 1-2",
                          "SD % 1-2",
                          "M n 3",
                          "SD n 3",
                          "M % 3",
                          "SD % 3",
                          "M n 4",
                          "SD n 4",
                          "M % 4",
                          "SD % 4",
                          "M total",
                          "SD total")) %>% 
      kable_classic_2()
  }
  

  ## Table -----------------------------------------------------------------------
  fc_summary <- 
    fc_data %>% 
    group_by(idx, rating16_L) %>% 
    summarise(nFC = n(),
              nCorrect  = sum(fc_item_selected == 1),
              pCorrect  = nCorrect / n() * 100,
              M_FC_conf = mean(KP_conf))
  
  
  # combine YN and FC data
  all_summary <- 
    recog_hfa_summary %>% left_join(demog_data)
  
  
  # YN: Table of mean pH, pF, and Pr
  table_H_FA_dpri <- 
    all_summary %>% 
      ungroup() %>% 
      summarise(M_pH  = mean(pH), 
                SD_pH = sd(pH),
                M_pF  = mean(pF),
                SD_pF = sd(pF),
                M_dpri  = mean(dpri),
                SD_dpri = sd(dpri)) %>% 
      kable(caption = paste0("Table X. ",doexp," Mean pH, pF, dpri"),
            digits=2
            ) %>% 
      kable_classic_2()
  
  # show table of H FA dpri
  print(table_H_FA_dpri)
  
  # greater precision (for reporting Exp 2)
  all_summary %>% 
    ungroup() %>% 
    summarise(M_pH  = mean(pH), 
              SD_pH = sd(pH),
              M_pF  = mean(pF),
              SD_pF = sd(pF),
              M_dpri  = mean(dpri),
              SD_dpri = sd(dpri)) %>% as.data.frame()
  
  
  # make id and YN rating factors
  fc_summary <- 
    fc_summary %>% mutate(idx=factor(idx),
                           rating16_L=factor(rating16_L))
  
  
  #-------------------------------------------------------------------------------
  # FC
  
  # N trials each FC cond
  fc_summary %>% group_by(rating16_L) %>% count()
  
  length(unique(fc_summary$idx))
  
  # N FC conditions x idx (max=6)
  fc_summary %>% 
    group_by(idx) %>% 
    count() %>% 
    as.data.frame()
  
  # n each FC condition
  fc_n_summary <- 
    fc_data %>% 
    group_by(idx,rating16_L) %>%
    summarise(ntrls = n()) %>% 
    group_by(rating16_L) %>% 
    summarise(M  = mean(ntrls), 
              SD = sd(ntrls)) %>% 
    pivot_wider(names_from="rating16_L", 
                values_from=c("M","SD"),
                names_vary="slowest") %>% 
    kable(caption = paste0("Table X Mean number trials in each FC condition"),
          digits=2) %>% 
    kable_classic_2()
  
  print(fc_n_summary)

  ## Figure----------------------------------------------------------------------
  
  panel_lab <- c("A","B")
  
  # n each FC trial type
  p1 <- 
    fc_summary %>% 
    ggplot() + 
    geom_quasirandom(aes(x=rating16_L,y=nFC,group=idx,alpha=0.1),
                     dodge.width = 0.4, varwidth = TRUE,alpha=0.1)+
    #geom_hline(yintercept = 50,lty=2)+
    scale_x_discrete(breaks = c(1:6))+
    xlab("2AFC condition") +
    ylab("Mean Frequency")+
    ylim(c(0,100))+
    scale_y_continuous(breaks = seq(0, 100, by = 10)) +
    theme_classic() +
    theme(legend.position="none") +
    stat_summary(aes(x=rating16_L,y=nFC,group=1), fun.data = "mean_cl_boot",colour="black",size=0.65) +
    stat_summary(aes(x=rating16_L,y=nFC,group=1), fun.data = "mean_cl_boot",colour="black",linewidth=0.5,geom="line") +
    ggtitle(paste0("Figure - M freq x 2AFC condition. ",doexp))
  
  print( p1 )
  
  

  ## Statistics-------------------------------------------------------------------
  
  # Single-item recognition
  recog_hfa_summary %>% ggplot(aes(HminF)) + geom_histogram() + geom_density()
  print( t.test(recog_hfa_summary$HminF, mu=0) ) # H - FA 
  print( ttestBF(recog_hfa_summary$HminF, mu=0) ) # H - FA
  print( cohen.d(recog_hfa_summary$HminF, mu=0,f=NA) ) # 
  

  
  # FC
  # prep for analysis with glmer
  fc_data <- 
    fc_data %>% 
    mutate(idx=factor(idx),
           rating16_L=factor(rating16_L))
  
  # glmer
  # outcome: FC accuracy (1 or 0 on each trial)
  # fixed eff: FC condition
  # random eff: ppt
  # random intercepts for each ppt
  null <- glmer(fc_item_selected ~ 1 + (1|idx),
                data=fc_data, 
                family=binomial(link = "logit"))
  
  m1   <- glmer(fc_item_selected ~ rating16_L + (1|idx), 
                data=fc_data, 
                family=binomial(link = "logit"))
  
  # a model with random intercepts AND slopes returns error - is singular
  # go with simpler model
  # m2 <- glmer(fc_item_selected ~ rating16_L + (1+rating16_L|idx), 
  #               data=fc_data, family=binomial(link = "logit")) 
  # 
  # 02-10-23
  # Can add in ",control = glmerControl(calc.derivs=FALSE)" to force converge
  # in exps 1a and 1b; still doesn't work for exp2
  
  # compare m1 with null (Chi-sq)
  print(paste0('GLMM: FC_accuracy x condition----------------------------------------'))
  print( anova(m1,null) )
  
  # go with m1
  m1
  
  # check residuals for overdispersion since is GLMM
  DHARMa::testDispersion(m1)
  
  # check residuals for other assumptions
  simulationOutput <- DHARMa::simulateResiduals(fittedModel = m1, plot =T)
  
  # EMMs estimate the marginal means (proportions)
  # type="response" back transforms parameters to original scale
  em1 <- emmeans(m1, ~ rating16_L, type="response") 
  
  # see ref grid
  #ref_grid(m1)
  #ref_grid(null)
  
  # em for null
  #emmeans(null, ~1,type="response") 
  
  # For point in GD of ms re Exp. 2: summary(emmeans(null, ~1,type="response"))$prob
  
  # show emms, confidence intervals=T, tests=T
  print( summary(em1, infer=c(confit=T,tests=T), adjust="holm") )
  
  # make EMM plot
  emms_plot <- 
    tibble(summary(em1)) %>% 
    mutate(prob=prob*100,
           SE=SE*100,
           asymp.LCL=asymp.LCL*100,
           asymp.UCL=asymp.UCL*100) %>% 
    ggplot(aes(x=rating16_L,y=prob))+
    geom_quasirandom(aes(x=rating16_L,y=pCorrect,group=idx,alpha=0.1),
                     dodge.width = 0.4, varwidth = TRUE,alpha=0.2, 
                     data=fc_summary,colour="darkgrey")+
    geom_hline(yintercept = 50,lty=2,linewidth=0.5,colour="black")+
    geom_line(group=1,linewidth=0.8,color="#515A5A")+
    geom_point(size=3,color="#424949") +
    geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,width=0.2),linewidth=0.5,color="#424949")+
    scale_x_discrete(labels = c("1-1","2-2","3-3","4-4","5-5","6-6"))+
    scale_y_continuous(breaks = seq(0,100,25),limits = c(0,100))+
    xlab("2AFC condition") +
    ylab("Percent Correct")+
    #ylim(c(0,100))+
    theme_classic() +
    ggtitle(paste0("Data ",fig1_title)) +
    theme(axis.line = element_line(),
          axis.text.y = element_text(size=13),
          axis.text.x = element_text(size=12),
          axis.title = element_text(size=14),
          legend.position = "none",
          panel.spacing.y = unit(1.5, "lines"),
          plot.title = element_text(hjust = 0.5,face="bold"),
          plot.margin = unit(c(t=0,r=1.5,b=1.75,l=0), "lines"))
  
  # show plot
  print( emms_plot )
  
  # save plot
  ggsave(paste0("2_Figure_emms_plot_",doexp,".png"),emms_plot,width=2.75,height=2.75)
  
  # save for use in multipanel plot later on
  save(emms_plot, file = paste0("2_Figure_emms_plot_",doexp,".rdata"))
  
  
  
  # Analyse FC confidence ratings-------------------------------------------------
  # FC
  # prep for analysis with glmer
  fc_data <- 
    fc_data %>% 
    mutate(idx=factor(idx),
           rating16_L=factor(rating16_L),
           fc_item_selected=factor(fc_item_selected))
  
  # glmer
  # outcome: FC conf (1-4 on each trial)
  # fixed eff: FC condition, corr/incorr
  # random eff: ppt
  # random intercepts for each ppt
  H0 <- lmer(KP_conf ~ 1 + (1|idx),
                data=fc_data)
  
  # additive
  H1  <- lmer(KP_conf ~ rating16_L + fc_item_selected + (1|idx), 
                data=fc_data)
  
  # interaction
  H2  <- lmer(KP_conf ~ rating16_L + fc_item_selected + rating16_L*fc_item_selected + (1|idx), 
              data=fc_data)
  
  # anova interaction model
  aov_H2 <- anova(H2)
  
  print(paste0('LMM: FC_conf x condition and decision----------------------------------------'))
  print(aov_H2)
  
  # EMMs estimate the marginal means (proportions)
  # type="response" back transforms parameters to original scale
  emm_options(disable.pbkrtest = TRUE) # disable df calcs
  emFCconf <- emmeans(H2, ~ rating16_L*fc_item_selected)
  
  # see ref grid
  #ref_grid(H2)
  #ref_grid(H1)
  #ref_grid(H0)
  
  # em for null
  #emmeans(H0, ~1)
  
  # show emFCconf, confidence intervals=T, tests=T
  emFCconf

  pd <- position_dodge(width = 0.2)
  
  # make EMM plot
  emFCconf_plot <- 
    tibble(summary(emFCconf)) %>% 
    mutate(fc_item_selected=factor(fc_item_selected)) %>% 
    ggplot(aes(x=rating16_L,y=emmean,color=fc_item_selected))+
    geom_line(aes(group=fc_item_selected,linetype=fc_item_selected),linewidth=0.8,position=pd)+
    scale_linetype(labels=c("Incorrect","Correct"),
                   guide = guide_legend(reverse=TRUE))+
    scale_color_manual(values=c("#873600","#DC7633"),
                       labels=c("Incorrect","Correct"),
                       guide = guide_legend(reverse=TRUE)) +
    geom_point(size=3,position=pd,color="#6E2C00") +
    geom_errorbar(aes(ymin=asymp.LCL,ymax=asymp.UCL,width=0.2),linewidth=0.5,position=pd,color="#6E2C00")+
    scale_x_discrete(labels = c("1-1","2-2","3-3","4-4","5-5","6-6"))+
    scale_y_continuous(breaks = seq(1,4,1),limits = c(1,4))+
    xlab("2AFC condition") +
    ylab("Mean Confidence Rating")+
    theme_classic() +
    ggtitle(fig2_title) +
    theme(legend.position="none",
          plot.title = element_text(hjust = 0.5,face="bold"),
          axis.text.y = element_text(size=11),
          axis.text.x = element_text(size=10),
          axis.title = element_text(size=12)) +
    labs(color  = "fc_item_selected", linetype = "fc_item_selected")

  # show plot
  print( emFCconf_plot )
  
  # save plot
  ggsave(paste0("2_Figure_emFCconf_plot_",doexp,".png"),emFCconf_plot,width=2.75,height=2.75)
  
  # save for use in multipanel plot later on
  save(emFCconf_plot, file = paste0("2_Figure_emFCconf_plot_",doexp,".rdata"))
  
}
