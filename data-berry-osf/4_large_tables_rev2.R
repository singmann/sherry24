# Chris Berry 2023
# Make tables for multiple exps
# Jan/Feb 2024: added exponential and gamma uvsd

rm(list=ls())

library(tidyverse)
library(kableExtra)

# recog data
d_e1a <- read_csv("2_e1a_recog_data_N72.csv",show_col_types = FALSE)
d_e1b <- read_csv("2_e1b_recog_data_N72.csv",show_col_types = FALSE) 
d_e2 <-  read_csv("2_e2_recog_data_N72.csv",show_col_types = FALSE) 
d_e3 <-  read_csv("2_e3_recog_data_N72.csv",show_col_types = FALSE) 

# fc_data
fc_e1a <- read_csv("2_e1a_fc_data_N72.csv",show_col_types = FALSE)
fc_e1b <- read_csv("2_e1b_fc_data_N72.csv",show_col_types = FALSE) 
fc_e2 <-  read_csv("2_e2_fc_data_N72.csv",show_col_types = FALSE) 


# Model fits: which participants to exclude due to being nonfittable?
exc_e1a <- c()
exc_e1b <- c()
exc_e2  <- c(43, 68)
exc_e3  <- c(24)


# read model fits, exclude relevant ppts
uv_e1a <-  read_csv("3_uvsd_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
uv_e1b <-  read_csv("3_uvsd_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
uv_e2  <-  read_csv("3_uvsd_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
uv_e3  <-  read_csv("3_uvsd_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

dp_e1a <-  read_csv("3_dpsd_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
dp_e1b <-  read_csv("3_dpsd_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
dp_e2  <-  read_csv("3_dpsd_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
dp_e3  <-  read_csv("3_dpsd_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

tht_e1a <-  read_csv("3_2ht_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
tht_e1b <-  read_csv("3_2ht_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
tht_e2  <-  read_csv("3_2ht_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
tht_e3  <-  read_csv("3_2ht_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

msd_e1a <-  read_csv("3_msd_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
msd_e1b <-  read_csv("3_msd_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
msd_e2  <-  read_csv("3_msd_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
msd_e3  <-  read_csv("3_msd_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

gum_e1a <-  read_csv("3_gumbel_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
gum_e1b <-  read_csv("3_gumbel_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
gum_e2  <-  read_csv("3_gumbel_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
gum_e3  <-  read_csv("3_gumbel_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

log_e1a <-  read_csv("3_logistic_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
log_e1b <-  read_csv("3_logistic_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
log_e2  <-  read_csv("3_logistic_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
log_e3  <-  read_csv("3_logistic_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

wei_e1a <-  read_csv("3_weibull_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
wei_e1b <-  read_csv("3_weibull_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
wei_e2  <-  read_csv("3_weibull_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
wei_e3  <-  read_csv("3_weibull_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

lno_e1a <-  read_csv("3_lognorm_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
lno_e1b <-  read_csv("3_lognorm_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
lno_e2  <-  read_csv("3_lognorm_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
lno_e3  <-  read_csv("3_lognorm_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

exo_e1a <-  read_csv("3_expo_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
exo_e1b <-  read_csv("3_expo_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
exo_e2  <-  read_csv("3_expo_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
exo_e3  <-  read_csv("3_expo_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

gam_e1a <-  read_csv("3_gamma_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
gam_e1b <-  read_csv("3_gamma_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
gam_e2  <-  read_csv("3_gamma_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
gam_e3  <-  read_csv("3_gamma_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

# bind model fits from exps together
uvsd <- bind_rows("1a"=uv_e1a,"1b"=uv_e1b,"2"=uv_e2,"3"=uv_e3,.id="exp_label") 
dpsd <- bind_rows("1a"=dp_e1a,"1b"=dp_e1b,"2"=dp_e2,"3"=dp_e3,.id="exp_label") 
tht  <- bind_rows("1a"=tht_e1a,"1b"=tht_e1b,"2"=tht_e2,"3"=tht_e3,.id="exp_label")
msd  <- bind_rows("1a"=msd_e1a,"1b"=msd_e1b,"2"=msd_e2,"3"=msd_e3,.id="exp_label")
gum  <- bind_rows("1a"=gum_e1a,"1b"=gum_e1b,"2"=gum_e2,"3"=gum_e3,.id="exp_label")
log  <- bind_rows("1a"=log_e1a,"1b"=log_e1b,"2"=log_e2,"3"=log_e3,.id="exp_label")
wei  <- bind_rows("1a"=wei_e1a,"1b"=wei_e1b,"2"=wei_e2,"3"=wei_e3,.id="exp_label")
lno  <- bind_rows("1a"=lno_e1a,"1b"=lno_e1b,"2"=lno_e2,"3"=lno_e3,.id="exp_label")
exo  <- bind_rows("1a"=exo_e1a,"1b"=exo_e1b,"2"=exo_e2,"3"=exo_e3,.id="exp_label")
gam  <- bind_rows("1a"=gam_e1a,"1b"=gam_e1b,"2"=gam_e2,"3"=gam_e3,.id="exp_label")

# checks - should be zero
uvsd %>% filter(is.na(pG)) %>% count()
dpsd %>% filter(is.na(pG)) %>% count()
tht  %>% filter(is.na(pG)) %>% count()
msd  %>% filter(is.na(pG)) %>% count()
gum  %>% filter(is.na(pG)) %>% count()
log  %>% filter(is.na(pG)) %>% count()
wei  %>% filter(is.na(pG)) %>% count()
lno  %>% filter(is.na(pG)) %>% count()
exo  %>% filter(is.na(pG)) %>% count()
gam  %>% filter(is.na(pG)) %>% count()

# checks - should be zero
uvsd %>% filter(convr!=0) %>% count()
dpsd %>% filter(convr!=0) %>% count()
tht  %>% filter(convr!=0) %>% count()
msd  %>% filter(convr!=0) %>% count()
gum  %>% filter(convr!=0) %>% count()
log  %>% filter(convr!=0) %>% count()
wei  %>% filter(convr!=0) %>% count()
lno  %>% filter(convr!=0) %>% count()
exo  %>% filter(convr!=0) %>% count()
gam  %>% filter(convr!=0) %>% count()

# checks - should be zero
# for some models and ppts chk_lik not exactly zero (suspect due to rounding)
# round to 10 D.P and check.
uvsd %>% filter(round(chk_lik,10)!=0) %>% count()
dpsd %>% filter(round(chk_lik,10)!=0) %>% count()
tht  %>% filter(round(chk_lik,10)!=0) %>% count()
msd  %>% filter(round(chk_lik,10)!=0) %>% count()
gum  %>% filter(round(chk_lik,10)!=0) %>% count()
log  %>% filter(round(chk_lik,10)!=0) %>% count()
wei  %>% filter(round(chk_lik,10)!=0) %>% count()
lno  %>% filter(round(chk_lik,10)!=0) %>% count()
exo  %>% filter(round(chk_lik,10)!=0) %>% count()
gam  %>% filter(round(chk_lik,10)!=0) %>% count()

# check tht para ests of g
tht %>% select(g1,g2,g3,g4,g5,g6) %>% mutate(sumg = g1+g2+g3+g4+g5+g6) %>% as.data.frame()
tht  %>% filter(chk_sumg!=1) %>% count() # check sum g1-g6; should be zero
tht  %>% filter(g6<0) %>% count() # check g6 positive; should be zero


# check extreme estimates
uvsd %>%
  select(exp_label,ppt,muo,sigo,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()


dpsd %>%
  select(exp_label,ppt,dpri,Ro,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()

# investigate extreme c5 in dpsd
dpsd %>% group_by(exp_label) %>% filter(c5>100) %>% as.data.frame() # extreme +ve c5 have N6=0
dpsd %>% group_by(exp_label) %>% filter(c5>100) %>% summarise(sum(N6>0))
dpsd %>% group_by(exp_label) %>% filter(c5>100) %>% count() # n extreme each exp

# in dpsd, 3 ppts with highest c5 values have sub 0.5 6-6 accuracy:
# F for old and new items with 5 ratings fails in simulations
dpsd %>% filter(c5 > 10000000) %>% select(exp_label, ppt, c5, Ro, sim_eFCpcor6)


# check sim vs. analytic 2AFC
uvsd %>% 
  summarise(d11 = sim_eFCpcor1 - eFCpcor1,
            d22 = sim_eFCpcor2 - eFCpcor2,
            d33 = sim_eFCpcor3 - eFCpcor3,
            d44 = sim_eFCpcor4 - eFCpcor4,
            d55 = sim_eFCpcor5 - eFCpcor5,
            d66 = sim_eFCpcor6 - eFCpcor6) %>% 
  pivot_longer(cols=everything(), names_to = "condition", values_to = "diff") %>% 
  ggplot(aes(x = condition, y = diff)) +
  ylab("Simulated minus analytic FCcorr") +
  geom_boxplot()

# check sim vs. analytic 2AFC
dpsd %>% 
  summarise(d11 = sim_eFCpcor1 - eFCpcor1,
            d22 = sim_eFCpcor2 - eFCpcor2,
            d33 = sim_eFCpcor3 - eFCpcor3,
            d44 = sim_eFCpcor4 - eFCpcor4,
            d55 = sim_eFCpcor5 - eFCpcor5,
            d66 = sim_eFCpcor6 - eFCpcor6) %>% 
  pivot_longer(cols=everything(), names_to = "condition", values_to = "diff") %>% 
  ggplot(aes(x = condition, y = diff)) +
  ylab("Simulated minus analytic FCcorr") +
  geom_boxplot()

# outliers in the 5-5 condition due to integration function failing for 
# 9 participants
# Therefore always use the sim_eFC* results
dpsd %>% 
    filter(abs((sim_eFCpcor5 - eFCpcor5)) > 0.05) %>% 
    select(ppt,exp_label,sim_eFCpcor5,eFCpcor5) %>% 
    as.data.frame()

# outliers in the 6-6 condition 
# Therefore always use the sim_eFC* results
dpsd %>% 
  filter(abs((sim_eFCpcor6 - eFCpcor6)) > 0.05) %>% 
  select(ppt,exp_label,sim_eFCpcor6,eFCpcor6) %>% 
  as.data.frame()

# replot sim minus integrated (without outlier)
dpsd %>%
  filter(abs((sim_eFCpcor5 - eFCpcor5)) < 0.05) %>% 
  filter(abs((sim_eFCpcor6 - eFCpcor6)) < 0.05) %>% 
  summarise(d11 = sim_eFCpcor1 - eFCpcor1,
            d22 = sim_eFCpcor2 - eFCpcor2,
            d33 = sim_eFCpcor3 - eFCpcor3,
            d44 = sim_eFCpcor4 - eFCpcor4,
            d55 = sim_eFCpcor5 - eFCpcor5,
            d66 = sim_eFCpcor6 - eFCpcor6) %>% 
  pivot_longer(cols=everything(), names_to = "condition", values_to = "diff") %>% 
  ggplot(aes(x = condition, y = diff)) +
  ylab("Simulated minus analytic FCcorr") +
  geom_boxplot()



tht %>%
  select(exp_label,ppt,dn,do,g1,g2,g3,g4,g5,g6) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()


msd %>%
  select(exp_label,ppt,dpri,la,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()

msd %>% group_by(exp_label) %>% filter(dpri>100) # extreme +ve dpri
msd %>% group_by(exp_label) %>% filter(dpri>100)
msd %>% group_by(exp_label) %>% filter(dpri>100) %>% count() # n extreme each exp


msd %>%
  filter(dpri < 100) %>% 
  select(exp_label,ppt,dpri,la,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()


gum %>%
  select(exp_label,ppt,loc,sca,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()

log %>% 
  select(exp_label,ppt,loc,sca,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()

wei %>% 
  select(exp_label,ppt,sha,sca,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()

lno %>% 
  select(exp_label,ppt,mu,sig,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()

exo %>% 
  select(exp_label,ppt,rateO,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()

# check rateO only
exo %>% 
  ggplot(aes(rateO))+
  geom_boxplot()


exo %>% 
  filter(if_any(starts_with("sim_eFC"), ~ .x < 0.5)) %>%  # if any of FC accuracy < 0.5
  glimpse %>% 
  select(exp_label,ppt,rateO,c1,c2,c3,c4,c5,starts_with("sim_eFC"),d_eStrength1) %>% 
  as.data.frame()
                      
exo %>% 
  filter(if_any(starts_with("sim_eFC"), ~ .x ==0)) %>%  # if any of FC accuracy < 0.5
  glimpse %>% 
  select(exp_label,ppt,rateO,c1,c2,c3,c4,c5,starts_with("sim_eFC"),d_eStrength1) %>% 
  as.data.frame()


# check gamma
gam %>% 
  select(exp_label,ppt,sha,sca,c1,c2,c3,c4,c5) %>% 
  pivot_longer(cols=-c(exp_label,ppt),names_to="parameter",values_to="estimate") %>% 
  ggplot(aes(x=parameter,y=estimate,group=parameter))+
  geom_boxplot()


# Table: H and FA rates, dpri (data)---------------------------------------------
recog_data <- 
  bind_rows(d_e1a,d_e1b,d_e2,d_e3) %>% 
  mutate(exp_label = case_when(exp=="EA_exp_1a" ~ "1a",
                               exp=="EA_exp_1b" ~ "1b",
                               exp=="EA_exp_2" ~ "2",
                               exp=="EA_exp_3" ~ "3",
                               exp=="EA_exp_3_forTB"~"3"),
         stud_label= case_when(exp=="EA_exp_1a" ~ "Memorise items",
                               exp=="EA_exp_1b" ~ "Memorise items",
                               exp=="EA_exp_2" ~ "Decide age",
                               exp=="EA_exp_3" ~ "Decide age",
                               exp=="EA_exp_3_forTB"~"Decide age") )

# YN: H and FA rates
recog_hfa_summary <- 
  recog_data %>% 
  group_by(exp_label,stud_label,idx) %>%
  summarise(pH = sum(hmfc == "hit") / (sum(hmfc == "hit") + sum(hmfc == "miss")),
            pF = sum(hmfc == "fa") / (sum(hmfc == "fa") + sum(hmfc == "cr")),
            HminF = pH - pF,
            dpri = qnorm(pH) - qnorm(pF))

# YN: Table of mean pH, pF, and dpri
recog_hfa_summary %>% 
  group_by(exp_label,stud_label) %>% 
  summarise(M_pH  = mean(pH), 
            SD_pH = sd(pH),
            M_pF  = mean(pF),
            SD_pF = sd(pF),
            M_dpri  = mean(dpri),
            SD_dpri = sd(dpri)) %>% 
  kable(caption = paste0("Table 1 Mean pH, pF, dpri"),
        digits=2
  ) %>% 
  kable_classic_2()

# check ns
recog_hfa_summary %>% group_by(exp_label,stud_label) %>% count()



# Table: %HCMs -----------------------------------------------------------------
#
# Use same row order as R&T2020 for table
row_order <- c("hit","miss","fa","cr")

# Create table like R&T2020 Table 1
table_hmfc_x_conf <- 
  recog_data %>% 
  group_by(exp_label,hmfc) %>% 
  summarise(n1_2 = sum(KP_conf==1 | KP_conf == 2),
            per1_2 = n1_2/n()*100,
            n3 = sum(KP_conf==3),
            per3 = n3/n()*100,
            n4 = sum(KP_conf==4),
            per4 = n4/n()*100,
            total = n()) %>% 
  slice(match(row_order,hmfc))


# Print table using kable
table_hmfc_x_conf %>% 
  mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                        hmfc=="miss"~"Miss",
                        hmfc=="fa"~"FA",
                        hmfc=="cr"~"CR")) %>% 
  ungroup() %>% 
  select(-exp_label) %>% 
  rename(Response=hmfc) %>% 
  kable(caption = paste0("Table X. Response x rating (aggregated across ppts)"),
        digits=2,
        col.names = c("",
                      "n",
                      "%",
                      "n",
                      "%",
                      "n",
                      "%",
                      "n")) %>% 
  kable_classic_2() %>% 
  add_header_above(c(" "=1,"1-2"=2,"3"=2,"4"=2,"Total"=1)) %>% 
  group_rows("Experiment 1a", start_row=1, end_row=4) %>% 
  group_rows("Experiment 1b", start_row=5, end_row=8) %>% 
  group_rows("Experiment 2",  start_row=9, end_row=12) %>% 
  group_rows("Experiment 3",  start_row=13, end_row=16)



## Table: nFC trials x condition------------------------------------------------

fc_data <- 
  bind_rows(fc_e1a,fc_e1b,fc_e2) %>% 
  mutate(exp_label = case_when(exp=="EA_exp_1a" ~ "1a",
                               exp=="EA_exp_1b" ~ "1b",
                               exp=="EA_exp_2" ~ "2",
                               exp=="EA_exp_3" ~ "3",
                               exp=="EA_exp_3_forTB"~"3"))

# n each FC condition
fc_n_summary <- 
  fc_data %>% 
  group_by(exp_label,idx,rating16_L) %>%
  summarise(ntrls = n())

# M n each FC condition
fc_n_summary %>% 
  group_by(exp_label,rating16_L) %>% 
  summarise(M  = mean(ntrls), 
            SD = sd(ntrls)) %>% 
  pivot_wider(names_from="rating16_L", 
              values_from=c("M","SD"),
              names_vary="slowest") %>% 
  kable(caption = paste0("Table X Mean number trials in each FC condition"),
        digits=2, 
        col.names = c(" ",
                      "M",
                      "SD",
                      "M",
                      "SD",
                      "M",
                      "SD",
                      "M",
                      "SD",
                      "M",
                      "SD",
                      "M",
                      "SD") ) %>% 
  kable_classic_2() %>% 
  add_header_above(c("Forced-choice condition"=1,"1"=2,"2"=2,"3"=2,"4"=2,"5"=2,"6"=2))

# check ns
recog_hfa_summary %>% group_by(exp_label,stud_label) %>% count()



# Table: UVSD DPSD Model Paras M SE --------------------------------------------


# Table: M para ests

# UVSD
uvsd_M_SE_pars <-
  uvsd %>% 
  group_by(exp_label) %>% 
  summarise(M_d  = mean(muo),
            SE_d = sd(muo)/sqrt(n()),
            M_sigo = mean(sigo),
            SE_sigo = sd(sigo)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

# Table of M SE para values
uvsd_m_se <- 
  uvsd_M_SE_pars %>% 
    group_by(exp_label) %>% 
    pivot_longer(!exp_label,
                 names_to=c('stat','para'),
                 names_sep="_") %>% 
    pivot_wider(names_from="stat",values_from="value") %>% 
    pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


# DPSD
dpsd_M_SE_pars <-
  dpsd %>% 
  filter(c5 < 100) %>% # Filter out inds with extreme c5 for this table
  group_by(exp_label) %>% 
  summarise(M_dpri = mean(dpri),
            SE_dpri = sd(dpri)/sqrt(n()),
            M_Ro = mean(Ro),
            SE_Ro = sd(Ro)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

# Table of M SE para values
dpsd_m_se <- 
  dpsd_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


m_se <- bind_rows(uvsd_m_se,dpsd_m_se)


m_se %>% 
  kable(caption = paste0("Table X.Parameter estimates M and SE"),
        digits=2,
        col.names = c(" ",
                      "M",
                      "SE",
                      "M",
                      "SE",
                      "M",
                      "SE",
                      "M",
                      "SE")) %>% 
  kable_classic_2() %>% 
  add_header_above(c(" "=1,"1a"=2, "1b"=2, "2"=2, "3"=2)) %>% 
  add_header_above(c(" "=1,"Experiment"=8)) %>% 
  group_rows("UVSD", start_row=1, end_row=7) %>% 
  group_rows("DPSD", start_row=8, end_row=14) 


# Table GOF models--------------------------------------------------------------

# UVSD and DPSD

# g-sq
uvsd_per_ns_Gsq <- 
  uvsd %>% 
  group_by(exp_label) %>% 
  summarise(
    #model="uvsd",
    #n_ns_pGsq = sum(pG > 0.05),
    uvsd_n = n(),
    uvsd_per_ns_Gsq = sum(pG > 0.05)/n()*100)

uvsd_per_ns_Gsq


dpsd_per_ns_Gsq <- 
  dpsd %>% 
  group_by(exp_label) %>% 
  summarise(
    #model="dpsd",
    #n_ns_pGsq = sum(pG > 0.05),
    dpsd_n = n(),
    dpsd_per_ns_Gsq = sum(pG > 0.05)/n()*100)

dpsd_per_ns_Gsq



# delta-aic
uvsd_lik <- 
  uvsd %>% 
  select(exp_label,ppt,lglik,aic,bic) %>% 
  rename_with(.cols = -c(ppt,exp_label), function(x){paste0("uvsd_", x)}) 


dpsd_lik <-
  dpsd %>% 
  select(exp_label,ppt,lglik,aic,bic) %>% 
  rename_with(.cols = -c(ppt,exp_label), function(x){paste0("dpsd_", x)})


dAIC <- 
  uvsd_lik %>% 
  left_join(dpsd_lik,by=join_by(exp_label,ppt)) %>% 
  rowwise() %>% 
  mutate(uvsd_delta_aic = uvsd_aic - min(uvsd_aic, dpsd_aic),
         dpsd_delta_aic = dpsd_aic - min(uvsd_aic, dpsd_aic)) %>% ungroup()


# obtain percentage best fit ppts
# obtain M dAIC
# Put in Table
dAIC %>% 
  group_by(exp_label) %>% 
  summarise(uvsd_M_delta_aic = mean(uvsd_delta_aic),
            dpsd_M_delta_aic = mean(dpsd_delta_aic),
            uvsd_perBF = sum(uvsd_delta_aic == 0)/n()*100,
            dpsd_perBF = sum(dpsd_delta_aic == 0)/n()*100,
            n=n()
            #uvsd_nBF = sum(uvsd_delta_aic == 0),
            #dpsd_nBF = sum(dpsd_delta_aic == 0)
  ) %>% 
  left_join(uvsd_per_ns_Gsq,by=join_by(exp_label,n==uvsd_n)) %>% 
  left_join(dpsd_per_ns_Gsq,by=join_by(exp_label,n==dpsd_n)) %>% 
  relocate(dpsd_per_ns_Gsq, .after=exp_label) %>% 
  relocate(uvsd_per_ns_Gsq, .after=exp_label) %>% 
  kable(caption = paste0("Table X. GOF UVSD and DPSD"),
        digits=2,
        col.names = c("",
                      "UVSD",
                      "DPSD",
                      "UVSD",
                      "DPSD",
                      "UVSD",
                      "DPSD",
                      "")) %>% 
  add_header_above(c("Experiment"=1,"% ppts w ns G-sq"=2,"delta-AIC"=2,"% ppts BF by AIC"=2,"N"=1)) %>% 
  kable_classic_2()


# look at histograms
dAIC_long <- 
  dAIC %>% 
  select(c(exp_label,ppt,contains("_delta_aic"))) %>% 
  pivot_longer(starts_with(c("uvsd","dpsd"))) %>% 
  separate(col=name,into=c("model","delta","aic"),sep="_") %>%
  select(-delta,-aic) %>% 
  rename(dAIC=value)

dAIC_long %>% 
  ggplot(aes(dAIC))+
  geom_histogram()+
  facet_wrap(~exp_label*model,nrow=4,ncol=2)+
  theme_minimal()





# Table: Model %HCM-------------------------------------------------------------

# Add percentage of (h/m/f/c x low/med/high) responses
uv_res <-
  uvsd %>% 
  group_by(exp_label) %>% 
  tibble( phit_Low = pO4 / (pO4 + pO5 + pO6)*100,
          phit_Med = pO5 / (pO4 + pO5 + pO6)*100,
          phit_Hig = pO6 / (pO4 + pO5 + pO6)*100,
          pmiss_Low = pO3 / (pO1 + pO2 + pO3)*100,
          pmiss_Med = pO2 / (pO1 + pO2 + pO3)*100,
          pmiss_Hig = pO1 / (pO1 + pO2 + pO3)*100,
          pfa_Low = pN4 / (pN4 + pN5 + pN6)*100,
          pfa_Med = pN5 / (pN4 + pN5 + pN6)*100,
          pfa_Hig = pN6 / (pN4 + pN5 + pN6)*100,
          pcr_Low = pN3 / (pN1 + pN2 + pN3)*100,
          pcr_Med = pN2 / (pN1 + pN2 + pN3)*100,
          pcr_Hig = pN1 / (pN1 + pN2 + pN3)*100 )

dp_res <-
  dpsd %>% 
  group_by(exp_label) %>% 
  tibble( phit_Low = pO4 / (pO4 + pO5 + pO6)*100,
          phit_Med = pO5 / (pO4 + pO5 + pO6)*100,
          phit_Hig = pO6 / (pO4 + pO5 + pO6)*100,
          pmiss_Low = pO3 / (pO1 + pO2 + pO3)*100,
          pmiss_Med = pO2 / (pO1 + pO2 + pO3)*100,
          pmiss_Hig = pO1 / (pO1 + pO2 + pO3)*100,
          pfa_Low = pN4 / (pN4 + pN5 + pN6)*100,
          pfa_Med = pN5 / (pN4 + pN5 + pN6)*100,
          pfa_Hig = pN6 / (pN4 + pN5 + pN6)*100,
          pcr_Low = pN3 / (pN1 + pN2 + pN3)*100,
          pcr_Med = pN2 / (pN1 + pN2 + pN3)*100,
          pcr_Hig = pN1 / (pN1 + pN2 + pN3)*100 )


# Use same row order as R&T2020 for table
row_order <- c("hit","miss","fa","cr")

uv_table_hmfc_x_conf_ppts <- 
  uv_res %>% 
  select(exp_label,ppt,
         phit_Low,phit_Med,phit_Hig,
         pmiss_Low,pmiss_Med,pmiss_Hig,
         pfa_Low,pfa_Med,pfa_Hig,
         pcr_Low,pcr_Med,pcr_Hig) %>% 
  pivot_longer(cols=!c(ppt,exp_label),
               names_prefix="p",
               names_to=c('hmfc','conf'),
               names_sep="_",
               values_to="per") %>% 
  group_by(exp_label,hmfc,conf) %>% 
  summarise(Mper=mean(per),
            SEper=sd(per)/sqrt(n())) %>% 
  pivot_wider(names_from=c("conf"),
              values_from=c("Mper","SEper")) %>% 
  ungroup() %>% 
  group_by(exp_label) %>% 
  slice(match(row_order,hmfc)) %>% 
  select(exp_label,hmfc,
         Mper_Low,SEper_Low,
         Mper_Med,SEper_Med,
         Mper_Hig,SEper_Hig) %>% 
  mutate(blank="")


dp_table_hmfc_x_conf_ppts <- 
  dp_res %>% 
  select(exp_label,ppt,
         phit_Low,phit_Med,phit_Hig,
         pmiss_Low,pmiss_Med,pmiss_Hig,
         pfa_Low,pfa_Med,pfa_Hig,
         pcr_Low,pcr_Med,pcr_Hig) %>% 
  pivot_longer(cols=!c(ppt,exp_label),
               names_prefix="p",
               names_to=c('hmfc','conf'),
               names_sep="_",
               values_to="per") %>% 
  group_by(exp_label,hmfc,conf) %>% 
  summarise(Mper=mean(per),
            SEper=sd(per)/sqrt(n())) %>% 
  pivot_wider(names_from=c("conf"),
              values_from=c("Mper","SEper")) %>% 
  ungroup() %>% 
  group_by(exp_label) %>% 
  slice(match(row_order,hmfc)) %>% 
  select(exp_label,hmfc,
         Mper_Low,SEper_Low,
         Mper_Med,SEper_Med,
         Mper_Hig,SEper_Hig)

# check: should be zero
sum(uv_table_hmfc_x_conf_ppts$exp_label != dp_table_hmfc_x_conf_ppts$exp_label)
sum(uv_table_hmfc_x_conf_ppts$hmfc != dp_table_hmfc_x_conf_ppts$hmfc)

# remove before bind
dp_table_hmfc_x_conf_ppts <- dp_table_hmfc_x_conf_ppts %>% ungroup() %>% select(-c(exp_label,hmfc))

model_pers <- bind_cols(uv_table_hmfc_x_conf_ppts,dp_table_hmfc_x_conf_ppts)


# Print table using kable
model_pers %>% 
  mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                        hmfc=="miss"~"Miss",
                        hmfc=="fa"~"FA",
                        hmfc=="cr"~"CR")) %>% 
  rename(Response=hmfc) %>% 
  ungroup() %>% 
  select(-exp_label) %>% 
  kable(caption = paste0("Table X. Response x rating (mean across ppts and models)"),
        digits=2,
        col.names = c(" ",
                      "M",
                      "SE",
                      "M",
                      "SE",
                      "M",
                      "SE",
                      " ",
                      "M",
                      "SE",
                      "M",
                      "SE",
                      "M",
                      "SE")) %>% 
  kable_classic_2() %>% 
  add_header_above(c(" "=1,"1-2"=2,"3"=2,"4"=2," "=1,"1-2"=2,"3"=2,"4"=2)) %>% 
  add_header_above(c(" "=1,"UVSD"=6," "=1,"DPSD"=6)) %>% 
  group_rows("Experiment 1a", start_row=1, end_row=4) %>% 
  group_rows("Experiment 1b", start_row=5, end_row=8) %>% 
  group_rows("Experiment 2",  start_row=9, end_row=12) %>% 
  group_rows("Experiment 3",  start_row=13, end_row=16)




# Tables for other models -------------------------------------------------------


# Table Parameter estimates other models

# 2HT
tht_M_SE_pars <-
  tht %>% 
  group_by(exp_label) %>% 
  summarise(M_do = mean(do),
            SE_do = sd(do)/sqrt(n()),
            M_dn = mean(dn),
            SE_dn = sd(dn)/sqrt(n()),
            M_g1 = mean(g1),
            SE_g1 = sd(g1)/sqrt(n()),
            M_g2 = mean(g2),
            SE_g2 = sd(g2)/sqrt(n()),
            M_g3 = mean(g3),
            SE_g3 = sd(g3)/sqrt(n()),
            M_g4 = mean(g4),
            SE_g4 = sd(g4)/sqrt(n()),
            M_g5 = mean(g5),
            SE_g5 = sd(g5)/sqrt(n()))

tht_m_se <- 
  tht_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


# MSD
msd_M_SE_pars <-
  msd %>% 
  filter(dpri < 100) %>% # Filter out inds with extreme dpri for this table
  group_by(exp_label) %>% 
  summarise(M_dpri = mean(dpri),
            SE_dpri = sd(dpri)/sqrt(n()),
            M_la = mean(la),
            SE_la = sd(la)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

msd_m_se <- 
  msd_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


# Gumbel
gum_M_SE_pars <-
  gum %>% 
  group_by(exp_label) %>% 
  summarise(M_loc = mean(loc),
            SE_loc = sd(loc)/sqrt(n()),
            M_sca = mean(sca),
            SE_sca = sd(sca)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

gum_m_se <- 
  gum_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


# Logistic
log_M_SE_pars <-
  log %>% 
  group_by(exp_label) %>% 
  summarise(M_loc = mean(loc),
            SE_loc = sd(loc)/sqrt(n()),
            M_sca = mean(sca),
            SE_sca = sd(sca)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

log_m_se <- 
  log_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


# Weibull
wei_M_SE_pars <-
  wei %>% 
  group_by(exp_label) %>% 
  summarise(M_sha = mean(sha),
            SE_sha = sd(sha)/sqrt(n()),
            M_sca = mean(sca),
            SE_sca = sd(sca)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

wei_m_se <- 
  wei_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


# Lognormal
lno_M_SE_pars <-
  lno %>% 
  group_by(exp_label) %>% 
  summarise(M_mu = mean(mu),
            SE_mu = sd(mu)/sqrt(n()),
            M_sig = mean(sig),
            SE_sig = sd(sig)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

lno_m_se <- 
  lno_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")


# Exponential
exo_M_SE_pars <-
  exo %>% 
  group_by(exp_label) %>% 
  summarise(M_rateO = mean(rateO),
            SE_rateO = sd(rateO)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

exo_m_se <- 
  exo_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")

exo_m_se


# Gamma
gam_M_SE_pars <-
  gam %>% 
  group_by(exp_label) %>% 
  summarise(M_sha = mean(sha),
            SE_sha = sd(sha)/sqrt(n()),
            M_sca = mean(sca),
            SE_sca = sd(sca)/sqrt(n()),
            M_c1 = mean(c1),
            SE_c1 = sd(c1)/sqrt(n()),
            M_c2 = mean(c2),
            SE_c2 = sd(c2)/sqrt(n()),
            M_c3 = mean(c3),
            SE_c3 = sd(c3)/sqrt(n()),
            M_c4 = mean(c4),
            SE_c4 = sd(c4)/sqrt(n()),
            M_c5 = mean(c5),
            SE_c5 = sd(c5)/sqrt(n()))

gam_m_se <- 
  gam_M_SE_pars %>% 
  group_by(exp_label) %>% 
  pivot_longer(!exp_label,
               names_to=c('stat','para'),
               names_sep="_") %>% 
  pivot_wider(names_from="stat",values_from="value") %>% 
  pivot_wider(names_from="exp_label", values_from=c("M","SE"),names_vary="slowest")



# bind together
m_se <- bind_rows(tht_m_se,
                  msd_m_se,
                  gum_m_se,
                  log_m_se,
                  wei_m_se,
                  lno_m_se,
                  exo_m_se,
                  gam_m_se)


m_se %>% 
  kable(caption = paste0("Table X.Parameter estimates M and SE"),
        digits=2,
        col.names = c(" ",
                      "M",
                      "SE",
                      "M",
                      "SE",
                      "M",
                      "SE",
                      "M",
                      "SE")) %>% 
  kable_classic_2() %>% 
  add_header_above(c(" "=1,"1a"=2, "1b"=2, "2"=2, "3"=2)) %>% 
  add_header_above(c(" "=1,"Experiment"=8)) %>% 
  group_rows("2HT", start_row=1, end_row=7) %>% 
  group_rows("MSD", start_row=8, end_row=14) %>% 
  group_rows("gumbel", start_row=15, end_row=21) %>% 
  group_rows("logistic", start_row=22, end_row=28) %>% 
  group_rows("weibull", start_row=29, end_row=35) %>% 
  group_rows("lognorm", start_row=36, end_row=42) %>% 
  group_rows("exo", start_row=43, end_row=48) %>% 
  group_rows("gamma",start_row=49, end_row=55)




# Table % HCMs in other models

hcm_res <-
  bind_rows("tht"=tht,"msd"=msd,"gum"=gum,"log"=log,"wei"=wei,"lno"=lno,"exo"=exo,"gam"=gam,.id="model_label") %>% 
  mutate(model_label=factor(model_label,levels=c("tht","msd","gum","log","wei","lno","exo","gam"))) %>% 
  group_by(model_label,exp_label,ppt) %>% 
  summarise( phit_Low = pO4 / (pO4 + pO5 + pO6)*100,
             phit_Med = pO5 / (pO4 + pO5 + pO6)*100,
             phit_Hig = pO6 / (pO4 + pO5 + pO6)*100,
             pmiss_Low = pO3 / (pO1 + pO2 + pO3)*100,
             pmiss_Med = pO2 / (pO1 + pO2 + pO3)*100,
             pmiss_Hig = pO1 / (pO1 + pO2 + pO3)*100,
             pfa_Low = pN4 / (pN4 + pN5 + pN6)*100,
             pfa_Med = pN5 / (pN4 + pN5 + pN6)*100,
             pfa_Hig = pN6 / (pN4 + pN5 + pN6)*100,
             pcr_Low = pN3 / (pN1 + pN2 + pN3)*100,
             pcr_Med = pN2 / (pN1 + pN2 + pN3)*100,
             pcr_Hig = pN1 / (pN1 + pN2 + pN3)*100 ) %>% 
  group_by(model_label,exp_label) %>% 
  summarise(M_phit_Low = mean(phit_Low),
            SE_phit_Low = sd(phit_Low)/sqrt(n()),
            M_phit_Med = mean(phit_Med),
            SE_phit_Med = sd(phit_Med)/sqrt(n()),
            M_phit_Hig = mean(phit_Hig),
            SE_phit_Hig = sd(phit_Hig)/sqrt(n()),
            M_pmiss_Low = mean(pmiss_Low),
            SE_pmiss_Low = sd(pmiss_Low)/sqrt(n()),
            M_pmiss_Med = mean(pmiss_Med),
            SE_pmiss_Med = sd(pmiss_Med)/sqrt(n()),
            M_pmiss_Hig = mean(pmiss_Hig),
            SE_pmiss_Hig = sd(pmiss_Hig)/sqrt(n()),
            M_pfa_Low = mean(pfa_Low),
            SE_pfa_Low = sd(pfa_Low)/sqrt(n()),
            M_pfa_Med = mean(pfa_Med),
            SE_pfa_Med = sd(pfa_Med)/sqrt(n()),
            M_pfa_Hig = mean(pfa_Hig),
            SE_pfa_Hig = sd(pfa_Hig)/sqrt(n()),
            M_pcr_Low = mean(pcr_Low),
            SE_pcr_Low = sd(pcr_Low)/sqrt(n()),
            M_pcr_Med = mean(pcr_Med),
            SE_pcr_Med = sd(pcr_Med)/sqrt(n()),
            M_pcr_Hig = mean(pcr_Hig),
            SE_pcr_Hig = sd(pcr_Hig)/sqrt(n())
            ) %>% 
  ungroup()


# % HCM only
hcm_res %>% 
  select(model_label,exp_label,M_pmiss_Hig,SE_pmiss_Hig) %>% 
  pivot_wider(names_from=model_label,values_from=c(M_pmiss_Hig,SE_pmiss_Hig),names_vary="slowest") %>% 
  kable(caption = paste0("Table. HCMs other models"),
        digits=2,
        col.names = c("",
                      rep(c("M","SE"),8))) %>% 
  kable_classic_2() %>% 
  add_header_above(c("Experiment"=1,"tht"=2, "msd"=2, "gum"=2, "log"=2, "wei"=2, "lno"=2, "exo"=2, "gam"=2))





# Table GOF Other models--------------------------------------------------------
# g-sq
tht_per_ns_Gsq <- 
  tht %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    tht_per_ns_Gsq = sum(pG > 0.05)/n()*100)

tht_per_ns_Gsq


msd_per_ns_Gsq <- 
  msd %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    msd_per_ns_Gsq = sum(pG > 0.05)/n()*100)

msd_per_ns_Gsq


gum_per_ns_Gsq <- 
  gum %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    gum_per_ns_Gsq = sum(pG > 0.05)/n()*100)

gum_per_ns_Gsq


log_per_ns_Gsq <- 
  log %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    log_per_ns_Gsq = sum(pG > 0.05)/n()*100)

log_per_ns_Gsq


wei_per_ns_Gsq <- 
  wei %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    wei_per_ns_Gsq = sum(pG > 0.05)/n()*100)

wei_per_ns_Gsq


lno_per_ns_Gsq <- 
  lno %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    lno_per_ns_Gsq = sum(pG > 0.05)/n()*100)

lno_per_ns_Gsq


exo_per_ns_Gsq <- 
  exo %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    exo_per_ns_Gsq = sum(pG > 0.05)/n()*100)

exo_per_ns_Gsq


gam_per_ns_Gsq <- 
  gam %>% 
  group_by(exp_label) %>% 
  summarise(
    n = n(),
    gam_per_ns_Gsq = sum(pG > 0.05)/n()*100)

gam_per_ns_Gsq


# Table Other models GOF G-sq
tht_per_ns_Gsq %>% 
  left_join(msd_per_ns_Gsq,by=join_by(exp_label,n)) %>% 
  left_join(gum_per_ns_Gsq,by=join_by(exp_label,n)) %>% 
  left_join(log_per_ns_Gsq,by=join_by(exp_label,n)) %>% 
  left_join(wei_per_ns_Gsq,by=join_by(exp_label,n)) %>% 
  left_join(lno_per_ns_Gsq,by=join_by(exp_label,n)) %>% 
  left_join(exo_per_ns_Gsq,by=join_by(exp_label,n)) %>% 
  left_join(gam_per_ns_Gsq,by=join_by(exp_label,n)) %>% 
  kable(caption = paste0("Table. Other models GOF"),
        digits=2,
        col.names = c("exp_label",
                      "n",
                      "THT",
                      "MSD",
                      "gumbel",
                      "logistic",
                      "weibull",
                      "lognormal",
                      "exponential",
                      "gamma")) %>% 
  kable_classic_2() 

