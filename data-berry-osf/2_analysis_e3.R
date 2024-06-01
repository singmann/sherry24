# Chris Berry 2023
# Experiment 3

# Analysis

rm(list = ls())

library(tidyverse)
library(kableExtra)
library(ggbeeswarm)
library(BayesFactor)
library(effsize)

doexp <- "e3" # 1=exp3(faces, 2xYN)
Ns    <- 72


# load data
study_data <- read_csv(paste0("1_",doexp,"_study_data_","N",Ns,".csv"))
recog_data <- read_csv(paste0("1_",doexp,"_recog_data_","N",Ns,".csv"),
                       col_types = cols(sex_kp_mfor=col_character()))
recog2_data<- read_csv(paste0("1_",doexp,"_recog2_data_","N",Ns,".csv"),
                       col_types = cols(sex_kp_mfor=col_character()))
demog_data <- read_csv(paste0("1_",doexp,"_demog_data_","N",Ns,".csv"),
                       col_types = cols(sex_kp_mfor=col_character()))


# Check exp ID
unique(study_data$exp)
unique(demog_data$exp)
unique(recog2_data$exp)
unique(recog_data$exp)


# Single-item recog 1-----------------------------------------------------------
# Check descriptives
recog_hfa_summary <- 
  recog_data %>% 
  group_by(idx) %>%
  summarise(pH = sum(hmfc == "hit") / (sum(hmfc == "hit") + sum(hmfc == "miss")),
            pF = sum(hmfc == "fa") / (sum(hmfc == "fa") + sum(hmfc == "cr")),
            HminF = pH - pF)

# check for pH-pF< 0.05
# no participants meet this criterion
# no participants excluded
recog_hfa_summary %>% as.data.frame()
print( recog_hfa_summary %>% filter(HminF <= 0.05) )


# Get n, age
print( paste0("N = ", length(unique(demog_data$idx)))  )
print( paste0("M age = ", round(mean(demog_data$age_kp, na.rm=T),2)) )
print( paste0("SD age = ", round(sd(demog_data$age_kp,na.rm=T),2)) )


# Get n in each counterbalance condition (randomly assigned by program)
demog_data %>% 
  group_by(cbcond) %>% 
  count()

N <- length(unique(demog_data$idx))

# save final data for later scripts
write_csv(study_data,paste0("2_",doexp,"_study_data_","N",N,".csv"))
write_csv(recog_data,paste0("2_",doexp,"_recog_data_","N",N,".csv"))
write_csv(recog2_data,paste0("2_",doexp,"_recog2_data_","N",N,".csv"))
write_csv(demog_data,paste0("2_",doexp,"_demog_data_","N",N,".csv"))



# study_data -------------------------------------------------------------------
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

print( table_stud_KPs )

# check for ppts with zero or one-type of response
M_ppt_stud_responses %>% 
  summarise(sum(nresp==0))

M_ppt_stud_responses %>% 
  summarise(sum(nKPolder==0))

M_ppt_stud_responses %>% 
  summarise(sum(nKPyounger==0))



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


# Make table using kable
table_HCM <-
  table_hmfc_x_conf %>% 
    mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                          hmfc=="miss"~"Miss",
                          hmfc=="fa"~"FA",
                          hmfc=="cr"~"CR")) %>% 
    rename(Response=hmfc) %>% 
    kable(caption = "Table X. Response x rating (aggregated across ppts)",
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

print( table_HCM )


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


# Make table using kable
table_hmfc_x_conf_ppts %>% 
  mutate(hmfc=case_when(hmfc=="hit"~"Hit",
                        hmfc=="miss"~"Miss",
                        hmfc=="fa"~"FA",
                        hmfc=="cr"~"CR")) %>% 
  rename(Response=hmfc) %>% 
  kable(caption = "Table X. Response x rating (mean across ppts)",
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


# Check the proportion of HCMs once old items that had no study phase 
# decision are filtered out
# Create table like R&T2020 Table 1
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
table_HCM_noStud <-
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

print( table_HCM_noStud )

# Make equivalent table, but averaged across ppts, rather than aggregated
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


# Recog phase 1-----------------------------------------------------------------
# YN: H and FA rates
recog_hfa_summary <- 
  recog_data %>% 
  group_by(idx) %>%
  summarise(pH = sum(hmfc == "hit") / (sum(hmfc == "hit") + sum(hmfc == "miss")),
            pF = sum(hmfc == "fa") / (sum(hmfc == "fa") + sum(hmfc == "cr")),
            HminF = pH - pF,
            dpri = qnorm(pH) - qnorm(pF)
            )

recog_hfa_summary %>% as.data.frame()

# YN: Table of mean pH, pF, and Pr
table_H_FA_dpri <- 
  recog_hfa_summary %>% 
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

print(  table_H_FA_dpri )


# Recog phase 2 - percentage trials old item selected 
recog2_summary <- 
  recog2_data %>% 
  group_by(idx) %>%
  summarise(nH   = sum(hmfc == "hit"),
            nF   = sum(hmfc == "fa"),
            nOld = sum(hmfc == "hit") + sum(hmfc == "miss"),
            nNew = sum(hmfc == "fa") + sum(hmfc == "cr"),
            pH = nH / nOld,
            pF = nF / nNew,
            HminF = pH - pF,
            MconfO = mean(kp16[cond==1]),
            MconfN = mean(kp16[cond==2])) %>% 
  rename_all(function(x) {paste0("R2_",x)}) %>% 
  rename(idx=R2_idx)

all_summary <- 
  recog_hfa_summary %>% left_join(recog2_summary,by="idx") %>% left_join(demog_data)


# Recog-1
recog_hfa_summary %>% ggplot(aes(HminF)) + geom_histogram() + geom_density()
print( t.test(recog_hfa_summary$HminF, mu=0) ) # H - FA 
print( ttestBF(recog_hfa_summary$HminF, mu=0) )  # H - FA
print( cohen.d(recog_hfa_summary$HminF, mu=0,f=NA) ) # 


# Recog-2
# Only possible to analyse ppts with trials in second single-item recog phase
all_summary2 <- 
  all_summary %>% 
    filter(R2_nOld > 0) %>% 
    mutate(n_trials = R2_nOld + R2_nNew) %>% 
    group_by(idx) %>% as.data.frame()


# Recog-2 details
recog2_n_trls <- 
  all_summary2 %>% 
    summarise(m_R2_n_trials = mean(n_trials),
              sd_R2_n_trials = sd(n_trials))  %>% 
    glimpse() %>% 
    kable(caption = paste0("Table X. ",doexp," Mean number items"),digits=2) %>% 
    kable_classic_2()

print( recog2_n_trls ) 

print( all_summary2 %>% select(n_trials) %>% summary )


table_recog2_mconf <- 
  all_summary2 %>% 
    summarise(m_R2_MconfO = mean(R2_MconfO),
              sd_R2_MconfO = sd(R2_MconfO),
              m_R2_MconfN = mean(R2_MconfN),
              sd_R2_MconfN = sd(R2_MconfN))  %>% 
    glimpse() %>% 
    kable(caption = paste0("Table X. ",doexp," Mean pH, pF, rating"),digits=2) %>% 
    kable_classic_2()

print( table_recog2_mconf )


table_recog2_mProp <- 
  all_summary2 %>% 
    summarise(m_R2_pH = mean(R2_pH),
              sd_R2_pH = sd(R2_pH),
              m_R2_pF = mean(R2_pF),
              sd_R2_pF = sd(R2_pF))  %>% 
    glimpse() %>% 
    kable(caption = paste0("Table X. ",doexp," Mean pH, pF, rating"),digits=2) %>% 
    kable_classic_2()

print( table_recog2_mProp )


# inspect dists
all_summary2 %>% ggplot(aes(R2_MconfO)) + geom_histogram()
all_summary2 %>% ggplot(aes(R2_MconfN)) + geom_histogram()
all_summary2 %>% ggplot(aes(R2_MconfO-R2_MconfN)) + geom_histogram()

all_summary2 %>% ggplot(aes(R2_pH)) + geom_histogram()
all_summary2 %>% ggplot(aes(R2_pF)) + geom_histogram()
all_summary2 %>% ggplot(aes(R2_pH-R2_pF)) + geom_histogram()

print( t.test(all_summary2$R2_MconfO,all_summary2$R2_MconfN,paired=TRUE) )
print( ttestBF(all_summary2$R2_MconfO,all_summary2$R2_MconfN,paired=TRUE) ) 
print( cohen.d(all_summary2$R2_MconfO,all_summary2$R2_MconfN,paired=TRUE) )

print( t.test(all_summary2$R2_pH,all_summary2$R2_pF,paired=TRUE) ) 
print( ttestBF(all_summary2$R2_pH,all_summary2$R2_pF,paired=TRUE) ) 
print( cohen.d(all_summary2$R2_pH,all_summary2$R2_pF,paired=TRUE) )

# check non-paras
wilcox.test(all_summary2$R2_MconfO,all_summary2$R2_MconfN)
wilcox.test(all_summary2$R2_pH,all_summary2$R2_pF)


all_summary %>% summarise(n()) # N = 72
all_summary %>% drop_na(R2_nOld) %>% summarise(n()) # N = 50 (22 with no recog 2 data)


# Plot Recog-2 conf for HCM and HCCR
p <- 
  all_summary %>% 
  drop_na(R2_nOld) %>% 
  select(idx,
         R2_MconfO,
         R2_MconfN) %>% 
  pivot_longer(cols=!idx,
               names_prefix="R2",
               names_to=c('stimulus'),
               values_to="confidence") %>% 
  ggplot() + 
  geom_quasirandom(aes(x=stimulus,y=confidence,group=idx,alpha=0.1),
                   dodge.width = 0.4, varwidth = TRUE,alpha=0.1,
                   groupOnX=TRUE,colour="blue")+
  scale_x_discrete(breaks = c(1:2))+
  scale_x_discrete(labels = c("HCCR","HCM"))+
  scale_y_continuous(breaks=c(1:6),labels=c("1","2","3","4","5","6"))+
 # ylim(c(1,6))+  
  xlab("Stimulus Type") +
  ylab("Confidence Rating")+
  theme_classic() +
  theme(legend.position="none") +
  stat_summary(aes(x=stimulus,y=confidence,group=1), fun.data = "mean_cl_boot",colour="black",fill="white",size=0.65) +
  stat_summary(aes(x=stimulus,y=confidence,group=1), fun.data = "mean_cl_boot",colour="black",linewidth=0.5,geom="line") +
  ggtitle(paste0("Fig conf \n x stim_",doexp)) 

p

# ggsave(paste0("2_Figure_conf_stim_",doexp,".png"),p,width=1.55,height=3.21) # not used in ms



# Plot Recog-2 conf DIFFERENCE for HCM and HCCR
p_diff <- 
  all_summary %>% 
  drop_na(R2_nOld) %>% 
  select(idx,
         R2_MconfO,
         R2_MconfN) %>% 
  mutate(diff_conf = R2_MconfO - R2_MconfN) %>% 
  select(idx, diff_conf) %>% 
  ggplot() + 
  geom_quasirandom(aes(x=1,y=diff_conf,group=idx,alpha=0.2),
                   alpha=0.2,width=0.1,
                   colour="darkgrey")+
  ylim(c(-6,6))+  
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  xlab("Data") +
  ylab("Confidence Rating \n (HCM - HCCR)")+
  theme_classic() +
  theme(legend.position="none") +
  stat_summary(aes(x=1,y=diff_conf), fun.data = "mean_cl_boot",colour="#424949",fill="white",size=0.35) +
  #ggtitle(paste0("Fig dconf \n x stim_",doexp)) +
  geom_hline(yintercept=0,lty=2)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(t=0,r=1.5,b=0,l=0), "lines"))


print( p_diff )

ggsave(paste0("2_Figure_dconf_stim_",doexp,".png"),p_diff,width=1.5,height=3.21)

# save for use in multipanel plot later on
save(p_diff, file = paste0("2_Figure_dconf_stim_",doexp,".rdata"))


# Plot H min FA
p_R2HminF <- 
  all_summary %>% 
  drop_na(R2_nOld) %>% 
  ggplot() + 
  geom_quasirandom(aes(x=1,y=R2_HminF,group=idx,alpha=0.2),
                   alpha=0.2,width=0.1,
                   colour="darkgrey")+
  ylim(c(-1,1))+  
  theme(axis.text.x = element_blank())+
  theme(axis.ticks.x = element_blank())+
  xlab("Data") +
  ylab("Hit - FA rate")+
  theme_classic() +
  theme(legend.position="none") +
  stat_summary(aes(x=1,y=R2_HminF), fun.data = "mean_cl_boot",colour="#424949",fill="white",size=0.35) +
  #ggtitle(paste0("Fig R2Hminf \n",doexp)) +
  geom_hline(yintercept=0,lty=2)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(t=0,r=1.5,b=0,l=0), "lines"))

print( p_R2HminF )
  
ggsave(paste0("2_Figure_R2HminF_",doexp,".png"),p_R2HminF,width=1.5,height=3.21)

# save for use in multipanel plot later on
save(p_R2HminF, file = paste0("2_Figure_R2HminF_",doexp,".rdata"))


