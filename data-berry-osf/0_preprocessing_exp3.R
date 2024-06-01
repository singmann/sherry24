# Chris Berry 2023
#
# Preprocessing and checks

# Note: In the first 10 participants (in Exp3_TB), 5 participants had FAIL jatos 
# label because they made zero HCM or zero HCR, so the experiment crashed when 
# it reached the second YN phase and no data was collected 
# This bug was fixed for subsequent participants

# clear everything
rm(list=ls())

# write to file?
write_to_file <- FALSE

# load tidyverse
library(tidyverse)

# jatosID of any ppts to exclude
exclude_ppt <- c() 


# read in data (downloaded from jatos and converted to csv using opensesame)
data1 <- read_csv('0_rawdata/e3_S_N42_FINISHED.csv',   col_types = cols(response_kp_sex=col_character()))
data2 <- read_csv('0_rawdata/e3_TB_a_N5_FAIL.csv')
data3 <- read_csv('0_rawdata/e3_TB_a_N5_FINISHED.csv',  col_types = cols(response_kp_sex=col_character()))
data4 <- read_csv('0_rawdata/e3_TB_b_N20_FINISHED.csv', col_types = cols(response_kp_sex=col_character()))

# check ns: 42 + 5 + 5 + 20 = 72
data1 %>% group_by(jatosStudyResultId) %>% count() %>% as.data.frame() # 42 
data2 %>% group_by(datetime) %>% count() %>% as.data.frame() # 5
data3 %>% group_by(jatosStudyResultId) %>% count() %>% as.data.frame() # 5
data4 %>% group_by(jatosStudyResultId) %>% count() %>% as.data.frame() # 20

# add unique identifier idx (1:72)
# base on datetime RTinst combo due to FAIL ppts having no jatosID
rawdata  <- 
  bind_rows(data1, data2, data3, data4) %>% 
  mutate(idx = match(paste0(datetime,response_time_inst_maxi),unique(paste0(datetime,response_time_inst_maxi)))) 

# check N = 72
rawdata %>% group_by(idx) %>% count() %>% as.data.frame()


# check n rows per ppt (should be zero if min rows = 300)
rawdata %>% 
  group_by(idx) %>% 
  count() %>% 
  filter(n<300)

ids_no_recog2_phase <- rawdata %>% group_by(idx) %>% count() %>% filter(n<=301) %>% select(idx)

# check experiment title (one type only)
rawdata %>% 
  group_by(title) %>% 
  count()

rawdata

exp <- rawdata$title[1]
  if(exp=="EA_exp_3"){doexp<-"e3"}else
    if(exp=="EA_exp_3_forTB"){doexp<-"e3"}

# select the columns of interest
data <- 
  rawdata %>% 
  select(idx = idx,                   # unique ppt id (created in R)
         datetime = datetime,         # date and time started
         jatosID = jatosStudyResultId,# jatos ID
         os_ppt_no = subject_nr,      # random ppt no. assigned 1-100
         exp = title,         # exp title
         cbcond = cbcond,     # cb condition (1 or 2)
         studstim = studstim, # ids of study items in order - use data$studstim[[1]]
         studresp = response_kp_study_YO, # study phase response Q=older, P=younger
         recog_trial = recog_trial,   # 1 to trial number
         recog_stimID = showrecog,       # stimulus number 1-200
         recog_cond   = showrecogID,  # recognition condition 1= old, 2 =new 
         recog_KP_zm   = response_kp_recog_YN,           # recognition letter Z or M
         recog_KP_conf = response_kp_recog_YN_CONF,      # confidence rating 1-4
         recog_RT_zm   = response_time_kp_recog_YN,      # RT recognition old-new
         recog_RT_conf = response_time_kp_recog_YN_CONF, # RT recog rating 1-4
         recog2_trial  = recog2_trial,    # 1 to trial number
         recog2_id    = showrecog2,     # stimulus ID
         recog2_cond  = showrecog2ID,   # stimulus condition
         recog2_kp16   = response_kp_recog2_1to6,     # 1-6 kp
         recog2_rt_kp16 = response_time_kp_recog2_1to6,   # RT for 1-6kp
         sex_kp_mfor   = response_kp_sex,     # m f o r
         age_kp        = textbox_displaytext)  # age

# copy idx, os_ppt_no and datetime
pptid <-
  data %>% 
  group_by(idx) %>% 
  summarise(exp=exp[1],idx=idx[1], os_ppt_no=os_ppt_no[1], datetime=datetime[1], jatosID=jatosID[1])

# study_data
# get list of stimulus IDs presented at study
study_data1 <-
  data %>% 
  group_by(idx) %>% 
  summarise(stud_ids = 
              str_extract_all(studstim[[1]],pattern="\\b[0-9.]+\\b") %>% 
              unlist() %>% 
              as.numeric() )

# get stud_juds 
study_data2 <- 
  data %>% 
  group_by(idx) %>% 
  slice(1:100) %>% 
  select(exp,idx,cbcond,studresp) %>% 
  mutate(stud_jud = case_when(studresp == "q" ~ "older",    # 
                              studresp == "p" ~ "younger")) # 


# bind stud ids and juds
study_data <- 
  study_data1 %>% 
  ungroup() %>% 
  select(stud_ids) %>% 
  bind_cols(study_data2) %>% 
  relocate(stud_ids,.after=stud_jud)

# check study obs (should be 100)
study_data %>% group_by(idx) %>% count() %>% as.data.frame()


# filter data for recognition trials (101:300)
# z = new / m = old
# 1=not at all confident
# 4=totally confident
recog_data  <- 
  data %>% 
  group_by(idx) %>% 
  slice(101:300) %>% 
  select(exp,idx,jatosID,cbcond,starts_with("recog_")) %>% 
  rename_all(function(x) gsub("recog_", "",x)) %>% 
  mutate(hmfc = case_when(cond == 1 & KP_zm == "z" ~ "miss",
                                cond == 1 & KP_zm == "m" ~ "hit",
                                cond == 2 & KP_zm == "z" ~ "cr",
                                cond == 2 & KP_zm == "m" ~ "fa"),
         rating16 = case_when( KP_zm == "z" & KP_conf == 4 ~ 1, # "sure new" 
                               KP_zm == "z" & KP_conf == 3 ~ 2,
                               KP_zm == "z" & KP_conf == 2 ~ 3,
                               KP_zm == "z" & KP_conf == 1 ~ 3,
                               KP_zm == "m" & KP_conf == 1 ~ 4,
                               KP_zm == "m" & KP_conf == 2 ~ 4,
                               KP_zm == "m" & KP_conf == 3 ~ 5,
                               KP_zm == "m" & KP_conf == 4 ~ 6   # "sure old"
                               ))

# filter for ppts who had trials in recog2
# filter data for forced-choice trials (201 to one less than total number of rows)
recog2_data <- 
  data %>% 
  filter(!(idx %in% ids_no_recog2_phase$idx)) %>% 
  group_by(idx) %>%
  slice(301:(n()-1)) %>%  
  select(exp,idx,cbcond,starts_with("recog2_")) %>% 
  rename_all(function(x) gsub("recog2_", "",x)) %>% 
  mutate(hmfc = case_when(cond == 1 & kp16 <= 3 ~ "miss",
                          cond == 1 & kp16 >= 4 ~ "hit",
                          cond == 2 & kp16 <= 3 ~ "cr",
                          cond == 2 & kp16 >= 4 ~ "fa"))
      


# filter data for last row, which is the demographic data
demog_data  <- 
  data %>% 
  group_by(idx) %>%
  slice(n()) %>% 
  select(exp,idx,jatosID,cbcond,sex_kp_mfor, age_kp)


# add demog details to recog_data
recog_data <- recog_data %>% left_join(demog_data, by = c("idx","exp","cbcond","jatosID"))

# add demog details to recog2_data
recog2_data <- recog2_data %>% left_join(demog_data, by = c("idx","exp","cbcond"))
  

# checks-------------------------------------------------------------------
# recognition 
recog_data %>% group_by(idx) %>% count() %>% filter(n!=200)

# n trials should be 200
recog_data %>% group_by(idx) %>% count() %>% 
  ungroup() %>% summarise(sum(n!=200)) # should be zero if each ppt had 200 trls

# n in each stimulus condition
# should be 100
recog_data %>% group_by(idx,cond) %>% count() %>% 
  ungroup() %>% 
  summarise(sum(n!=100)) # should be zero if each condition had 100 trls

# check for N high conf misses each ppt
recog_data %>% group_by(idx) %>% 
  summarise(sum(hmfc=="miss" & KP_conf==4)) %>% as.data.frame()

# check for N high conf crs each ppt
recog_data %>% group_by(idx) %>% 
  summarise(sum(hmfc=="cr" & KP_conf==4)) %>% as.data.frame()

# number of trials according to cond(old/new) x rating(1-6)
recog_data %>% group_by(idx,cond,rating16) %>% count() %>% as.data.frame()

# check study items presented at test
study_data_chk <- study_data %>% mutate(presented=1) # add col to enable check

# should be zero if all study items appeared at test and labelled as cond 1
recog_data %>% 
  mutate(stimID=as.numeric(stimID)) %>% 
  group_by(idx) %>% 
  left_join(study_data_chk,by=c("stimID"="stud_ids","idx"="idx")) %>% 
  filter(cond==1) %>% 
  summarise(chk=sum(presented)) %>% 
  ungroup() %>% 
  summarise(sum(chk!=100)) 

# should be zero if all new items in the recog phase were not presented at study
recog_data %>% 
  mutate(stimID=as.numeric(stimID)) %>% 
  group_by(idx) %>% 
  left_join(study_data_chk,by=c("stimID"="stud_ids","idx"="idx")) %>% 
  filter(cond==2) %>% 
  summarise(chk=sum(presented)) %>% 
  ungroup() %>% 
  summarise(sum(is.na(chk)==0)) 

# subset recog_data for checks with YN2
recog_data_chk <- 
  recog_data %>% 
  select(idx,stimID,cond,rating16) %>% mutate(stimID=as.numeric(stimID)) 

# should be zero if all items in YN-2 received a 1 sure new rating in YN-1
recog2_data %>% 
  select(idx,id) %>% 
  filter(id>0) %>% 
  mutate(id=as.numeric(id)) %>% 
  left_join(recog_data_chk,by=c("id"="stimID","idx"="idx")) %>% 
  ungroup() %>% 
  summarise(chk=sum(rating16!=1))

# number of recog2 trials by condition (should be equal)
recog2_data %>% filter(is.na(hmfc)==0) %>% group_by(idx,cond) %>% count() %>% as.data.frame()

# number of recog2 trials by kp
recog2_data %>%filter(is.na(hmfc)==0) %>%  group_by(idx,kp16) %>% count()

# number of recog2 trials by condition and kp
recog2_data %>% filter(is.na(hmfc)==0) %>% group_by(cond,kp16) %>% count()

#------------------------------------------------------------------------------

# add study data to recog_data
recog_data <- 
  recog_data %>% 
  mutate(stimID=as.numeric(stimID)) %>% 
  group_by(idx) %>% 
  left_join(study_data_chk,by=c("stimID"="stud_ids","idx"="idx","exp"="exp","cbcond"="cbcond"))

# write results to file
N <- length(demog_data$idx)

if(write_to_file==TRUE){
  write_csv(pptid,paste0("1_",doexp,"_pptids_N",N,".csv"))
  write_csv(study_data,paste0("1_",doexp,"_study_data_N",N,".csv"))
  write_csv(recog_data,paste0("1_",doexp,"_recog_data_N",N,".csv"))
  write_csv(recog2_data, paste0("1_",doexp,"_recog2_data_N",N,".csv"))
  write_csv(demog_data, paste0("1_",doexp,"_demog_data_N",N,".csv"))
}
