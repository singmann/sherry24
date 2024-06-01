# Chris Berry 2022
#
# Preprocessing and checks

# clear everything
rm(list=ls())

# load tidyverse
library(tidyverse)

# write to file?
write_to_file <- FALSE

# user input
doexp  <- 3  # 1=exp1a (faces), 2=exp1b (words), 3=exp2 (=exp1a+study_task)

expID  <- c("e1a","e1b","e2")

# read in data (downloaded from jatos and converted to csv using opensesame)
data <- read_csv(paste0('0_rawdata/',expID[doexp],'.csv'), 
                 col_types = cols(response_kp_sex=col_character()))

# give ppts unique code based on jatosID
rawdata <- 
  data %>% 
  mutate(idx = match(paste0(jatosStudyResultId), unique(paste0(jatosStudyResultId)))) 

# check experiment title (should be one type only)
rawdata %>% 
  group_by(title) %>% 
  count()
  
# check N
rawdata %>% 
  group_by(idx) %>% 
  count() %>% as.data.frame()

exp <- rawdata$title[1]
if(exp=="EA_exp_1a"){explabel<-"e1a"}else
  if(exp=="EA_exp_1b"){explabel<-"e1b"}else
    if(exp=="EA_exp_2"){explabel<-"e2"}

if(explabel=="e1a"|explabel=="e1b"){
  # select the columns of interest
  data <- 
    rawdata %>% 
    select(idx = idx,                   # unique ppt id (created in R)
           datetime = datetime,         # date and time started
           jatosID = jatosStudyResultId,# jatos ID
           os_ppt_no = subject_nr,      # random OS ppt no.: 0 or 1 (exp 1a) or 1-100 (exp 1b) - does nothing!
           exp = title,         # exp title
           cbcond = cbcond,     # cb condition (1 or 2)
           studstim = studstim, # ids of study items in order - use data$studstim[[1]]
           recog_trial = recog_trial,   # 1 to trial number
           recog_stimID = showrecog,       # stimulus number 1-200
           recog_cond   = showrecogID,  # recognition condition 1= old, 2 =new 
           recog_KP_zm   = response_kp_recog_YN,           # recognition letter Z or M
           recog_KP_conf = response_kp_recog_YN_CONF,      # confidence rating 1-4
           recog_RT_zm   = response_time_kp_recog_YN,      # RT recognition old-new
           recog_RT_conf = response_time_kp_recog_YN_CONF, # RT recog rating 1-4
           fc_trial      = fc_trial,    # 1 to trial number
           fc_left_id    = showFC1,     # left stimulus ID
           fc_left_cond  = showFC1ID,   # left stimulus condition code
           fc_right_id   = showFC2,     # right stimulus ID
           fc_right_cond = showFC2ID,   # right stimulus condition code
           fc_KP_lr      = response_kp_FC_LR, # forced choice press letters D=left, J = right
           fc_KP_conf    = response_kp_FC_LR_CONF,      # confidence in left-right decision
           fc_RT_lr      = response_time_kp_FC_LR,      # RT for left-right decision
           fc_RT_conf    = response_time_kp_FC_LR_CONF, # RT for FC confidence decision
           sex_kp_mfor   = response_kp_sex,     # m(ale) f(emale) o(ther) r(ather not say)
           age_kp        = textbox_displaytext)  # age
}else
  if(explabel=="e2"){
    # select the columns of interest
    data <- 
      rawdata %>% 
      select(idx = idx,                   # unique ppt id (created in R)
             datetime = datetime,         # date and time started
             jatosID = jatosStudyResultId,# jatos ID
             os_ppt_no = subject_nr,      # random OS ppt no.: 0 or 1 (exp 1a) or 1-100 (exp 1b) - does nothing!
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
             fc_trial      = fc_trial,    # 1 to trial number
             fc_left_id    = showFC1,     # left stimulus ID
             fc_left_cond  = showFC1ID,   # left stimulus condition code
             fc_right_id   = showFC2,     # right stimulus ID
             fc_right_cond = showFC2ID,   # right stimulus condition code
             fc_KP_lr      = response_kp_FC_LR, # forced choice press letters D=left, J = right
             fc_KP_conf    = response_kp_FC_LR_CONF,      # confidence in left-right decision
             fc_RT_lr      = response_time_kp_FC_LR,      # RT for left-right decision
             fc_RT_conf    = response_time_kp_FC_LR_CONF, # RT for FC confidence decision
             sex_kp_mfor   = response_kp_sex,     # m(ale) f(emale) o(ther) r(ather not say)
             age_kp        = textbox_displaytext)  # age
  }


# copy idx, os_ppt_no and datetime
pptid <-
  data %>% 
  group_by(idx) %>% 
  summarise(exp=exp[1],idx=idx[1], os_ppt_no=os_ppt_no[1], datetime=datetime[1])

# study_data
if(explabel=="e1a"|explabel=="e1b"){
  # get list of stimulus IDs presented at study
  study_data <-
    data %>% 
    group_by(idx) %>% 
    summarise(stud_ids = 
                str_extract_all(studstim[[1]],pattern="\\b[0-9.]+\\b") %>% 
                unlist() %>% 
                as.numeric() )
}else
  if(explabel=="e2"){
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
  }

# filter data for recognition trials (rows 1:200)
# z = new / m = old
# 1=not at all confident
# 4=totally confident

slice_loc_rec <- c(1,1,101)
slice_loc_rec_end <- c(200,200,300)

recog_data  <- 
  data %>% 
  group_by(idx) %>% 
  slice(slice_loc_rec[doexp]:slice_loc_rec_end[doexp]) %>% 
  select(exp,idx,cbcond,starts_with("recog_")) %>% 
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

# filter data for forced-choice trials (201 to one less than total number of rows)
# cond:old-new x 1-6 rating
# Each stimulus in the FC phase has a code to identify
# the stimulus type (old, new) and YN rating previously given (1-6):
# 1=old1,2=new1
# 3=old2,4=new2
# 5=old3,6=new3
# 7=old4,8=new4
# 9=old5,10=new5
# 11=old6,12=new6

oldID <- c(1,3,5,7,9,11)  # odd
newID <- c(2,4,6,8,10,12) # even

slice_loc_fc <- c(201,201,301) # location to slice file (e2 has 100 extra rows for study phase)

fc_data <- 
  data %>% 
  group_by(idx) %>%
  slice(slice_loc_fc[doexp]:(n()-1)) %>% 
  select(exp,idx,cbcond,starts_with("fc_")) %>% 
  rename_all(function(x) gsub("fc_", "",x)) %>% 
  mutate(left_id    = as.numeric(left_id),
         left_cond  = as.numeric(left_cond),
         right_id   = as.numeric(right_id),
         right_cond = as.numeric(right_cond)) %>% 
  mutate(fc_item_selected = case_when(left_cond %% 2 !=0 & KP_lr == "d" ~ 1, #if L=old and L sel "old_selected"
                                      left_cond %% 2 !=0 & KP_lr == "j" ~ 0, #if L=old and R sel "new_selected"
                                      left_cond %% 2 ==0 & KP_lr == "d" ~ 0, #if L=new and L sel "new_selected"
                                      left_cond %% 2 ==0 & KP_lr == "j" ~ 1  #if L=new and R sel "old_selected"
                                      )) %>% 
  mutate(rating16_L = (left_cond + left_cond %% 2)/2,
         rating16_R = (right_cond + right_cond %% 2)/2) %>% 
  mutate(left_on  = case_when(left_cond %% 2 !=0 ~ "old",
                              left_cond %% 2 ==0 ~ "new"),
         right_on = case_when(right_cond %% 2 !=0 ~ "old",
                              right_cond %% 2 ==0 ~ "new"))

#check - should be TRUE
sum(fc_data$rating16_L - fc_data$rating16_R) == 0

# filter data for last row, which contains the demographic data
demog_data  <- 
  data %>% 
  group_by(idx) %>%
  slice(n()) %>% 
  select(exp,idx,cbcond,sex_kp_mfor, age_kp)

demog_data <-
  demog_data %>% 
  mutate(sex_kp_mfor = case_when(sex_kp_mfor=="m" ~ "male",
                                 sex_kp_mfor=="f" ~ "female",
                                 sex_kp_mfor=="o" ~ "other",
                                 sex_kp_mfor=="r" ~ "rathernotsay"))

# add demog details to recog_data
recog_data <- recog_data %>% left_join(demog_data, by = c("idx","exp","cbcond"))

# add demog details to fc_data
fc_data <- fc_data %>% left_join(demog_data, by = c("idx","exp","cbcond"))
  

# More checks-------------------------------------------------------------------
# recognition 
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

# number of FC trials by condition type (rating 1-6)
fc_data %>% group_by(rating16_L) %>% count()

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

# subset recog_data for checks with FC
recog_data_chk <- 
  recog_data %>% 
  select(idx,stimID,cond,rating16) %>% mutate(stimID=as.numeric(stimID))
  
# check fc trials contained the correct items
# check left FC item coded with same rating as YN task; should be zero
fc_data %>% 
  select(idx,left_id,rating16_L) %>% 
  left_join(recog_data_chk,by=c("left_id"="stimID","idx"="idx")) %>% 
  ungroup() %>% 
  summarise(chk=sum(rating16_L-rating16))

# check right FC item coded with same rating as YN task; should be zero
fc_data %>% 
  select(idx,right_id,rating16_R) %>% 
  left_join(recog_data_chk,by=c("right_id"="stimID","idx"="idx")) %>% 
  ungroup() %>% 
  summarise(chk=sum(rating16_R-rating16))

# check left and right items had same rating in YN; should be zero
fc_data %>% ungroup() %>% summarise(sum(rating16_L-rating16_R))

# check studied and non-studied on each FC; should be zero
fc_data %>% ungroup() %>% summarise(sum(left_on==right_on))

#------------------------------------------------------------------------------


if(expID[doexp]=="e2"){
  # add study data to recog_data
  recog_data <- 
    recog_data %>% 
    mutate(stimID=as.numeric(stimID)) %>% 
    group_by(idx) %>% 
    left_join(study_data_chk,by=c("stimID"="stud_ids","idx"="idx","exp"="exp","cbcond"="cbcond"))
}

# write results to file
N <- length(demog_data$idx) #get N

if(write_to_file==TRUE){
  write_csv(pptid,paste0("1_",explabel,"_pptids_N",N,".csv"))
  write_csv(study_data,paste0("1_",explabel,"_study_data_N",N,".csv"))
  write_csv(recog_data,paste0("1_",explabel,"_recog_data_N",N,".csv"))
  write_csv(fc_data, paste0("1_",explabel,"_fc_data_N",N,".csv"))
  write_csv(demog_data, paste0("1_",explabel,"_demog_data_N",N,".csv"))
}


