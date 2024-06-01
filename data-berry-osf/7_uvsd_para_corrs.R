
# Chris Berry 2024
# Explore correlations 1-1 FC accuracy and parameter estimates of UVSD and DPSD models
# (corrs of sigo and muo in uvsd are similar to Spanton and Berry 2020, 2022, and Dobbins 2023)
# General Discussion footnote

rm(list=ls())

library(tidyverse)
library(kableExtra)


# Model fits: which participants to exclude due to being nonfittable?
exc_e1a <- c()
exc_e1b <- c()
exc_e2  <- c(43, 68)
exc_e3  <- c(24)


# recog data
d_e1a <- read_csv("2_e1a_fc_data_N72.csv",show_col_types = FALSE) %>% filter(!(idx %in% exc_e1a))
d_e1b <- read_csv("2_e1b_fc_data_N72.csv",show_col_types = FALSE) %>% filter(!(idx %in% exc_e1b))
d_e2 <-  read_csv("2_e2_fc_data_N72.csv",show_col_types = FALSE) %>% filter(!(idx %in% exc_e2))
#d_e3 <-  read_csv("2_e3_fc_data_N72.csv",show_col_types = FALSE) %>% filter(!(idx %in% exc_e3))

dat <- bind_rows("1a"=d_e1a,"1b"=d_e1b,"2"=d_e2,.id="exp_label") 

# FC summary data
fc_summary <- 
  dat %>% 
  group_by(exp_label, idx, rating16_L) %>% 
  summarise(nFC = n(),
            nCorrect  = sum(fc_item_selected == 1),
            pCorrect  = nCorrect / n() * 100,
            M_FC_conf = mean(KP_conf))


# read model fits, exclude relevant ppts
uv_e1a <-  read_csv("3_uvsd_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
uv_e1b <-  read_csv("3_uvsd_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
uv_e2  <-  read_csv("3_uvsd_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
uv_e3  <-  read_csv("3_uvsd_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

uvsd <- bind_rows("1a"=uv_e1a,"1b"=uv_e1b,"2"=uv_e2,"3"=uv_e3,.id="exp_label") 

ggsave("15_uvsd_para_corr_sigo_muo.png",width=8,height=8)


# corr sigo and muo in uvsd?
# strong +ve correlations each experiment
uvsd %>% 
  ggplot(aes(x=muo,y=sigo)) +
  geom_point() +
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 2, label.y = 3.5) +
  facet_wrap(~exp_label) 


# dpsd
dp_e1a <-  read_csv("3_dpsd_results_exp1a_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1a))
dp_e1b <-  read_csv("3_dpsd_results_exp1b_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e1b))
dp_e2  <-  read_csv("3_dpsd_results_exp2_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e2))
dp_e3  <-  read_csv("3_dpsd_results_exp3_ind.csv",show_col_types = FALSE) %>% filter(!(ppt %in% exc_e3))

dpsd <- bind_rows("1a"=dp_e1a,"1b"=dp_e1b,"2"=dp_e2,"3"=dp_e3,.id="exp_label") 

# corr R and d' in dpsd
# not reliably correlated across exps
dpsd %>% 
  ggplot(aes(x=dpri,y=Ro)) +
  geom_point() +
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 1, label.y = 1) +
  facet_wrap(~exp_label) 



# MR: is 1-1 FC accuracy related to sigo, muo and c5?
dat_for_MR_uvsd <-
  fc_summary %>% 
    filter(rating16_L==1) %>% 
    left_join(uvsd,by=join_by(exp_label,idx==ppt))





# predicted accuracy is strongly negatively correlated with sigo in the UVSD model.
dat_for_MR_uvsd %>% 
  ggplot(aes(x=sigo,y=eFCpcor1)) +
  geom_point()+
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 1, label.y = 1) +
  facet_wrap(~exp_label)


# but observed accuracy is not correlated with sigo in the UVSD model.
dat_for_MR_uvsd %>% 
  ggplot(aes(x=sigo,y=pCorrect)) +
  geom_point()+
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 1, label.y = 1) + 
  facet_wrap(~exp_label)


# predicted accuracy is not correlated with muo in the UVSD model.
dat_for_MR_uvsd %>% 
  ggplot(aes(x=muo,y=eFCpcor1)) +
  geom_point()+
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 1, label.y = 1) +
  facet_wrap(~exp_label)


## (AND observed accuracy is not consistently correlated with muo across experiments)
# dat_for_MR_uvsd %>% 
#   ggplot(aes(x=muo,y=pCorrect)) +
#   geom_point()+
#   theme_light() +
#   geom_smooth(method="lm",se=F) +
#   stat_cor(method = "pearson", label.x = 1, label.y = 1) + 
#   facet_wrap(~exp_label)



# MR: is 1-1 FC accuracy related to Ro, dpri and c?
dat_for_MR_dpsd <-
  fc_summary %>% 
  filter(rating16_L==1) %>% 
  left_join(dpsd,by=join_by(exp_label,idx==ppt))




# correlations with 1-1 FC accuracy (observed and predicted) and sigo, muo, c5


# predicted accuracy is correlated with dpri in the DPSD model
dat_for_MR_dpsd %>% 
  ggplot(aes(x=dpri,y=eFCpcor1)) +
  geom_point()+
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 0, label.y = 1) + 
  facet_wrap(~exp_label)

# but observed accuracy is not correlated with dpri in the DPSD model.
dat_for_MR_dpsd %>% 
  ggplot(aes(x=dpri,y=pCorrect)) +
  geom_point()+
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 0, label.y = 5) + 
  facet_wrap(~exp_label)

# predicted accuracy is NOT correlated with Ro in the DPSD model
dat_for_MR_dpsd %>% 
  ggplot(aes(x=Ro,y=eFCpcor1)) +
  geom_point()+
  theme_light() +
  geom_smooth(method="lm",se=F) +
  stat_cor(method = "pearson", label.x = 0, label.y = 1) + 
  facet_wrap(~exp_label)

## (AND observed accuracy is NOT correlated with Ro in the DPSD model.)
# dat_for_MR_dpsd %>% 
#   ggplot(aes(x=Ro,y=pCorrect)) +
#   geom_point()+
#   theme_light() +
#   geom_smooth(method="lm",se=F) +
#   stat_cor(method = "pearson", label.x = 0, label.y = 5) + 
#   facet_wrap(~exp_label)