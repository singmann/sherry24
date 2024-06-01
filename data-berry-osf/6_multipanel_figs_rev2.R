# Chris Berry 2023
# Load in figures created previously and combine to multipanel figs
# Jan/Feb 2024: added exponential and gamma uvsd

rm(list=ls())

library(tidyverse)
library(gridExtra)



## Figure FC data and uvsd/dpsd model predictions-------------------------------

# DATA
load(paste0("2_Figure_emms_plot_e1a.rdata")) 
e1a_emms_plot <- emms_plot

load(paste0("2_Figure_emms_plot_e1b.rdata")) 
e1b_emms_plot <- emms_plot

load(paste0("2_Figure_emms_plot_e2.rdata")) 
e2_emms_plot <- emms_plot


# UVSD
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_uvsd.rdata")) 
e1a_uvsd_plot <- p

load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_uvsd.rdata")) 
e1b_uvsd_plot <- p

load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_uvsd.rdata")) 
e2_uvsd_plot <- p


# DPSD
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_dpsd.rdata")) 
e1a_dpsd_plot <- p

load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_dpsd.rdata")) 
e1b_dpsd_plot <- p

load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_dpsd.rdata")) 
e2_dpsd_plot <- p


# use grid.arrange to combine 
multi <-
  grid.arrange(e1a_emms_plot,
               e1a_uvsd_plot,
               e1a_dpsd_plot,
               e1b_emms_plot,
               e1b_uvsd_plot,
               e1b_dpsd_plot,
               e2_emms_plot, 
               e2_uvsd_plot,
               e2_dpsd_plot,             
               ncol=3,nrow=3)

ggsave(paste0("6_Figure_FCacc_multipanel_ind.png"),multi,width=8.25,height=8.25)


# agg

# UVSD
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_uvsd.rdata")) 
e1a_uvsd_plot_agg <- p

load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_uvsd.rdata")) 
e1b_uvsd_plot_agg <- p

load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_uvsd.rdata")) 
e2_uvsd_plot_agg <- p


# DPSD
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_dpsd.rdata")) 
e1a_dpsd_plot_agg <- p

load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_dpsd.rdata")) 
e1b_dpsd_plot_agg <- p

load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_dpsd.rdata")) 
e2_dpsd_plot_agg <- p

# use grid.arrange to combine 
multi <-
  grid.arrange(e1a_emms_plot,
               e1a_uvsd_plot_agg,
               e1a_dpsd_plot_agg,
               e1b_emms_plot,
               e1b_uvsd_plot_agg,
               e1b_dpsd_plot_agg,
               e2_emms_plot, 
               e2_uvsd_plot_agg,
               e2_dpsd_plot_agg,             
               ncol=3,nrow=3)

ggsave(paste0("6_Figure_FCacc_multipanel_agg.png"),multi,width=8.25,height=8.25)



## Figure FC confidence ratings-------------------------------------------------

# DATA
load(paste0("2_Figure_emFCconf_plot_e1a.rdata")) 
e1a_fc_conf_plot <- 
  emFCconf_plot + 
  theme(legend.position=c(0.35,0.8),
        legend.title=element_blank())
  
e1a_fc_conf_plot

load(paste0("2_Figure_emFCconf_plot_e1b.rdata")) 
e1b_fc_conf_plot <- emFCconf_plot

load(paste0("2_Figure_emFCconf_plot_e2.rdata")) 
e2_fc_conf_plot <- emFCconf_plot

# use grid.arrange to combine 
multi2 <-
  grid.arrange(e1a_fc_conf_plot,
               e1b_fc_conf_plot,
               e2_fc_conf_plot,             
               ncol=3,nrow=1)

ggsave(paste0("6_Figure_FCconf_multipanel.png"),multi2,width=6.5,height=3)




## Figure Exp 3: 2nd single-item recog phase data and model---------------------

# ind data

# DATA
load(paste0("2_Figure_R2HminF_e3.rdata")) 
e3_R2HminF_plot <- p_R2HminF + geom_text(x=0.935,y=1,label="A",size=5)

load(paste0("2_Figure_dconf_stim_e3.rdata")) 
e3_confdiff_plot <- p_diff + geom_text(x=0.935,y=6,label="B",size=5)

# UVSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_uvsd.rdata"))
e3_confdiff_plot_uvsd <- p_diff + geom_text(x=0.94,y=0.6,label="C",size=5)

# DPSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_dpsd.rdata"))
e3_confdiff_plot_dpsd <- p_diff + geom_text(x=0.94,y=0.6,label="D",size=5)


# use grid.arrange to combine 
multi3 <-
  grid.arrange(e3_R2HminF_plot,
               e3_confdiff_plot,
               e3_confdiff_plot_uvsd,
               e3_confdiff_plot_dpsd,             
               ncol=4,nrow=1)

ggsave(paste0("6_Figure_Exp3_multipanel_ind.png"),multi3,width=6.5,height=3)


# agg data

# UVSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_uvsd.rdata"))
e3_confdiff_plot_uvsd_agg <- p_diff + geom_text(x=0.94,y=0.6,label="C",size=5)

# DPSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_dpsd.rdata"))
e3_confdiff_plot_dpsd_agg <- p_diff + geom_text(x=0.94,y=0.6,label="D",size=5)


# use grid.arrange to combine 
multi3 <-
  grid.arrange(e3_R2HminF_plot,
               e3_confdiff_plot,
               e3_confdiff_plot_uvsd_agg,
               e3_confdiff_plot_dpsd_agg,             
               ncol=4,nrow=1)

ggsave(paste0("6_Figure_Exp3_multipanel_agg.png"),multi3,width=6.5,height=3)


theme_smaller_font <- 
  theme(axis.title = element_text(size=13),
        axis.text.x = element_text(size=10.5),
        axis.text.y = element_text(size=12))

## Figure FC data and other model predictions (ind)------------------------------

## Exp 1a

# 2ht
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_2ht.rdata")) 
e1a_2ht_plot <- p  + ggtitle("2HT") + xlab("2AFC Trial") + theme_smaller_font

# msd
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_msd.rdata")) 
e1a_msd_plot <- p  + ggtitle("MSD") + xlab("2AFC Trial") + theme_smaller_font

# gumbel-uvsd
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_gumbel.rdata")) 
e1a_gumbel_plot <- p + ggtitle("Gumbel-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# logistic-uvsd
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_logistic.rdata")) 
e1a_logistic_plot <- p + ggtitle("Logistic-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# weibull-uvsd
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_weibull.rdata")) 
e1a_weibull_plot <- p + ggtitle("Weibull-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# lognorm-uvsd
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_lognorm.rdata")) 
e1a_lognorm_plot <- p + ggtitle("Lognormal-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# expo-uvsd
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_expo.rdata")) 
e1a_expo_plot <- p + ggtitle("Exponential-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# gamma-uvsd
load(paste0("5_exp1a_ind_Figure_FCacc_x_rating_Model_gamma.rdata")) 
e1a_gamma_plot <- p + ggtitle("Gamma-UVSD") + xlab("2AFC Trial") + theme_smaller_font


# use gridextra to combine 
multi <-
  grid.arrange(e1a_2ht_plot,
               e1a_msd_plot,
               e1a_gumbel_plot,
               e1a_logistic_plot,
               e1a_weibull_plot,
               e1a_lognorm_plot,
               e1a_expo_plot,
               e1a_gamma_plot,
               ncol=4,nrow=2)

ggsave(paste0("6_Figure_Exp1a_other_models_FCacc_multipanel_ind.png"),multi,width=9.5,height=5.50)


## Exp 1b

# 2ht
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_2ht.rdata")) 
e1b_2ht_plot <- p  + ggtitle("2HT") + xlab("2AFC Trial")  + theme_smaller_font

# msd
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_msd.rdata")) 
e1b_msd_plot <- p  + ggtitle("MSD") + xlab("2AFC Trial") + theme_smaller_font

# gumbel-uvsd
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_gumbel.rdata")) 
e1b_gumbel_plot <- p + ggtitle("Gumbel-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# logistic-uvsd
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_logistic.rdata")) 
e1b_logistic_plot <- p + ggtitle("Logistic-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# weibull-uvsd
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_weibull.rdata")) 
e1b_weibull_plot <- p + ggtitle("Weibull-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# lognorm-uvsd
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_lognorm.rdata")) 
e1b_lognorm_plot <- p + ggtitle("Lognormal-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# exponential-uvsd
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_expo.rdata")) 
e1b_expo_plot <- p + ggtitle("Exponential-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# gamma-uvsd
load(paste0("5_exp1b_ind_Figure_FCacc_x_rating_Model_gamma.rdata")) 
e1b_gamma_plot <- p + ggtitle("Gamma-UVSD") + xlab("2AFC Trial") + theme_smaller_font


# use gridextra to combine 
multi <-
  grid.arrange(e1b_2ht_plot,
               e1b_msd_plot,
               e1b_gumbel_plot,
               e1b_logistic_plot,
               e1b_weibull_plot,
               e1b_lognorm_plot,
               e1b_expo_plot,
               e1b_gamma_plot,
               ncol=4,nrow=2)

ggsave(paste0("6_Figure_Exp1b_other_models_FCacc_multipanel_ind.png"),multi,width=9.5,height=5.50)





## Exp 2

# 2ht
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_2ht.rdata")) 
e2_2ht_plot <- p  + ggtitle("2HT") + xlab("2AFC Trial") + theme_smaller_font

# msd
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_msd.rdata")) 
e2_msd_plot <- p  + ggtitle("MSD") + xlab("2AFC Trial") + theme_smaller_font

# gumbel-uvsd
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_gumbel.rdata")) 
e2_gumbel_plot <- p + ggtitle("Gumbel-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# logistic-uvsd
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_logistic.rdata")) 
e2_logistic_plot <- p + ggtitle("Logistic-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# weibull-uvsd
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_weibull.rdata")) 
e2_weibull_plot <- p + ggtitle("Weibull-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# lognorm-uvsd
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_lognorm.rdata")) 
e2_lognorm_plot <- p + ggtitle("Lognormal-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# exponential-uvsd
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_expo.rdata")) 
e2_expo_plot <- p + ggtitle("Exponential-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# gamma-uvsd
load(paste0("5_exp2_ind_Figure_FCacc_x_rating_Model_gamma.rdata")) 
e2_gamma_plot <- p + ggtitle("Gamma-UVSD") + xlab("2AFC Trial") + theme_smaller_font


# use gridextra to combine 
multi <-
  grid.arrange(e2_2ht_plot,
               e2_msd_plot,
               e2_gumbel_plot,
               e2_logistic_plot,
               e2_weibull_plot,
               e2_lognorm_plot,
               e2_expo_plot,
               e2_gamma_plot,
               ncol=4,nrow=2)

ggsave(paste0("6_Figure_Exp2_other_models_FCacc_multipanel_ind.png"),multi,width=9.5,height=5.50)




# Experiment 3

# MSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_msd.rdata"))
e3_confdiff_plot_msd <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("MSD") 

# Gumbel-UVSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_gumbel.rdata"))
e3_confdiff_plot_gumbel <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Gumbel") + ylab("")

# Logistic-UVSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_logistic.rdata"))
e3_confdiff_plot_logistic <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Logistic") + ylab("")

# Weibull-UVSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_weibull.rdata"))
e3_confdiff_plot_weibull <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Weibull") + ylab("")

# Lognormal-UVSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_lognorm.rdata"))
e3_confdiff_plot_lognorm <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Lognormal") + ylab("")

# Exponential-UVSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_expo.rdata"))
e3_confdiff_plot_expo <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Exponential") + ylab("")

# Gamma-UVSD
load(paste0("5_exp3_ind_Figure_eDiffStrength_stimulus_Model_gamma.rdata"))
e3_confdiff_plot_gamma <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Gamma") + ylab("")

# use grid.arrange to combine 
multi <-
  grid.arrange(e3_confdiff_plot_msd,
               e3_confdiff_plot_gumbel,
               e3_confdiff_plot_logistic,
               e3_confdiff_plot_weibull,
               e3_confdiff_plot_lognorm,
               e3_confdiff_plot_expo,
               e3_confdiff_plot_gamma,
               ncol=7,nrow=1)

ggsave(paste0("6_Figure_Exp3_other_models_multipanel_ind.png"),multi,width=8,height=3)




## Figure FC data and other model predictions (agg)------------------------------

## Exp 1a

# 2ht
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_2ht.rdata")) 
e1a_2ht_plot <- p  + ggtitle("2HT") + xlab("2AFC Trial") + theme_smaller_font

# msd
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_msd.rdata")) 
e1a_msd_plot <- p  + ggtitle("MSD") + xlab("2AFC Trial") + theme_smaller_font

# gumbel-uvsd
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_gumbel.rdata")) 
e1a_gumbel_plot <- p + ggtitle("Gumbel-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# logistic-uvsd
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_logistic.rdata")) 
e1a_logistic_plot <- p + ggtitle("Logistic-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# weibull-uvsd
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_weibull.rdata")) 
e1a_weibull_plot <- p + ggtitle("Weibull-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# lognorm-uvsd
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_lognorm.rdata")) 
e1a_lognorm_plot <- p + ggtitle("Lognormal-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# exponential-uvsd
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_expo.rdata")) 
e1a_expo_plot <- p + ggtitle("Expo-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# gamma-uvsd
load(paste0("5_exp1a_agg_Figure_FCacc_x_rating_Model_gamma.rdata")) 
e1a_gamma_plot <- p + ggtitle("Gamma-UVSD") + xlab("2AFC Trial") + theme_smaller_font



# use gridextra to combine 
multi <-
  grid.arrange(e1a_2ht_plot,
               e1a_msd_plot,
               e1a_gumbel_plot,
               e1a_logistic_plot,
               e1a_weibull_plot,
               e1a_lognorm_plot,
               e1a_expo_plot,
               e1a_gamma_plot,
               ncol=4,nrow=2)

ggsave(paste0("6_Figure_Exp1a_other_models_FCacc_multipanel_agg.png"),multi,width=9.5,height=5.50)


## Exp 1b

# 2ht
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_2ht.rdata")) 
e1b_2ht_plot <- p  + ggtitle("2HT") + xlab("2AFC Trial") + theme_smaller_font

# msd
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_msd.rdata")) 
e1b_msd_plot <- p  + ggtitle("MSD") + xlab("2AFC Trial") + theme_smaller_font

# gumbel-uvsd
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_gumbel.rdata")) 
e1b_gumbel_plot <- p + ggtitle("Gumbel-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# logistic-uvsd
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_logistic.rdata")) 
e1b_logistic_plot <- p + ggtitle("Logistic-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# weibull-uvsd
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_weibull.rdata")) 
e1b_weibull_plot <- p + ggtitle("Weibull-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# lognorm-uvsd
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_lognorm.rdata")) 
e1b_lognorm_plot <- p + ggtitle("Lognormal-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# expo-uvsd
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_expo.rdata")) 
e1b_expo_plot <- p + ggtitle("Exponential-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# gamma-uvsd
load(paste0("5_exp1b_agg_Figure_FCacc_x_rating_Model_gamma.rdata")) 
e1b_gamma_plot <- p + ggtitle("Gamma-UVSD") + xlab("2AFC Trial") + theme_smaller_font



# use gridextra to combine 
multi <-
  grid.arrange(e1b_2ht_plot,
               e1b_msd_plot,
               e1b_gumbel_plot,
               e1b_logistic_plot,
               e1b_weibull_plot,
               e1b_lognorm_plot,
               e1b_expo_plot,
               e1b_gamma_plot,
               ncol=4,nrow=2)

ggsave(paste0("6_Figure_Exp1b_other_models_FCacc_multipanel_agg.png"),multi,width=9.5,height=5.50)





## Exp 2

# 2ht
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_2ht.rdata")) 
e2_2ht_plot <- p  + ggtitle("2HT") + xlab("2AFC Trial") + theme_smaller_font

# msd
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_msd.rdata")) 
e2_msd_plot <- p  + ggtitle("MSD") + xlab("2AFC Trial") + theme_smaller_font

# gumbel-uvsd
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_gumbel.rdata")) 
e2_gumbel_plot <- p + ggtitle("Gumbel-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# logistic-uvsd
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_logistic.rdata")) 
e2_logistic_plot <- p + ggtitle("Logistic-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# weibull-uvsd
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_weibull.rdata")) 
e2_weibull_plot <- p + ggtitle("Weibull-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# lognorm-uvsd
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_lognorm.rdata")) 
e2_lognorm_plot <- p + ggtitle("Lognormal-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# exponential-uvsd
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_expo.rdata")) 
e2_expo_plot <- p + ggtitle("Exponential-UVSD") + xlab("2AFC Trial") + theme_smaller_font

# gamma-uvsd
load(paste0("5_exp2_agg_Figure_FCacc_x_rating_Model_gamma.rdata")) 
e2_gamma_plot <- p + ggtitle("Gamma-UVSD") + xlab("2AFC Trial") + theme_smaller_font


# use gridextra to combine 
multi <-
  grid.arrange(e2_2ht_plot,
               e2_msd_plot,
               e2_gumbel_plot,
               e2_logistic_plot,
               e2_weibull_plot,
               e2_lognorm_plot,
               e2_expo_plot,
               e2_gamma_plot,
               ncol=4,nrow=2)

ggsave(paste0("6_Figure_Exp2_other_models_FCacc_multipanel_agg.png"),multi,width=9.5,height=5.50)




# Experiment 3

# MSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_msd.rdata"))
e3_confdiff_plot_msd <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("MSD") 

# Gumbel-UVSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_gumbel.rdata"))
e3_confdiff_plot_gumbel <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Gumbel") + ylab("")

# Logistic-UVSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_logistic.rdata"))
e3_confdiff_plot_logistic <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Logistic") + ylab("")

# Weibull-UVSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_weibull.rdata"))
e3_confdiff_plot_weibull <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Weibull") + ylab("")

# Lognormal-UVSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_lognorm.rdata"))
e3_confdiff_plot_lognorm <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Lognormal") + ylab("")

# Exponential-UVSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_expo.rdata"))
e3_confdiff_plot_expo <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Exponential") + ylab("")

# Gamma-UVSD
load(paste0("5_exp3_agg_Figure_eDiffStrength_stimulus_Model_gamma.rdata"))
e3_confdiff_plot_gamma <- p_diff + geom_text(x=0.94,y=0.5,label="",size=5) + xlab("Gamma") + ylab("")


# use grid.arrange to combine 
multi <-
  grid.arrange(e3_confdiff_plot_msd,
               e3_confdiff_plot_gumbel,
               e3_confdiff_plot_logistic,
               e3_confdiff_plot_weibull,
               e3_confdiff_plot_lognorm,
               e3_confdiff_plot_expo,
               e3_confdiff_plot_gamma,
               ncol=7,nrow=1)

ggsave(paste0("6_Figure_Exp3_other_models_multipanel_agg.png"),multi,width=8,height=3)

