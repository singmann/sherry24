## Chris Berry 2023
##
## Fit evsd, uvsd, dpsd, 2ht, msd, nonGau-uvsd models to simulated data 
## (to check parameter recovery) or fit to real data from Exps 1-3.
## evsd model included for model development purposes (as a check)
## Then use the MLE parameters to derive predicted 2AFC proportion correct 
## (Exps 1a,1b,2) under each model, where the old and new items on a 2AFC trial 
## received the same (remapped) 1-6 Y-N rating. 
## For Exp 3, derive the expected strength vals HCMs and HCCRs (not 2HT model)
##
## Jan/Feb 2024: added exponential and gamma uvsd and ran individually

rm(list = ls()) # clear all

library(tidyverse)
library(truncnorm)
library(evd) # for gumbel
library(crch) # for trunc logistic
library(truncdist) # for trunc weibull
library(sensitivity) # for trunc gumbel   # has conflicts with dplyr::select (Feb 2024)
library(EnvStats) # for trunc log normal


# options: "Nelder-Mead"
FitType    <- "Nelder-Mead" 

wrt_file <- TRUE # write to file?

# set random seed for reproducible code
set.seed(12345)

# User input options, type: 
# "simulated"  = simulate and fit simulated data 
# "exp1a_agg"  = fit exp1a aggregate data
# "exp1a_ind"  = fit exp1a individual data
# "exp1b_agg"  = fit exp1b aggregate data
# "exp1b_ind"  = fit exp1b individual data
# "exp2_agg"   = fit exp2 agg data
# "exp2_ind"   = fit exp2 ind data
# "exp3_agg"   = fit exp3 agg data
# "exp3_ind"   = fit exp3 ind data
# "T&R_2017_e1_agg" = fit aggregate data from T&R Exp 2
# "T&R_2017_e2_agg" = fit aggregate data from T&R Exp 2
#
# To do all datasets (takes a while with all models!):
do_to_fits <- c("simulated",
               "exp1a_agg","exp1a_ind",
               "exp1b_agg","exp1b_ind",
               "exp2_agg","exp2_ind",
               "exp3_agg","exp3_ind",
               "T&R_2017_e1_agg",
               "T&R_2017_e2_agg")
#
# To do selected datasets:
#do_to_fits <- c("simulated")

# User input options, type: 
# "evsd" = fit EVSD model to 6-point YN
# "uvsd" = fit UVSD model to 6-point YN
# "dpsd" = fit DPSD model to 6-point YN
# "2ht"  = fit 2HT  model to 6-point YN
# "msd"  = fit 2HT  model to 6-point YN
# "gumbel" = fit gumbel-uvsd model to 6-point YN
# "logistic" = fit logistic-uvsd to 6-point YN 
# "weibull" = fit weibull-uvsd to 6-point YN
# "lognorm" = fit log-normal-uvsd to 6-point YN
# "gamma" = fit gamma-uvsd to 6-point YN

# To do all models:
do_models <- c("evsd","uvsd","dpsd","2ht","msd",
                "gumbel","logistic","weibull","lognorm","expo","gamma")

# To do selected model
do_models <- c("gamma")


# number optim reps to do
n_opts <- 50

# input: for simulated data only
n_sim_ppts  <- 1 
n_sim_items <- 10000 

# input: for simulation method of deriving 2AFC accuracy
nsim2AFC <- 200000

# names of data files
e1a_dat <- "2_e1a_recog_data_N72.csv"
e1b_dat <- "2_e1b_recog_data_N72.csv"
e2_dat  <- "2_e2_recog_data_N72.csv"
e3_dat  <- "2_e3_recog_data_N72.csv"

# input ids of any non-fittable ppts 
exp1a_ind_nonfittable <- c()
exp1b_ind_nonfittable <- c()
exp2_ind_nonfittable <- c(43,68) # (these ppts gave an error when fitting due to missing responses)
exp3_ind_nonfittable <- c(24) # (missing responses, returns NaN for p-G)


for(model in do_models){
  
  cat(paste0("Fitting model ",model, "...\n"))
  
  # reset random seed for expo and gamma models
  # for reproducible results
  # because run one-by-one for ms revision
  if(model=="expo" | model=="gamma"){set.seed(12345)} 
  
  for(to_fit in do_to_fits){
    
    cat(paste0("   Fitting ", to_fit, " data...\n"))
    
    # get IDs of any nonfittable
    if(str_detect(to_fit,"ind")==TRUE){ 
      nonfittable <- get(paste0(to_fit,"_nonfittable")) 
      }else
      {nonfittable <- c()}

    # input parameters used for simulated data
    if (to_fit == "simulated"){

      
      # Parameters for model simulation
      if (model == "evsd"){
        p <- 
          tibble( ppt  = c(1:n_sim_ppts),
                  dpri = runif(n_sim_ppts,0,2),
                  C1   = runif(n_sim_ppts,-1,0.75),
                  C2   = C1+runif(n_sim_ppts,0.1,0.4),
                  C3   = C2+runif(n_sim_ppts,0.1,0.4),
                  C4   = C3+runif(n_sim_ppts,0.1,0.4),
                  C5   = C4+runif(n_sim_ppts,0.1,0.4) )
      }
      if (model == "uvsd"){
        p <- 
          tibble( ppt  = c(1:n_sim_ppts),
                  muo  = runif(n_sim_ppts,0,2),
                  sigo = runif(n_sim_ppts,0.75,2),
                  C1   = runif(n_sim_ppts,-1,0.75),
                  C2   = C1+runif(n_sim_ppts,0.1,0.4),
                  C3   = C2+runif(n_sim_ppts,0.1,0.4),
                  C4   = C3+runif(n_sim_ppts,0.1,0.4),
                  C5   = C4+runif(n_sim_ppts,0.1,0.4) )
      }else
      if (model == "dpsd"){
        p <- 
          tibble( ppt  = c(1:n_sim_ppts),
                  dpri = runif(n_sim_ppts,0,2),
                  Ro   = runif(n_sim_ppts,0,1), # must be 0 to 1
                  C1   = runif(n_sim_ppts,-1,0.75),
                  C2   = C1+runif(n_sim_ppts,0.1,0.4),
                  C3   = C2+runif(n_sim_ppts,0.1,0.4),
                  C4   = C3+runif(n_sim_ppts,0.1,0.4),
                  C5   = C4+runif(n_sim_ppts,0.1,0.4) )
      }else
      if(model == "2ht"){
        p <- 
          tibble(ppt = c(1:n_sim_ppts),
                 do = runif(n_sim_ppts,0,1),
                 dn = runif(n_sim_ppts,0,1),
                 g1=runif(n_sim_ppts,0,1),# 1=sureN, 6=sureO
                 g2=runif(n_sim_ppts,0,1),
                 g3=runif(n_sim_ppts,0,1),
                 g4=runif(n_sim_ppts,0,1),
                 g5=runif(n_sim_ppts,0,1),
                 g6=runif(n_sim_ppts,0,1)) %>% 
          # g1-6 need to sum to 1; the following does this
          rowwise() %>% 
          mutate(sumg = sum(c_across(cols=c("g1","g2","g3","g4","g5","g6")))) %>% 
          mutate(g1=g1/sumg,
                 g2=g2/sumg,
                 g3=g3/sumg,
                 g4=g4/sumg,
                 g5=g5/sumg,
                 g6=g6/sumg,
                 # fixed 2ht paras in certainty version
                 ro1=0,ro2=0,ro3=0,ro4=0,ro5=0,ro6=1,
                 rn1=1,rn2=0,rn3=0,rn4=0,rn5=0,rn6=0) %>% 
            select(-sumg)
      }else
        if(model == "msd"){
          p <- 
            tibble( ppt  = c(1:n_sim_ppts),
                    dpri = runif(n_sim_ppts,1,2),
                    la   = runif(n_sim_ppts,0,1),
                    C1   = runif(n_sim_ppts,-1,0.75),
                    C2   = C1+runif(n_sim_ppts,0.1,0.4),
                    C3   = C2+runif(n_sim_ppts,0.1,0.4),
                    C4   = C3+runif(n_sim_ppts,0.1,0.4),
                    C5   = C4+runif(n_sim_ppts,0.1,0.4) )
        }else
          if(model == "gumbel"|model == "logistic"){
            p <-
              tibble(ppt  = c(1:n_sim_ppts),
                     loc  = runif(n_sim_ppts,0,2),
                     sca  = runif(n_sim_ppts,0.001,2), 
                     C1   = runif(n_sim_ppts,-1,0.75),
                     C2   = C1+runif(n_sim_ppts,0.1,0.4),
                     C3   = C2+runif(n_sim_ppts,0.1,0.4),
                     C4   = C3+runif(n_sim_ppts,0.1,0.4),
                     C5   = C4+runif(n_sim_ppts,0.1,0.4))
          }else
            if(model == "weibull"){
              p <-
                tibble(ppt  = c(1:n_sim_ppts),
                       sha  = runif(n_sim_ppts,1,3), # vs. new = 3
                       sca  = runif(n_sim_ppts,0.5,3), # vs. new = 1
                       C1   = runif(n_sim_ppts,0,0.75),
                       C2   = C1+runif(n_sim_ppts,0.1,0.4),
                       C3   = C2+runif(n_sim_ppts,0.1,0.4),
                       C4   = C3+runif(n_sim_ppts,0.1,0.4),
                       C5   = C4+runif(n_sim_ppts,0.1,0.4))
            }else
              if(model == "lognorm"){
                p <-
                  tibble(ppt  = c(1:n_sim_ppts),
                         mu  = runif(n_sim_ppts,0,2.5), # vs. new = 0
                         sig  = runif(n_sim_ppts,0,1), # vs. new = 0.25
                         C1   = runif(n_sim_ppts,0,0.75),
                         C2   = C1+runif(n_sim_ppts,0.1,0.4),
                         C3   = C2+runif(n_sim_ppts,0.1,0.4),
                         C4   = C3+runif(n_sim_ppts,0.1,0.4),
                         C5   = C4+runif(n_sim_ppts,0.1,0.4))
              }else
                if(model == "expo"){
                  p <-
                    tibble(ppt  = c(1:n_sim_ppts),
                           rateO= runif(n_sim_ppts,0.01,1), # rate parameter vs. new = 1
                           C1   = runif(n_sim_ppts,0.02,0.75),
                           C2   = C1+runif(n_sim_ppts,0.1,0.1),
                           C3   = C2+runif(n_sim_ppts,0.1,0.1),
                           C4   = C3+runif(n_sim_ppts,0.1,0.1),
                           C5   = C4+runif(n_sim_ppts,0.1,0.1))
                }else
                  if(model == "gamma"){
                    p <-
                      tibble(ppt  = c(1:n_sim_ppts),
                             sha  = runif(n_sim_ppts,1,3), # vs. new = 2
                             sca  = runif(n_sim_ppts,0.5,3), # vs. new = 1
                             C1   = runif(n_sim_ppts,0,0.75),
                             C2   = C1+runif(n_sim_ppts,0.1,0.4),
                             C3   = C2+runif(n_sim_ppts,0.1,0.4),
                             C4   = C3+runif(n_sim_ppts,0.1,0.4),
                             C5   = C4+runif(n_sim_ppts,0.1,0.4))
                  }
    }
    
    
    # Specify n free paras each model
    if (model=="evsd"){model_npar <- 6}else  # dpri,c1,c2,c3,c4,c5
      if (model=="uvsd"){model_npar <- 7}else # muo,sigo,c1,c2,c3,c4,c5
        if (model=="dpsd"){model_npar <- 7}else # dpri,R,c1,c2,c3,c4,c5
            if(model=="2ht"){model_npar <- 7}else  # do,dn,g1,g2,g3,g4,g5
              if(model=="msd"){model_npar <- 7}else # dpri,la,c1,c2,c3,c4,c5
                if(model=="gumbel"){model_npar <- 7}else # loc (old),sca (old),c1,c2,c3,c4,c5
                  if(model=="logistic"){model_npar <- 7}else # loc (old),sca (old),c1,c2,c3,c4,c5
                    if(model=="weibull"){model_npar <- 7}else # shape, scale, c1,c2,c3,c4,c5
                      if(model=="lognorm"){model_npar <- 7}else # mu, sig, c1,c2,c3,c4,c5
                        if(model=="expo"){model_npar <- 6}else # rate, c1,c2,c3,c4,c5
                          if(model=="gamma"){model_npar <- 7} # shape,scale, c1,c2,c3,c4,c5
    
    
    # Functions ----------------------------------------------------------
    
    # Functions to simulate recognition rating for every item under each model
    simulate_data_evsd <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f   = c(rnorm(nitems,dpri,1),rnorm(nitems,0,1))) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    
    simulate_data_uvsd <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f   = c(rnorm(nitems,muo,sigo),rnorm(nitems,0,1))) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    
    simulate_data_dpsd <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f   = c(rnorm(nitems,dpri,1),rnorm(nitems,0,1)), # f from normal
               R   = c(rbinom(nitems,size=1,prob=Ro),rep(0,nitems))) %>%  # R old
        mutate(J = case_when(f<C1 & R==0 ~ 1,
                             f>=C1 & f<C2 & R==0 ~ 2,
                             f>=C2 & f<C3 & R==0 ~ 3,
                             f>=C3 & f<C4 & R==0 ~ 4,
                             f>=C4 & f<C5 & R==0 ~ 5,
                             f>=C5 & R==0 ~ 6,
                             R==1 ~ 6)) 
      return(data)
    }
    
    simulate_data_msd <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               Dist= c(rbinom(nitems,size=1,prob=la),rep(0,nitems)), # att-old=1,non-att=0,new=0
               f   = c(rnorm(nitems,dpri,1),rnorm(nitems,0,1)),
               fmix = rnorm(nitems*2,0,1)) %>% # f for non-att dist
        mutate(f = case_when(Stim==1&Dist==0 ~ fmix, TRUE ~ f),
               J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    

    
    
    simulate_data_2ht <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      # sim data
      # messy but works!
      data <-
        tibble(p,
               J =   c(   rep(1,round(nitems*(do*ro1 + (1-do)*g1))),
                          rep(2,round(nitems*(do*ro2 + (1-do)*g2))),
                          rep(3,round(nitems*(do*ro3 + (1-do)*g3))),
                          rep(4,round(nitems*(do*ro4 + (1-do)*g4))),
                          rep(5,round(nitems*(do*ro5 + (1-do)*g5))),
                          rep(6,round(nitems*(do*ro6 + (1-do)*g6))),
                          rep(1,round(nitems*(dn*rn1 + (1-dn)*g1))),
                          rep(2,round(nitems*(dn*rn2 + (1-dn)*g2))),
                          rep(3,round(nitems*(dn*rn3 + (1-dn)*g3))),
                          rep(4,round(nitems*(dn*rn4 + (1-dn)*g4))),
                          rep(5,round(nitems*(dn*rn5 + (1-dn)*g5))),
                          rep(6,round(nitems*(dn*rn6 + (1-dn)*g6)))),
                  Stim= c(rep(1,round(nitems*(do[1]*ro1[1] + (1-do[1])*g1[1]))),
                          rep(1,round(nitems*(do[1]*ro2[1] + (1-do[1])*g2[1]))),
                          rep(1,round(nitems*(do[1]*ro3[1] + (1-do[1])*g3[1]))),
                          rep(1,round(nitems*(do[1]*ro4[1] + (1-do[1])*g4[1]))),
                          rep(1,round(nitems*(do[1]*ro5[1] + (1-do[1])*g5[1]))),
                          rep(1,round(nitems*(do[1]*ro6[1] + (1-do[1])*g6[1]))),
                          rep(0,round(nitems*(dn[1]*rn1[1] + (1-dn[1])*g1[1]))),
                          rep(0,round(nitems*(dn[1]*rn2[1] + (1-dn[1])*g2[1]))),
                          rep(0,round(nitems*(dn[1]*rn3[1] + (1-dn[1])*g3[1]))),
                          rep(0,round(nitems*(dn[1]*rn4[1] + (1-dn[1])*g4[1]))),
                          rep(0,round(nitems*(dn[1]*rn5[1] + (1-dn[1])*g5[1]))),
                          rep(0,round(nitems*(dn[1]*rn6[1] + (1-dn[1])*g6[1]))))) # 1=old,0=new
      return(data)
    }
    
    simulate_data_gumbel <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      rgum <- c( rgumbel(n=nitems,loc=p$loc,scale=p$sca), 
                 rgumbel(n=nitems,loc=0,scale=1))
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f = rgum ) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    
    simulate_data_logistic <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      rlog <- c( rlogis(n=nitems,location=p$loc,scale=p$sca), 
                 rlogis(n=nitems,location=0,scale=1))
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f = rlog ) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    
    simulate_data_weibull <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      rwei <- c( rweibull(n=nitems,shape=p$sha,scale=p$sca), 
                 rweibull(n=nitems,shape=3,scale=1))
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f = rwei ) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    
    simulate_data_lognorm <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      rlno <- c( rlnorm(n=nitems,meanlog=p$mu,sdlog=p$sig), 
                 rlnorm(n=nitems,meanlog=0,sdlog=0.25))
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f = rlno ) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    simulate_data_expo <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      rexpo <- c( rexp(n=nitems,rate=p$rateO), 
                  rexp(n=nitems,rate=1))
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f = rexpo ) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    simulate_data_gamma <- function(p,ppt,nitems){
      # select paras for that ppt
      p <- p[ppt,]
      
      rgam <- c( rgamma(n=nitems,shape=p$sha,scale=p$sca), 
                 rgamma(n=nitems,shape=2,scale=1)) # new fixed shape=2; scale=1
      
      # sim data
      data <-
        tibble(p,
               Stim= c(rep(1,nitems),rep(0,nitems)), # 1=old,0=new
               f = rgam ) %>% 
        mutate(J = case_when(f<C1~1,
                             f>=C1&f<C2~2,
                             f>=C2&f<C3~3,
                             f>=C3&f<C4~4,
                             f>=C4&f<C5~5,
                             f>=C5~6))
      return(data)
    }
    
    
    # Function for rnd truncated exponential 
    # uses uniform dist to sample between
    # pexp(a) and pexp(b) then transform back using qexp()
    # Neat trick!
    rexpot <- function(n, a, b, rate) {
      
      F.a <- pexp(a, rate = rate)
      F.b <- pexp(b, rate = rate)
      
      u <- runif(n, min = F.a, max = F.b)
      qexp(u, rate = rate)
    }
    
    # Function for rnd truncated gamma 
    rgammat <- function(n, a, b, shape, scale = 1) {
      
      F.a <- pgamma(a, shape = shape, scale = scale)
      F.b <- pgamma(b, shape = shape, scale = scale)
      
      u <- runif(n, min = F.a, max = F.b)
      qgamma(u, shape = shape, scale = scale)
    }
    
    # Function to get summary stats from data
    get_summary_stats <- function(dat){
      summary_stats <- 
        dat %>% 
        group_by(ppt) %>% 
        summarise(n_old = sum(Stim==1),
                  n_new = sum(Stim==0),
                  p_hit = sum(Stim==1 & J>3)/sum(Stim==1),
                  p_fa  = sum(Stim==0 & J>3)/sum(Stim==0),
                  Pr = p_hit - p_fa,
                  N1 = sum(Stim==0&J==1),
                  N2 = sum(Stim==0&J==2),
                  N3 = sum(Stim==0&J==3),
                  N4 = sum(Stim==0&J==4),
                  N5 = sum(Stim==0&J==5),
                  N6 = sum(Stim==0&J==6),
                  O1 = sum(Stim==1&J==1),
                  O2 = sum(Stim==1&J==2),
                  O3 = sum(Stim==1&J==3),
                  O4 = sum(Stim==1&J==4),
                  O5 = sum(Stim==1&J==5),
                  O6 = sum(Stim==1&J==6),
                  zH1=qnorm(sum(J[Stim==1]>=2)/length(J[Stim==1])),
                  zH2=qnorm(sum(J[Stim==1]>=3)/length(J[Stim==1])),
                  zH3=qnorm(sum(J[Stim==1]>=4)/length(J[Stim==1])),
                  zH4=qnorm(sum(J[Stim==1]>=5)/length(J[Stim==1])),
                  zH5=qnorm(sum(J[Stim==1]==6)/length(J[Stim==1])),
                  zF1=qnorm(sum(J[Stim==0]>=2)/length(J[Stim==0])),
                  zF2=qnorm(sum(J[Stim==0]>=3)/length(J[Stim==0])),
                  zF3=qnorm(sum(J[Stim==0]>=4)/length(J[Stim==0])),
                  zF4=qnorm(sum(J[Stim==0]>=5)/length(J[Stim==0])),
                  zF5=qnorm(sum(J[Stim==0]==6)/length(J[Stim==0])),
                  do_zROC=sum(zH1,zH2,zH3,zH4,zH5,zF1,zF2,zF3,zF4,zF5)>-Inf & sum(zH1,zH2,zH3,zH4,zH5,zF1,zF2,zF3,zF4,zF5)<Inf,
                  zROC_slp=ifelse(do_zROC==TRUE, lm(c(zH1,zH2,zH3,zH4,zH5)~c(zF1,zF2,zF3,zF4,zF5))$coefficients[[2]], NA),
                  zROC_int=ifelse(do_zROC==TRUE, lm(c(zH1,zH2,zH3,zH4,zH5)~c(zF1,zF2,zF3,zF4,zF5))$coefficients[[1]], NA)
        ) 
      return(summary_stats)
    }
    
    
    # Function to get evsd mle parameters 
    optfun_evsd <- function(par,x) {
      dpri  <- exp(par[1])
      C1    <- par[2]
      C2    <- C1 + exp(par[3])
      C3    <- C2 + exp(par[4])
      C4    <- C3 + exp(par[5])
      C5    <- C4 + exp(par[6])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(0,dpri)
      sig   <- c(1,1)
      
      likJ  <- x$n*(log(pnorm(cV[x$J+1],mean=mu[x$Stim+1],sd=sig[x$Stim+1]) - 
                          pnorm(cV[x$J],mean=mu[x$Stim+1],sd=sig[x$Stim+1])))
      
      return( -sum(likJ) )
    }
    
    
    # Function to get uvsd mle parameters 
    optfun_uvsd <- function(par,x) {
      muo   <- exp(par[1])
      sigo  <- exp(par[2])
      C1    <- par[3]
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(0,muo)
      sig   <- c(1,sigo)
      
      likJ  <- x$n*(log(pnorm(cV[x$J+1],mean=mu[x$Stim+1],sd=sig[x$Stim+1]) - 
                          pnorm(cV[x$J],mean=mu[x$Stim+1],sd=sig[x$Stim+1])))
    
      return( -sum(likJ) )
    }
    
    
    # Function to get DPSD mle parameters 
    optfun_dpsd <- function(par,x) {
      dpri  <- exp(par[1])
      Ro    <- 1/(1+exp(-par[2])) # ensures Ro between 0 and 1
      C1    <- par[3]
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(0,dpri)
      sig   <- c(1,1)
      rec   <- c(0,0,0,0,0,Ro) # whether rec occurs for each J (only J=6)
      
      # J 1-5 are F based; J 6 are R or F based
      # Pold = R + (1-R)*(1-pnorm(C_old,M=dpri,SD=1))
      likJ <- x$n*(log(rec[x$J]*x$Stim + (1-Ro*x$Stim)*(pnorm(cV[x$J+1],mean=mu[x$Stim+1],sd=sig[x$Stim+1]) - pnorm(cV[x$J],mean=mu[x$Stim+1],sd=sig[x$Stim+1])) ))
      
      return( -sum(likJ) )
    }
    
    # Function to get msd mle parameters 
    optfun_msd <- function(par,x) {
      dpri  <- exp(par[1])
      la    <- 1/(1+exp(-par[2])) # ensures la between 0 and 1
      C1    <- par[3]
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(0,dpri)
      sig   <- c(1,1)
      lambda<- c(0,la)
      
      
      likJ  <- x$n*
        (log(lambda[x$Stim+1]   *(pnorm(cV[x$J+1],mean=mu[x$Stim+1],sd=1) - 
                                pnorm(cV[x$J],mean=mu[x$Stim+1],sd=1)) +
            (1-lambda[x$Stim+1])*(pnorm(cV[x$J+1],mean=0,sd=1) - 
                                pnorm(cV[x$J],mean=0,sd=1))))
      
      return( -sum(likJ) )
    }
    
    
    # Function to get 2ht mle parameters     
    optfun_2ht <- function(par,xo,xn){
      par <- 1/(1+exp(-par))
      
      g <- vector()
      do <- par[1] # 0-1
      dn <- par[2] # 0-1
      g1 <- par[3] # 0-1
      g2 <- par[4] # 0-1
      g3 <- par[5] # 0-1
      g4 <- par[6] # 0-1
      g5 <- par[7] # 0-1
      g6 <- 1-sum(g1,g2,g3,g4,g5) 
      
      rn <- c(1,0,0,0,0,0) # fixed
      ro <- c(0,0,0,0,0,1) # fixed
      
      dx <- c(dn,do)
      g <- c(g1,g2,g3,g4,g5,g6)
      
      # model eqs
      # Pn = Nn*log((dn*rn) + (1-dn)*g)
      # Po = No*log((do*ro) + (1-do)*g)
      likJn <- xn$n*log( ((dx[xn$Stim+1]*rn[xn$J]) + (1-dx[xn$Stim+1])*g[xn$J]) )
      likJo <- xo$n*log( ((dx[xo$Stim+1]*ro[xo$J]) + (1-dx[xo$Stim+1])*g[xo$J]) )
      
      penalty1 <- (sum(g1,g2,g3,g4,g5,g6) - 1)^2 * -1e10 # force g1-g6 to 1 
      penalty2 <- +(g6<0) * -1e10 # force g6 to positive
      
      return(-sum(likJn,likJo,penalty1,penalty2))
    }
    
    
    # Function to get gumbel-UVSD mle parameters 
    optfun_gumbel <- function(par,x) {
      loc   <- exp(par[1])
      sca   <- exp(par[2])
      C1    <- par[3]
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(0,loc)
      sig   <- c(1,sca)
      
      likJ  <- x$n*(log(pgumbel(cV[x$J+1],loc=mu[x$Stim+1],scale=sig[x$Stim+1]) - pgumbel(cV[x$J],loc=mu[x$Stim+1],scale=sig[x$Stim+1])))
      
      return( -sum(likJ) )
    }
    
    
    # Function to get logistic-UVSD mle parameters 
    optfun_logistic <- function(par,x) {
      loc   <- exp(par[1])
      sca   <- exp(par[2])
      C1    <- par[3]
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(0,loc)
      sig   <- c(1,sca)
      
      likJ  <- x$n*(log(plogis(cV[x$J+1],location=mu[x$Stim+1],scale=sig[x$Stim+1]) - plogis(cV[x$J],location=mu[x$Stim+1],scale=sig[x$Stim+1])))
      
      return( -sum(likJ) )
    }
    
    # Function to get weibull-UVSD mle parameters 
    optfun_weibull <- function(par,x) {
      sha   <- exp(par[1])
      sca   <- exp(par[2])
      C1    <- exp(par[3]) # must be +ve
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(3,sha)
      sig   <- c(1,sca)
      
      likJ  <- x$n*(log(pweibull(cV[x$J+1],shape=mu[x$Stim+1],scale=sig[x$Stim+1]) - pweibull(cV[x$J],shape=mu[x$Stim+1],scale=sig[x$Stim+1])))
      
      return( -sum(likJ) )
    }
    
    # Function to get lognormal-UVSD mle parameters 
    optfun_lognorm <- function(par,x) {
      mu   <- exp(par[1])
      sig   <- exp(par[2])
      C1    <- exp(par[3]) # must be +ve
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(-Inf,C1,C2,C3,C4,C5,Inf)
      
      mu    <- c(0,mu)
      sig   <- c(0.25,sig)
      
      likJ  <- x$n*(log(plnorm(cV[x$J+1],meanlog=mu[x$Stim+1],sdlog=sig[x$Stim+1]) - plnorm(cV[x$J],meanlog=mu[x$Stim+1],sdlog=sig[x$Stim+1])))
      
      return( -sum(likJ) )
    }
    
    # Function to get exponential-UVSD mle parameters 
    optfun_expo <- function(par,x) {
      rateO <- exp(par[1])
      C1    <- exp(par[2]) # must be +ve
      C2    <- C1 + exp(par[3])
      C3    <- C2 + exp(par[4])
      C4    <- C3 + exp(par[5])
      C5    <- C4 + exp(par[6])
      
      cV    <- c(0,C1,C2,C3,C4,C5,Inf) # distribution lbound = 0
      
      ra    <- c(1,rateO) # rateN fixed 1 (Swets, 1986)

      likJ  <- x$n*(log(pexp(cV[x$J+1],rate=ra[x$Stim+1]) - pexp(cV[x$J],rate=ra[x$Stim+1])))
      
      penalty <- +(rateO<0) * -1e10 # force rateO to positive; superfluous
      
      return( -sum(likJ,penalty) )
    }
    
    # Function to get gamma-UVSD mle parameters 
    optfun_gamma <- function(par,x) {
      sha   <- exp(par[1])
      sca   <- exp(par[2])
      C1    <- exp(par[3]) # must be +ve
      C2    <- C1 + exp(par[4])
      C3    <- C2 + exp(par[5])
      C4    <- C3 + exp(par[6])
      C5    <- C4 + exp(par[7])
      
      cV    <- c(0,C1,C2,C3,C4,C5,Inf) # distribution lbound=0
      
      mu    <- c(2,sha)
      sig   <- c(1,sca)
      
      likJ  <- x$n*(log(pgamma(cV[x$J+1],shape=mu[x$Stim+1],scale=sig[x$Stim+1]) - pgamma(cV[x$J],shape=mu[x$Stim+1],scale=sig[x$Stim+1])))
      
      return( -sum(likJ) )
    }
    
    
    # Function to simulate 2AFC accuracy for each rating (evsd)
    simFCpcorr_evsd <- function(n,a,b,dpri){
      FCacc <- 
        tibble(o=rtruncnorm(n=n,mean=dpri,sd=1,a=a,b=b),
               n=rtruncnorm(n=n,mean=0,sd=1,a=a,b=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    
    # Function to simulate 2AFC accuracy for each rating (uvsd)
    simFCpcorr_uvsd <- function(n,a,b,Mo,SDo){
      FCacc <- 
        tibble(o=rtruncnorm(n=n,mean=Mo,sd=SDo,a=a,b=b),
               n=rtruncnorm(n=n,mean=0,sd=1,a=a,b=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    # Function to simulate 2AFC accuracy for each rating (msd)
    simFCpcorr_msd <- function(n,a,b,dpri,la){
      FCacc <- 
        tibble(o=c(rtruncnorm(n=round(n*la),mean=dpri,sd=1,a=a,b=b),
                   rtruncnorm(n=n-round(n*la),mean=0,sd=1,a=a,b=b)),
               n=rtruncnorm(n=n,mean=0,sd=1,a=a,b=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    # Function to simulate expected value for high con new rating (msd)
    simExpectVal_msd <- function(n,a,b,dpri,la){
      Eval <- 
        tibble(o=c(rtruncnorm(n=round(n*la),mean=dpri,sd=1,a=a,b=b),
                   rtruncnorm(n=n-round(n*la),mean=0,sd=1,a=a,b=b))) %>% 
        summarise(mean(o))
      
      return(Eval[[1]])
    }
    
    # Function to simulate expected value for high con new rating (gumbel)
    simExpectVal_gumbel <- function(n,a,b,loc,sca){
      EVal <- 
        tibble(i=rgumbel.trunc(n=n,loc=loc,sca=sca,min=a,max=b)) %>% 
        summarise(mean(i))
      
      return(EVal[[1]])
    }
    
    # Function to simulate expected value for high con new rating (logistic)
    simExpectVal_logistic <- function(n,a,b,loc,sca){
      EVal <- 
        tibble(i=rtlogis(n=n,location=loc,scale=sca,left=a,right=b)) %>% 
        summarise(mean(i))
      
      return(EVal[[1]])
    }
    
    # Function to simulate expected value  (weibull)
    simExpectVal_weibull <- function(n,a,b,sha,sca){
      EVal <- 
        tibble(i=rtrunc(n=n,"weibull",shape=sha,scale=sca,a=a,b=b)) %>% 
        summarise(mean(i))
      
      return(EVal[[1]])
    }
    
    # Function to simulate expected value  (lognorm)
    simExpectVal_lognorm <- function(n,a,b,mu,sig){
      EVal <- 
        tibble(i=rlnormTrunc(n=n,meanlog=mu,sdlog=sig,min=a,max=b)) %>% 
        summarise(mean(i))
      
      return(EVal[[1]])
    }
    
    # Function to simulate expected value  (expo)
    simExpectVal_expo <- function(n,a,b,rate){
      EVal <- 
        tibble(i=rexpot(n=n,a=a,b=b,rate=rate)) %>% 
        summarise(mean(i))
      
      return(EVal[[1]])
    }
    
    # Function to simulate expected value  (gamma)
    simExpectVal_gamma <- function(n,a,b,sha,sca){
      EVal <- 
        tibble(i=rgammat(n=n,shape=sha,scale=sca,a=a,b=b)) %>% 
        summarise(mean(i))
      
      return(EVal[[1]])
    }
    
    
    
    # Function to simulate 2AFC accuracy for each rating (dpsd)
    simFCpcorr_dpsd <- function(n,a,b,dpri,Ro){
      # if item is recollected, it's selected as old, else
      # item with greater F is selected.
      
      if(b==Inf){ # YN J=6 - some old items recollected with P Ro (represent as Inf)
        FCacc <- 
          tibble(oR=sample(c(0,Inf), replace=TRUE, size=n, prob=c(1-Ro,Ro)),
                 nR=0,
                 oF=rtruncnorm(n=n,mean=dpri,sd=1,a=a,b=b),
                 nF=rtruncnorm(n=n,mean=0,sd=1,a=a,b=b),
                 o=oR+oF,
                 n=nR+nF) %>% 
          summarise(sum(o > n)/n())
      }else
      if(b<Inf){ # YN J<6 - FC is F based
        FCacc <- 
          tibble(oF=rtruncnorm(n=n,mean=dpri,sd=1,a=a,b=b),
                 nF=rtruncnorm(n=n,mean=0,sd=1,a=a,b=b)) %>% 
          summarise(sum(oF > nF)/n())
      }
      
      return(FCacc[[1]])
    }
    
    # Function to simulate 2AFC accuracy for each rating (gumbel)
    simFCpcorr_gumbel <- function(n,a,b,loc,sca){
      FCacc <- 
        tibble(o=rgumbel.trunc(n=n,loc=loc,sca=sca,min=a,max=b),
               n=rgumbel.trunc(n=n,loc=0,sca=1,min=a,max=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    # Function to simulate 2AFC accuracy for each rating (logistic)
    simFCpcorr_logistic <- function(n,a,b,loc,sca){
      FCacc <- 
        tibble(o=rtlogis(n=n,location=loc,scale=sca,left=a,right=b),
               n=rtlogis(n=n,location=0,scale=1,left=a,right=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    # Function to simulate 2AFC accuracy for each rating (weibull)
    simFCpcorr_weibull <- function(n,a,b,sha,sca){
      FCacc <- 
        tibble(o=rtrunc(n=n,"weibull",shape=sha,scale=sca,a=a,b=b),
               n=rtrunc(n=n,"weibull",shape=3,scale=1,a=a,b=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    
    # Function to simulate 2AFC accuracy for each rating (lognorm)
    simFCpcorr_lognorm <- function(n,a,b,mu,sig){
      FCacc <- 
        tibble(o=rlnormTrunc(n=n,meanlog=mu,sdlog=sig,min=a,max=b),
               n=rlnormTrunc(n=n,meanlog=0,sdlog=0.25,min=a,max=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    # Function to simulate 2AFC accuracy for each rating (expo)
    simFCpcorr_expo <- function(n,a,b,rateO){
      FCacc <- 
        tibble(o=rexpot(n=n,rate=rateO,a=a,b=b),
               n=rexpot(n=n,rate=1,a=a,b=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    # Function to simulate 2AFC accuracy for each rating (gamma)
    simFCpcorr_gamma <- function(n,a,b,sha,sca){
      FCacc <- 
        tibble(o=rgammat(n=n,shape=sha,scale=sca,a=a,b=b),
               n=rgammat(n=n,shape=2,scale=1,a=a,b=b)) %>% 
        summarise(sum(o > n)/n())
      
      return(FCacc[[1]])
    }
    
    
    # function to derive prop correct 2AFC trial using integration (evsd)
    intFCpcorr_evsd <- function(f,a,b,dpri){
      FCacc <- 
        dnorm(f,dpri,1) / (pnorm(b,dpri,1) - pnorm(a,dpri,1)) *
        (pnorm(f,0,1) - pnorm(a,0,1)) / (pnorm(b,0,1) - pnorm(a,0,1))
      
      return(FCacc)
    }
    
    # function to derive prop correct 2AFC trial using integration (uvsd)
    intFCpcorr_uvsd <- function(f,a,b,Mo,SDo){
      FCacc <- 
        dnorm(f,Mo,SDo) / (pnorm(b,Mo,SDo) - pnorm(a,Mo,SDo)) *
        (pnorm(f,0,1) - pnorm(a,0,1)) / (pnorm(b,0,1) - pnorm(a,0,1))
      
      return(FCacc)
    }
    
   
    # function to derive prop correct 2AFC trial via F using 
    # integration in dpsd (same as evsd)
    intFCpcorr_dpsd <- function(f,a,b,dpri,Ro){
      FCacc <- 
          (dnorm(f,dpri,1) / (pnorm(b,dpri,1) - pnorm(a,dpri,1))) *
          ((pnorm(f,0,1) - pnorm(a,0,1)) / (pnorm(b,0,1) - pnorm(a,0,1)))
      return(FCacc)
    }
    
    
    # function to derive pcorrect FC trial in 2ht
    # Certainty version: if d_n, then rating = 1, else guess 1-6. 
    # if d_o then rating = 6, else guess 1-6
    # 2,3,4,& 5 ratings don't arise from do and dn states
    # (handled with the ro and rn parameters)
    #
    # In the Detect Correct model: 1,2,3 ratings can arise from dn state
    # 4,5,6 ratings can arise from do state
    # (handled with the ro and rn parameters)
    FCpcorr_2ht_jn <- function(dn,g,rn){
      (dn*rn + 0.5*(1-dn)*g)/
        (dn*rn + (1-dn)*g)
    }
    
    FCpcorr_2ht_jo <- function(do,g,ro){
      (do*ro + 0.5*(1-do)*g)/
        (do*ro + (1-do)*g)
    }
    
    
    
    # Function to do chi-sq/g-test test on recog 1-6 ratings
    g_test <- function(ob1,ob2,ob3,ob4,ob5,ob6,ob7,ob8,ob9,ob10,ob11,ob12,
                          ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12,n){
      
      fs <-
        tibble(Obs = c(ob1,ob2,ob3,ob4,ob5,ob6,ob7,ob8,ob9,ob10,ob11,ob12),
               Exp = c(ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9,ex10,ex11,ex12) ) %>% 
        filter(Obs > 0) # remove cells with zero obs freq
        
      nfpar <- n
      
      g_test <- 
        tibble(
          ChiSq = sum( (fs$Obs-fs$Exp)^2/fs$Exp ), # Not used
          G = 2*sum(fs$Obs*log(fs$Obs/fs$Exp)),    # Better approximator to Chi-sq dist than Pearson Chi-Sq
          df = 10-nfpar,       # 6 ratings x 2 stimuli = 12 freq, 10 of which are free to vary. 
          pG = 1-pchisq(G,df)     # p value of G
        )
      return(g_test)
    }
    
    
    # Get the data (simulated or empirical)
    # 
    if(to_fit=="simulated"){
      # simulate data according to model
      all_data <- 
        tibble(ppt=map(1:n_sim_ppts, ~ get(paste0("simulate_data_",model))(p,nitems=n_sim_items,ppt=.x))) %>% 
        unnest(cols=c(ppt))
      
    }else
      if(to_fit=="exp1a_agg" | to_fit=="exp1a_ind"){
        # read exp data, and rename cols for fit functions accordingly:
        # ppt=1 to n_ppts, J=1-6, Stim=0(new) or 1(old)
        all_data <- 
          read_csv(e1a_dat,show_col_types=F) %>% 
          mutate(ppt=idx,
                 J=rating16,
                 Stim=abs(cond-2)
          ) %>% 
          select(ppt,J,Stim)
      }else
        if(to_fit=="exp1b_agg" | to_fit=="exp1b_ind"){
          # read exp data, and rename cols for fit functions accordingly:
          # ppt=1 to n_ppts, J=1-6, Stim=0(new) or 1(old)
          all_data <- 
            read_csv(e1b_dat,show_col_types=F) %>% 
            mutate(ppt=idx,
                   J=rating16,
                   Stim=abs(cond-2)
            ) %>% 
            select(ppt,J,Stim)
        }else
          if(to_fit=="exp2_agg" | to_fit=="exp2_ind"){
            # read exp data, and rename cols for fit functions accordingly:
            # ppt=1 to n_ppts, J=1-6, Stim=0(new) or 1(old)
            all_data <- 
              read_csv(e2_dat,show_col_types=F) %>% 
              mutate(ppt=idx,
                     J=rating16,
                     Stim=abs(cond-2)
              ) %>% 
              select(ppt,J,Stim)
          }else
            if(to_fit=="exp3_agg" | to_fit=="exp3_ind"){
              # read exp data, and rename cols for fit functions accordingly:
              # ppt=1 to n_ppts, J=1-6, Stim=0(new) or 1(old)
              all_data <- 
                read_csv(e3_dat,show_col_types=F) %>% 
                mutate(ppt=idx,
                       J=rating16,
                       Stim=abs(cond-2)
                ) %>% 
                select(ppt,J,Stim) 
            }else
              if(to_fit=="T&R_2017_e1_agg"){
                #From R&T2020 Table1
                x<-
                  tibble(Stim=c(rep(0,6),rep(1,6)),
                         J=c(seq(1:6),seq(1:6)),
                         n=c(2158,2813,4362,2631,1460,976,
                             683,1163,2421,2425,2203,5505))
                
                all_data <- x %>% uncount(weights=n) %>% mutate(ppt=1)
                
              }else
                if(to_fit=="T&R_2017_e2_agg"){
                  #From R&T2020 Table1
                  x<-
                    tibble(Stim=c(rep(0,6),rep(1,6)),
                           J=c(seq(1:6),seq(1:6)),
                           n=c(1944,2014,2097,189,352,604,
                               412,619,1056,868,957,3288))
                  
                  all_data <- x %>% uncount(weights=n) %>% mutate(ppt=1)
                  
                }
    
    
    ### Fit the data -----------------------------------------------------------
    
    # create list to store each ppt's info
    pptlist <- list() # parameters and summary data 
    
    if(to_fit=="simulated"){n_fits <- length(unique(all_data$ppt))}else
      if(str_detect(to_fit,"ind")==1){n_fits <- length(unique(all_data$ppt))}else
        if(str_detect(to_fit,"agg")==1){n_fits <- 1}
    
    
    for(fit in c(1:n_fits)){
      
      if(n_fits>1){ # fit ind data
        fitppt <- unique(all_data$ppt)[fit]     # store ppt ID
        cat("     Fitting ppt ", fitppt, ".....\n")  # show message
        dat <- all_data %>% filter(ppt==fitppt) # get ppt data
      } else
        if(n_fits==1){ # fit agg data
           fitppt <- 1
           cat("     Fitting aggregated data...\n") 
           dat <- all_data 
           dat$ppt <- 1
        }
      
      # For deriving starting estimates
      d <- get_summary_stats(dat=dat)
      
      # proceed if ppt is NOT in the nonfittable list
      if(!fitppt %in% nonfittable){ 
        
        ## Calc rough estimates for starting parameters (derived from J, Stim)
        
        if (model=="evsd"){
          par_est<-
            dat %>% 
            summarise(dpri=d$zH3-d$zF3,
                      C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                qnorm(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,-0.9,0.2)),   # wrt new item dist (rough est.)
                      dC2=qnorm(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                      dC3=qnorm(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                      dC4=qnorm(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                      dC5=qnorm(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
            )
        }else
        if (model=="uvsd"){
          par_est<-
            dat %>% 
            summarise(sigo=ifelse(is.na(d$zROC_slp>0)==0, 1/d$zROC_slp, 1),
                      dpri=d$zH3-d$zF3,
                      muo=ifelse((is.na(d$zROC_slp>0)==0)&dpri>0, sqrt( (2/(1+d$zROC_slp^2))*(d$zH3-(d$zROC_slp*d$zF3))), 1), # da (seems to underestimate true mu slightly)
                      C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                    qnorm(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,-0.9,0.2)),   # wrt new item dist (rough est.)
                      dC2=qnorm(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                      dC3=qnorm(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                      dC4=qnorm(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                      dC5=qnorm(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
            )
          
        }else
          if(model=="dpsd"){  
            par_est<-
              dat %>% 
              summarise(dpri=d$zH3-d$zF3,
                        C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                  qnorm(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,-0.9,0.2)),   # wrt new item dist (rough est.)
                        dC2=qnorm(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                        dC3=qnorm(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                        dC4=qnorm(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                        dC5=qnorm(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4),
                        Ro=abs(d$O6/d$n_old - (1-pnorm(C1+dC2+dC3+dC4+dC5,mean=dpri))) # rough est Ro where Ro = data(p(J=6|O)) - starting_para_est(p(J=6|O))
              )
            
          }else
            if (model=="msd"){
              par_est<-
                dat %>% 
                summarise(dpri=d$zH3-d$zF3,
                          la=runif(1,0,1), # random lambda
                          C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                    qnorm(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,-0.9,0.2)),   # wrt new item dist (rough est.)
                          dC2=qnorm(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                          dC3=qnorm(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                          dC4=qnorm(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                          dC5=qnorm(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
                )
            
              
            }else
              if (model=="gumbel"){
                par_est<-
                  dat %>% 
                  summarise(loc=runif(1,0,3), # random
                            sca=runif(1,1,3), # random 
                            C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                      qgumbel(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,-0.9,0.2)),   # wrt new item dist (rough est.)
                            dC2=tryCatch(qgumbel(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1, error=function(err) NA),
                            dC3=tryCatch(qgumbel(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2), error=function(err) NA),
                            dC4=tryCatch(qgumbel(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3), error=function(err) NA),
                            dC5=tryCatch(qgumbel(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4), error=function(err) NA)
                  )
              }else
                if (model=="logistic"){
                  par_est<-
                    dat %>% 
                    summarise(loc=runif(1,0,3), # random
                              sca=runif(1,1,3), # random 
                              C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                        qlogis(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,-0.9,0.2)),   # wrt new item dist (rough est.)
                              dC2=qlogis(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                              dC3=qlogis(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                              dC4=qlogis(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                              dC5=qlogis(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
                    )
                }else
                  if (model=="weibull"){
                    par_est<-
                      dat %>% 
                      summarise(sha=runif(1,3,5), # random
                                sca=runif(1,1,2), # random 
                                C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                          qweibull(shape=3,1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,0,0.3)),   # wrt new item dist (rough est.)
                                dC2=qweibull(shape=3,1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                                dC3=qweibull(shape=3,1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                                dC4=qweibull(shape=3,1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                                dC5=qweibull(shape=3,1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
                      )
                  }else
                    if (model=="lognorm"){
                      par_est<-
                        dat %>% 
                        summarise(mu=runif(1,0,3), # random
                                  sig=runif(1,0.20,1), # random 
                                  C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                            qlnorm(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,0,0.3)),   # wrt new item dist (rough est.)
                                  dC2=qlnorm(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                                  dC3=qlnorm(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                                  dC4=qlnorm(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                                  dC5=qlnorm(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
                        )
                    }else
                      if (model=="expo"){
                        par_est<-
                          dat %>% 
                          summarise(rateO=runif(1,0,1), # random
                                    C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                              qexp(1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,0,0.3)),   # wrt new item dist (rough est.)
                                    dC2=qexp(1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                                    dC3=qexp(1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                                    dC4=qexp(1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                                    dC5=qexp(1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
                          )
                      }else
                        if (model=="gamma"){
                          par_est<-
                            dat %>% 
                            summarise(sha=runif(1,3,5), # random
                                      sca=runif(1,1,2), # random 
                                      C1=ifelse(sum(J[Stim==0]>=2) < length(J[Stim==0]), 
                                                qgamma(shape=2,scale=1,1 - sum(J[Stim==0]>=2)/length(J[Stim==0])),runif(1,0,0.3)),   # wrt new item dist (rough est.)
                                      dC2=qgamma(shape=2,scale=1,1 - sum(J[Stim==0]>=3)/length(J[Stim==0]))-C1,
                                      dC3=qgamma(shape=2,scale=1,1 - sum(J[Stim==0]>=4)/length(J[Stim==0]))-(C1+dC2),
                                      dC4=qgamma(shape=2,scale=1,1 - sum(J[Stim==0]>=5)/length(J[Stim==0]))-(C1+dC2+dC3),
                                      dC5=qgamma(shape=2,scale=1,1 - sum(J[Stim==0]>=6)/length(J[Stim==0]))-(C1+dC2+dC3+dC4)
                            )
                        }
        
        if(model != "2ht"){
          #replace NA dc and Inf
          par_est <-
            par_est %>% 
            mutate(dC2=ifelse((is.na(par_est$dC2)==1)|(par_est$dC2==-Inf)|(par_est$dC2==Inf),0.2,dC2),
                   dC3=ifelse((is.na(par_est$dC3)==1)|(par_est$dC3==-Inf)|(par_est$dC3==Inf),0.2,dC3),
                   dC4=ifelse((is.na(par_est$dC4)==1)|(par_est$dC4==-Inf)|(par_est$dC4==Inf),0.2,dC4),
                   dC5=ifelse((is.na(par_est$dC5)==1)|(par_est$dC5==-Inf)|(par_est$dC5==Inf),0.2,dC5))
        }
        
        # summarise data, store in x
        x <- dat %>% select(Stim,J) %>% group_by(Stim,J) %>% count() # give frequency data (for faster optim later)
        
        ## Fit data
        # Do n opts with diff starting paras to help avoid local maxima/minima, 
        # half with rough starting paras guessed from data where possible, 
        # half with less restraint then pick best fitting opt
        if(model=="evsd"){
          for (optrep in c(1:n_opts)) { # get starting paras
            if(optrep <=n_opts/2){ # use estimated paras
              par_est <- 
                par_est %>% 
                mutate(JITdpri = abs(rnorm(1,mean=dpri,sd=0.1)),  # jitter estimated pars
                       JITC1   = rnorm(1,mean=C1,sd=0.1),
                       JITdC2  = abs(rnorm(1,mean=dC2,sd=0.1)),      
                       JITdC3  = abs(rnorm(1,mean=dC3,sd=0.1)),       
                       JITdC4  = ifelse(dC4<Inf, abs(rnorm(1,mean=dC4,sd=0.1)), runif(1,0.05,0.2)),      
                       JITdC5  = ifelse((dC5<Inf & is.na(dC5)==F), abs(rnorm(1,mean=dC5,sd=0.1)), runif(1,0.05,0.2))
                )}else
                  if(optrep > n_opts/2){ # use random paras 
                    par_est <- 
                      par_est %>% 
                        mutate(JITdpri  = runif(1,0.001,3.5),   
                               JITC1    = runif(1,-0.9,0.2),
                               JITdC2   = runif(1,0.01,0.3),      
                               JITdC3   = runif(1,0.01,0.3),       
                               JITdC4   = runif(1,0.01,0.3),      
                               JITdC5   = runif(1,0.01,0.3))
                    }
            
            pstart <- c( log(par_est$JITdpri),
                         par_est$JITC1,  # no log to allow -ve val of c1
                         log(par_est$JITdC2),
                         log(par_est$JITdC3),
                         log(par_est$JITdC4),
                         log(par_est$JITdC5) )
            
            -optfun_evsd(pstart,x=x)
            
            # first opt
            opt0 <- optim(pstart,optfun_evsd,x=x,
                         method=FitType,
                         control=list(maxit=100000))
            
            # restart optim with BF paras
            # second opt with best fitting paras from opt0
            opt <- optim(opt0$par,optfun_evsd,x=x,
                         method=FitType,
                         control=list(maxit=100000))
            
            if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
              if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
            if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
            
            }
          }else
            if(model=="uvsd"){
              for (optrep in c(1:n_opts)) { # get starting paras
                if(optrep <=n_opts/2){ # use estimated paras
                  par_est <- 
                    par_est %>% 
                    mutate(JITmuo   = ifelse(is.na(muo)==0,abs(rnorm(1,mean=muo,sd=0.1)),runif(1,0.1,2.5)),   # jitter estimated pars
                           JITsigo  = abs(rnorm(1,mean=sigo,sd=0.1)),  
                           JITC1   = rnorm(1,mean=C1,sd=0.1),
                           JITdC2  = abs(rnorm(1,mean=dC2,sd=0.1)),      
                           JITdC3  = abs(rnorm(1,mean=dC3,sd=0.1)),       
                           JITdC4  = ifelse(dC4<Inf, abs(rnorm(1,mean=dC4,sd=0.1)), runif(1,0.05,0.2)),      
                           JITdC5  = ifelse((dC5<Inf & is.na(dC5)==F), abs(rnorm(1,mean=dC5,sd=0.1)), runif(1,0.05,0.2))
                    )}else
                      if(optrep > n_opts/2){ # use random paras 
                        par_est <- 
                          par_est %>% 
                          mutate(JITmuo   = runif(1,0.001,3.5),   
                                 JITsigo  = runif(1,0.6,2.5),  
                                 JITC1    = runif(1,-0.9,0.2),
                                 JITdC2   = runif(1,0.01,0.3),      
                                 JITdC3   = runif(1,0.01,0.3),       
                                 JITdC4   = runif(1,0.01,0.3),      
                                 JITdC5   = runif(1,0.01,0.3))
                      }
                
                pstart <- c( log(par_est$JITmuo),
                             log(par_est$JITsigo),
                             par_est$JITC1,  # no log to allow -ve val c1
                             log(par_est$JITdC2),
                             log(par_est$JITdC3),
                             log(par_est$JITdC4),
                             log(par_est$JITdC5) )
                
                -optfun_uvsd(pstart,x=x)
                opt0 <- optim(pstart,optfun_uvsd,x=x,
                             method=FitType,
                             control=list(maxit=100000))
                
                # restart optim with BF paras
                # second opt with best fitting paras from opt0
                opt <- optim(opt0$par,optfun_uvsd,x=x,
                             method=FitType,
                             control=list(maxit=100000))
                
                if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                  if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                
                }
              }else
                if(model=="dpsd"){
                  for (optrep in c(1:n_opts)) { # get starting paras
                    if(optrep <=n_opts/2){ # use estimated paras
                      par_est <- 
                        par_est %>% 
                        mutate(JITdpri = ifelse(dpri<Inf, abs(rnorm(1,mean=dpri,sd=0.1)), runif(1,0.05,1.8)),   # jitter estimated pars
                               JITRo   = ifelse(is.na(Ro)==0,abs(rnorm(1,mean=Ro,sd=0.1)),runif(1,0,1)),  
                               JITC1   = rnorm(1,mean=C1,sd=0.1),
                               JITdC2  = abs(rnorm(1,mean=dC2,sd=0.1)),      
                               JITdC3  = abs(rnorm(1,mean=dC3,sd=0.1)),       
                               JITdC4  = ifelse(dC4<Inf, abs(rnorm(1,mean=dC4,sd=0.1)), runif(1,0.05,0.2)),      
                               JITdC5  = ifelse((dC5<Inf & is.na(dC5)==F), abs(rnorm(1,mean=dC5,sd=0.1)), runif(1,0.05,0.2))
                        )}else
                          if(optrep > n_opts/2){ # use random paras 
                            par_est <- 
                              par_est %>% 
                              mutate(JITdpri  = runif(1,0.001,3.5),   
                                     JITRo    = runif(1,0,1),  
                                     JITC1    = runif(1,-0.9,0.2),
                                     JITdC2   = runif(1,0.01,0.3),      
                                     JITdC3   = runif(1,0.01,0.3),       
                                     JITdC4   = runif(1,0.01,0.3),      
                                     JITdC5   = runif(1,0.01,0.3))
                          }
                    
                    pstart <- c( log(par_est$JITdpri),
                                 log(par_est$JITRo/(1-par_est$JITRo)), # ensures Ro between 0 and 1
                                 par_est$JITC1,  # no log to allow -ve val
                                 log(par_est$JITdC2),
                                 log(par_est$JITdC3),
                                 log(par_est$JITdC4),
                                 log(par_est$JITdC5) )
                    
                    -optfun_dpsd(pstart,x=x)
                    
                    opt0 <- optim(pstart,optfun_dpsd,x=x,
                                 method=FitType,
                                 control=list(maxit=100000))
                    
                    # restart optim with BF paras
                    # second opt with best fitting paras from opt0
                    opt <- optim(opt0$par,optfun_dpsd,x=x,
                                 method=FitType,
                                 control=list(maxit=100000))
                    
                    if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                      if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                    if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                    
                  }
                }else
                  if(model=="2ht"){
                    for (optrep in c(1:n_opts)) { # get starting paras
                      
                      if(optrep <= n_opts/2){ # base do and dn on PO6, PN1
                        rnddo <- ifelse(d$O6>1,d$O6/d$n_old,runif(1,0,1))
                        rnddn <- ifelse(d$N1>1,d$N1/d$n_new,runif(1,0,1))
                        rndg_tmp  <- runif(6,0,1)
                        rndg  <- rndg_tmp/sum(rndg_tmp)
                        
                        par_est <- c(rnddo,rnddn,rndg[1],rndg[2],rndg[3],rndg[4],rndg[5])
                      }else
                      if(optrep > n_opts/2){ # random
                        rnddo <- runif(1,0,1)
                        rnddn <- runif(1,0,1)
                        rndg_tmp<- runif(6,0.1,0.2) # approx equal
                        rndg  <- rndg_tmp/sum(rndg_tmp)
                        
                        par_est <- c(rnddo,rnddn,rndg[1],rndg[2],rndg[3],rndg[4],rndg[5])
                      }
                      
                      pstart <- log(par_est/(1-par_est)) 

                      xn <- x %>% filter(Stim==0) # separate o and n freqs for faster optim
                      xo <- x %>% filter(Stim==1)
                      
                      -optfun_2ht(pstart,xn=xn,xo=xo)
                      
                      #first opt
                      opt0 <- optim(pstart,optfun_2ht,xn=xn,xo=xo,
                                   method=FitType,
                                   control=list(maxit=100000))
                      
                      # restart with BF paras
                      # second opt with best fitting paras from opt0
                      opt <- optim(opt0$par,optfun_2ht,xn=xn,xo=xo,
                                   method=FitType,
                                   control=list(maxit=100000))
                      
                      if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                        if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                      if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                      
                    }
                  }else
                    if(model=="msd"){
                      for (optrep in c(1:n_opts)) { # get starting paras
                        if(optrep<=n_opts/2){ # use estimated paras
                          par_est <- 
                            par_est %>% 
                            mutate(JITdpri = abs(rnorm(1,mean=dpri,sd=0.1)),   # jitter estimated pars
                                   JITla   = runif(1,0,1),  
                                   JITC1   = rnorm(1,mean=C1,sd=0.1),
                                   JITdC2  = abs(rnorm(1,mean=dC2,sd=0.1)),      
                                   JITdC3  = abs(rnorm(1,mean=dC3,sd=0.1)),       
                                   JITdC4  = ifelse(dC4<Inf, abs(rnorm(1,mean=dC4,sd=0.1)), runif(1,0.05,0.2)),      
                                   JITdC5  = ifelse((dC5<Inf & is.na(dC5)==F), abs(rnorm(1,mean=dC5,sd=0.1)), runif(1,0.05,0.2))
                            )}else
                              if(optrep > n_opts/2){ # use random paras 
                                par_est <- 
                                   
                                  tibble(JITdpri  = runif(1,0.1,3.5),   
                                         JITla    = runif(1,0.1,1),  
                                         JITC1    = runif(1,-0.9,0.2),
                                         JITdC2   = runif(1,0.05,0.3),      
                                         JITdC3   = runif(1,0.05,0.3),       
                                         JITdC4   = runif(1,0.05,0.3),      
                                         JITdC5   = runif(1,0.05,0.3))
                              }
                        
                        pstart <- c( log(par_est$JITdpri),
                                     log(par_est$JITla/(1-par_est$JITla)),
                                     par_est$JITC1,  # no log to allow -ve val c1
                                     log(par_est$JITdC2),
                                     log(par_est$JITdC3),
                                     log(par_est$JITdC4),
                                     log(par_est$JITdC5) )
                        
                        optfun_msd(pstart,x=x)
                        # first opt
                        opt0 <- optim(pstart,optfun_msd,x=x,
                                     method=FitType,
                                     control=list(maxit=100000))
                        
                        # restart with BF paras
                        # second opt with best fitting paras from opt0
                        opt <- optim(opt0$par,optfun_msd,x=x,
                                     method=FitType,
                                     control=list(maxit=100000))
                        
                        if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                          if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                        if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                        
                      }
                    }else
                      if(model=="gumbel"){
                        for (optrep in c(1:n_opts)) { # get starting paras
                          
                          # use random paras 
                          par_est <- 
                            
                            tibble(JITloc   = runif(1,0.1,3.5),   
                                   JITsca   = runif(1,0.8,2),  
                                   JITC1    = runif(1,-0.9,0.2),
                                   JITdC2   = runif(1,0.05,0.3),      
                                   JITdC3   = runif(1,0.05,0.3),       
                                   JITdC4   = runif(1,0.05,0.3),      
                                   JITdC5   = runif(1,0.05,0.3))
                          
                          
                          pstart <- c( log(par_est$JITloc),
                                       log(par_est$JITsca),
                                       par_est$JITC1,  # no log to allow -ve val c1
                                       log(par_est$JITdC2),
                                       log(par_est$JITdC3),
                                       log(par_est$JITdC4),
                                       log(par_est$JITdC5) )
                          
                          optfun_gumbel(pstart,x=x)
                          # first opt
                          opt0 <- optim(pstart,optfun_gumbel,x=x,
                                        method=FitType,
                                        control=list(maxit=100000))
                          
                          # restart with BF paras
                          # second opt with best fitting paras from opt0
                          opt <- optim(opt0$par,optfun_gumbel,x=x,
                                       method=FitType,
                                       control=list(maxit=100000))
                          
                          if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                            if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                          if(length(do_models)==1){ cat("       Optim run = ", optrep, ".....\n")}
                          
                        }
                      }else
                        if(model=="logistic"){
                          for (optrep in c(1:n_opts)) { # get starting paras
                            # use random paras 
                            par_est <- 
                              tibble(JITloc   = runif(1,0.1,3.5),   
                                     JITsca   = runif(1,0.8,2),  
                                     JITC1    = runif(1,-0.9,0.2),
                                     JITdC2   = runif(1,0.05,0.3),      
                                     JITdC3   = runif(1,0.05,0.3),       
                                     JITdC4   = runif(1,0.05,0.3),      
                                     JITdC5   = runif(1,0.05,0.3))
                            
                              
                              pstart <- c( log(par_est$JITloc),
                                           log(par_est$JITsca),
                                           par_est$JITC1,  # no log to allow -ve val c1
                                           log(par_est$JITdC2),
                                           log(par_est$JITdC3),
                                           log(par_est$JITdC4),
                                           log(par_est$JITdC5) )
                              
                              optfun_logistic(pstart,x=x)
                              # first opt
                              opt0 <- optim(pstart,optfun_logistic,x=x,
                                            method=FitType,
                                            control=list(maxit=100000))
                              
                              # restart with BF paras
                              # second opt with best fitting paras from opt0
                              opt <- optim(opt0$par,optfun_logistic,x=x,
                                           method=FitType,
                                           control=list(maxit=100000))
                              
                              if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                                if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                              if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                              
                            }
                        }else
                          if(model=="weibull"){
                            for (optrep in c(1:n_opts)) { # get starting paras
                              
                              # use random paras 
                              par_est <- 
                                tibble(JITsha   = runif(1,0.1,3.5),   
                                       JITsca   = runif(1,0.8,3.5),  
                                       JITC1    = runif(1,0,0.4),
                                       JITdC2   = runif(1,0.05,0.3),      
                                       JITdC3   = runif(1,0.05,0.3),       
                                       JITdC4   = runif(1,0.05,0.3),      
                                       JITdC5   = runif(1,0.05,0.3))
                                
                              
                              pstart <- c( log(par_est$JITsha),
                                           log(par_est$JITsca),
                                           log(par_est$JITC1),  # +ve C1 in weibull
                                           log(par_est$JITdC2),
                                           log(par_est$JITdC3),
                                           log(par_est$JITdC4),
                                           log(par_est$JITdC5) )
                              
                              optfun_weibull(pstart,x=x)
                              # first opt
                              opt0 <- optim(pstart,optfun_weibull,x=x,
                                            method=FitType,
                                            control=list(maxit=100000))
                              
                              # restart with BF paras
                              # second opt with best fitting paras from opt0
                              opt <- optim(opt0$par,optfun_weibull,x=x,
                                           method=FitType,
                                           control=list(maxit=100000))
                              
                              if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                                if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                              if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                              
                            }
                          }else
                            if(model=="lognorm"){
                              for (optrep in c(1:n_opts)) { # get starting paras
                                
                                par_est <- 
                                  tibble(JITmu    = runif(1,0.1,3),   
                                         JITsig   = runif(1,0.2,2.5),  
                                         JITC1    = runif(1,0.05,0.3),
                                         JITdC2   = runif(1,0.05,0.3),      
                                         JITdC3   = runif(1,0.05,0.3),       
                                         JITdC4   = runif(1,0.05,0.3),      
                                         JITdC5   = runif(1,0.05,0.3))
                                
                                
                                pstart <- c( log(par_est$JITmu),
                                             log(par_est$JITsig),
                                             log(par_est$JITC1),  # +ve C1 in lognorm
                                             log(par_est$JITdC2),
                                             log(par_est$JITdC3),
                                             log(par_est$JITdC4),
                                             log(par_est$JITdC5) )
                                
                                optfun_lognorm(pstart,x=x)
                                # first opt
                                opt0 <- optim(pstart,optfun_lognorm,x=x,
                                              method=FitType,
                                              control=list(maxit=100000))
                                
                                # restart with BF paras
                                # second opt with best fitting paras from opt0
                                opt <- optim(opt0$par,optfun_lognorm,x=x,
                                             method=FitType,
                                             control=list(maxit=100000))
                                
                                if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                                  if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                                if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                                
                              }
                            }else
                              if(model=="expo"){
                                for (optrep in c(1:n_opts)) { # get starting paras
                                  
                                  par_est <- 
                                    tibble(JITra    = runif(1,0.01,0.1),   
                                           JITC1    = runif(1,0.05,0.1),
                                           JITdC2   = runif(1,0.05,0.1),      
                                           JITdC3   = runif(1,0.05,0.1),       
                                           JITdC4   = runif(1,0.05,0.1),      
                                           JITdC5   = runif(1,0.05,0.1))
                                  
                                  
                                  pstart <- c( log(par_est$JITra),
                                               log(par_est$JITC1),  # +ve C1 in exp
                                               log(par_est$JITdC2),
                                               log(par_est$JITdC3),
                                               log(par_est$JITdC4),
                                               log(par_est$JITdC5) )
                                  
                                  optfun_lognorm(pstart,x=x)
                                  # first opt
                                  opt0 <- optim(pstart,optfun_expo,x=x,
                                                method=FitType,
                                                control=list(maxit=100000))
                                  
                                  # restart with BF paras
                                  # second opt with best fitting paras from opt0
                                  opt <- optim(opt0$par,optfun_expo,x=x,
                                               method=FitType,
                                               control=list(maxit=100000))
                                  
                                  if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                                    if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                                  if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                                  
                                }
                              }else
                                if(model=="gamma"){
                                  for (optrep in c(1:n_opts)) { # get starting paras
                                    
                                    # use random paras 
                                    par_est <- 
                                      tibble(JITsha   = runif(1,0.1,3.5),   
                                             JITsca   = runif(1,0.8,3.5),  
                                             JITC1    = runif(1,0,0.4),
                                             JITdC2   = runif(1,0.05,0.3),      
                                             JITdC3   = runif(1,0.05,0.3),       
                                             JITdC4   = runif(1,0.05,0.3),      
                                             JITdC5   = runif(1,0.05,0.3))
                                    
                                    
                                    pstart <- c( log(par_est$JITsha),
                                                 log(par_est$JITsca),
                                                 log(par_est$JITC1),  # +ve C1 gamma
                                                 log(par_est$JITdC2),
                                                 log(par_est$JITdC3),
                                                 log(par_est$JITdC4),
                                                 log(par_est$JITdC5) )
                                    
                                    optfun_gamma(pstart,x=x)
                                    # first opt
                                    opt0 <- optim(pstart,optfun_gamma,x=x,
                                                  method=FitType,
                                                  control=list(maxit=100000))
                                    
                                    # restart with BF paras
                                    # second opt with best fitting paras from opt0
                                    opt <- optim(opt0$par,optfun_gamma,x=x,
                                                 method=FitType,
                                                 control=list(maxit=100000))
                                    
                                    if(optrep==1){AllOptReps<-opt} else  # to store multiple ppts
                                      if(optrep>1) {AllOptReps <- rbind(AllOptReps,opt)}
                                    if(length(do_models)==1 & length(do_to_fits)==1){ cat("       Optim run = ", optrep, ".....\n")}
                                    
                                  }
                                }
        
        # select best rep
        bestfit <- which.min( AllOptReps[,2] )  # col 2 = lik val
        opt     <- AllOptReps[bestfit,]         # These are the mle paras
        par     <- opt$par                      # Assign mle paras to par
        
    
        # store fitppt results in f
        if(model == "evsd"){
          f <-
            tibble(fitppt= fitppt,
                   dpri  = exp(par[1]),
                   c1    = par[2],
                   c2    = c1 + exp(par[3]),
                   c3    = c2 + exp(par[4]),
                   c4    = c3 + exp(par[5]),
                   c5    = c4 + exp(par[6]))
          
          mlikpars <- c(log(f$dpri),
                        f$c1,
                        log(f$c2-f$c1),
                        log(f$c3-f$c2),
                        log(f$c4-f$c3),
                        log(f$c5-f$c4))
          
          check_lik   <- -(optfun_evsd(mlikpars,x=x))
        }else
          if(model == "uvsd"){
            f <-
              tibble(fitppt= fitppt,
                     muo   = exp(par[1]),
                     sigo  = exp(par[2]),
                     c1    = par[3],
                     c2    = c1 + exp(par[4]),
                     c3    = c2 + exp(par[5]),
                     c4    = c3 + exp(par[6]),
                     c5    = c4 + exp(par[7]))
            
            mlikpars <- c(log(f$muo),
                          log(f$sigo),
                          f$c1,
                          log(f$c2-f$c1),
                          log(f$c3-f$c2),
                          log(f$c4-f$c3),
                          log(f$c5-f$c4))
            
            check_lik   <- -(optfun_uvsd(mlikpars,x=x))
          }else
            if(model == "dpsd"){
              f <-
                tibble(fitppt= fitppt,
                       dpri  = exp(par[1]),
                       Ro    = 1/(1+exp(-par[2])),
                       c1    = par[3],
                       c2    = c1 + exp(par[4]),
                       c3    = c2 + exp(par[5]),
                       c4    = c3 + exp(par[6]),
                       c5    = c4 + exp(par[7]))
              
              mlikpars <- c(log(f$dpri),
                            log(f$Ro/(1-f$Ro)),
                            f$c1,
                            log(f$c2-f$c1),
                            log(f$c3-f$c2),
                            log(f$c4-f$c3),
                            log(f$c5-f$c4))
              
              check_lik   <- -(optfun_dpsd(mlikpars,x=x))
            }else
              if(model == "2ht"){
                par <- 1/(1+exp(-par)) # 0 to 1
                
                tmpg1 <- par[3]
                tmpg2 <- par[4]
                tmpg3 <- par[5]
                tmpg4 <- par[6]
                tmpg5 <- par[7]
                tmpg6 <- 1 - sum(par[3],par[4],par[5],par[6],par[7])
                
                f <-
                  tibble(fitppt= fitppt,
                         do = par[1],
                         dn = par[2],
                         g1 = tmpg1,
                         g2 = tmpg2,
                         g3 = tmpg3,
                         g4 = tmpg4,
                         g5 = tmpg5,
                         g6 = tmpg6,
                         chk_sumg=sum(g1,g2,g3,g4,g5,g6),
                         ro1= 0,
                         ro2= 0,
                         ro3= 0,
                         ro4= 0,
                         ro5= 0,
                         ro6= 1,
                         rn1= 1,
                         rn2= 0,
                         rn3= 0,
                         rn4= 0,
                         rn5= 0,
                         rn6= 0)
              
                mlikpars <- c(log(f$do/(1-f$do)),
                              log(f$dn/(1-f$dn)),
                              log(f$g1/(1-f$g1)), 
                              log(f$g2/(1-f$g2)), 
                              log(f$g3/(1-f$g3)), 
                              log(f$g4/(1-f$g4)), 
                              log(f$g5/(1-f$g5))) 
                
                check_lik   <- -(optfun_2ht(mlikpars,xn=xn,xo=xo))
              }else
                if(model == "msd"){
                  f <-
                    tibble(fitppt= fitppt,
                           dpri  = exp(par[1]),
                           la    = 1/(1+exp(-par[2])),
                           c1    = par[3],
                           c2    = c1 + exp(par[4]),
                           c3    = c2 + exp(par[5]),
                           c4    = c3 + exp(par[6]),
                           c5    = c4 + exp(par[7]))
                  
                  mlikpars <- c(log(f$dpri),
                                log(f$la/(1-f$la)),
                                f$c1,
                                log(f$c2-f$c1),
                                log(f$c3-f$c2),
                                log(f$c4-f$c3),
                                log(f$c5-f$c4))
                  
                  check_lik   <- -(optfun_msd(mlikpars,x=x))
                }else
                  if(model == "gumbel"){
                    f <-
                      tibble(fitppt= fitppt,
                             loc   = exp(par[1]),
                             sca   = exp(par[2]),
                             c1    = par[3],
                             c2    = c1 + exp(par[4]),
                             c3    = c2 + exp(par[5]),
                             c4    = c3 + exp(par[6]),
                             c5    = c4 + exp(par[7])
                             )
                    
                    mlikpars <- c(log(f$loc),
                                  log(f$sca),
                                  f$c1,
                                  log(f$c2-f$c1),
                                  log(f$c3-f$c2),
                                  log(f$c4-f$c3),
                                  log(f$c5-f$c4))
                    
                    check_lik   <- -(optfun_gumbel(mlikpars,x=x))
                  }else
                     if(model == "logistic"){
                       f <-
                         tibble(fitppt= fitppt,
                                loc   = exp(par[1]),
                                sca   = exp(par[2]),
                                c1    = par[3],
                                c2    = c1 + exp(par[4]),
                                c3    = c2 + exp(par[5]),
                                c4    = c3 + exp(par[6]),
                                c5    = c4 + exp(par[7])
                         )
                        
                        mlikpars <- c(log(f$loc),
                                      log(f$sca),
                                      f$c1,
                                      log(f$c2-f$c1),
                                      log(f$c3-f$c2),
                                      log(f$c4-f$c3),
                                      log(f$c5-f$c4))
                        
                        check_lik   <- -(optfun_logistic(mlikpars,x=x))
                     }else
                       if(model == "weibull"){
                         f <-
                           tibble(fitppt= fitppt,
                                  sha   = exp(par[1]),
                                  sca   = exp(par[2]),
                                  c1    = exp(par[3]),
                                  c2    = c1 + exp(par[4]),
                                  c3    = c2 + exp(par[5]),
                                  c4    = c3 + exp(par[6]),
                                  c5    = c4 + exp(par[7])
                           )
                         
                         mlikpars <- c(log(f$sha),
                                       log(f$sca),
                                       log(f$c1),
                                       log(f$c2-f$c1),
                                       log(f$c3-f$c2),
                                       log(f$c4-f$c3),
                                       log(f$c5-f$c4))
                         
                         check_lik   <- -(optfun_weibull(mlikpars,x=x))
                       }else
                         if(model == "lognorm"){
                           f <-
                             tibble(fitppt= fitppt,
                                    mu    = exp(par[1]),
                                    sig   = exp(par[2]),
                                    c1    = exp(par[3]),
                                    c2    = c1 + exp(par[4]),
                                    c3    = c2 + exp(par[5]),
                                    c4    = c3 + exp(par[6]),
                                    c5    = c4 + exp(par[7])
                             )
                           
                           mlikpars <- c(log(f$mu),
                                         log(f$sig),
                                         log(f$c1),
                                         log(f$c2-f$c1),
                                         log(f$c3-f$c2),
                                         log(f$c4-f$c3),
                                         log(f$c5-f$c4))
                           
                           check_lik   <- -(optfun_lognorm(mlikpars,x=x))
                         }else
                           if(model == "expo"){
                             f <-
                               tibble(fitppt= fitppt,
                                      rateO = exp(par[1]),
                                      c1    = exp(par[2]),
                                      c2    = c1 + exp(par[3]),
                                      c3    = c2 + exp(par[4]),
                                      c4    = c3 + exp(par[5]),
                                      c5    = c4 + exp(par[6])
                               )
                             
                             mlikpars <- c(log(f$rateO),
                                           log(f$c1),
                                           log(f$c2-f$c1),
                                           log(f$c3-f$c2),
                                           log(f$c4-f$c3),
                                           log(f$c5-f$c4))
                             
                             check_lik   <- -(optfun_expo(mlikpars,x=x))
                           }else
                             if(model == "gamma"){
                               f <-
                                 tibble(fitppt= fitppt,
                                        sha   = exp(par[1]),
                                        sca   = exp(par[2]),
                                        c1    = exp(par[3]),
                                        c2    = c1 + exp(par[4]),
                                        c3    = c2 + exp(par[5]),
                                        c4    = c3 + exp(par[6]),
                                        c5    = c4 + exp(par[7])
                                 )
                               
                               mlikpars <- c(log(f$sha),
                                             log(f$sca),
                                             log(f$c1),
                                             log(f$c2-f$c1),
                                             log(f$c3-f$c2),
                                             log(f$c4-f$c3),
                                             log(f$c5-f$c4))
                               
                               check_lik   <- -(optfun_gamma(mlikpars,x=x))
                             }
        
        # general fit info
        f <- 
          f %>% 
          mutate( lglik = (-opt$value),
                  convr = opt$convergence,
                  fn    = opt$counts[[1]],
                  gr    = opt$counts[[2]],
                  dpoints = length(dat$Stim),
                  Nold   = sum(dat$Stim==1),
                  Nnew   = sum(dat$Stim==0),
                  freep  = model_npar,
                  aic    = -2*(-opt$value) + 2*model_npar,
                  bic    = -2*(-opt$value) + model_npar*log(dpoints),
                  bfSim  = bestfit,
                  chk_lik = lglik - check_lik) 
        
        ## Derive expected values 
        #
        if (model=="evsd"){
          e <- 
            tibble( 
              # expected old-new performance
              epH  = 1-pnorm(f$c3,mean=f$dpri,sd=1),
              epF  = 1-pnorm(f$c3,mean=0,sd=1),
              edpri= f$dpri, 
              pN1 = pnorm(f$c1,mean=0,sd=1),
              pN2 = pnorm(f$c2,mean=0,sd=1) - pnorm(f$c1,mean=0,sd=1),
              pN3 = pnorm(f$c3,mean=0,sd=1) - pnorm(f$c2,mean=0,sd=1),
              pN4 = pnorm(f$c4,mean=0,sd=1) - pnorm(f$c3,mean=0,sd=1),
              pN5 = pnorm(f$c5,mean=0,sd=1) - pnorm(f$c4,mean=0,sd=1),
              pN6 = 1 - pnorm(f$c5,mean=0,sd=1),
              pO1 = pnorm(f$c1,mean=f$dpri,sd=1),
              pO2 = pnorm(f$c2,mean=f$dpri,sd=1) - pnorm(f$c1,mean=f$dpri,sd=1),
              pO3 = pnorm(f$c3,mean=f$dpri,sd=1) - pnorm(f$c2,mean=f$dpri,sd=1),
              pO4 = pnorm(f$c4,mean=f$dpri,sd=1) - pnorm(f$c3,mean=f$dpri,sd=1),
              pO5 = pnorm(f$c5,mean=f$dpri,sd=1) - pnorm(f$c4,mean=f$dpri,sd=1),
              pO6 = 1 - pnorm(f$c5,mean=f$dpri,sd=1)
          )
        }else
          if(model == "uvsd"){
            e <- 
              tibble( 
                # expected old-new performance
                epH  = 1-pnorm(f$c3,mean=f$muo,sd=f$sigo),
                epF  = 1-pnorm(f$c3,mean=0,sd=1),
                pN1 = pnorm(f$c1,mean=0,sd=1),
                pN2 = pnorm(f$c2,mean=0,sd=1) - pnorm(f$c1,mean=0,sd=1),
                pN3 = pnorm(f$c3,mean=0,sd=1) - pnorm(f$c2,mean=0,sd=1),
                pN4 = pnorm(f$c4,mean=0,sd=1) - pnorm(f$c3,mean=0,sd=1),
                pN5 = pnorm(f$c5,mean=0,sd=1) - pnorm(f$c4,mean=0,sd=1),
                pN6 = 1 - pnorm(f$c5,mean=0,sd=1),
                pO1 = pnorm(f$c1,mean=f$muo,sd=f$sigo),
                pO2 = pnorm(f$c2,mean=f$muo,sd=f$sigo) - pnorm(f$c1,mean=f$muo,sd=f$sigo),
                pO3 = pnorm(f$c3,mean=f$muo,sd=f$sigo) - pnorm(f$c2,mean=f$muo,sd=f$sigo),
                pO4 = pnorm(f$c4,mean=f$muo,sd=f$sigo) - pnorm(f$c3,mean=f$muo,sd=f$sigo),
                pO5 = pnorm(f$c5,mean=f$muo,sd=f$sigo) - pnorm(f$c4,mean=f$muo,sd=f$sigo),
                pO6 = 1 - pnorm(f$c5,mean=f$muo,sd=f$sigo)
              )
          }else
            if(model == "dpsd"){
              e <- 
                tibble( 
                  # expected old-new performance
                  epH  = f$Ro + (1-f$Ro)*(1-pnorm(f$c3,mean=f$dpri,sd=1)),
                  epF  = 1-pnorm(f$c3,mean=0,sd=1),
                  pN1 = pnorm(f$c1,mean=0,sd=1),
                  pN2 = pnorm(f$c2,mean=0,sd=1) - pnorm(f$c1,mean=0,sd=1),
                  pN3 = pnorm(f$c3,mean=0,sd=1) - pnorm(f$c2,mean=0,sd=1),
                  pN4 = pnorm(f$c4,mean=0,sd=1) - pnorm(f$c3,mean=0,sd=1),
                  pN5 = pnorm(f$c5,mean=0,sd=1) - pnorm(f$c4,mean=0,sd=1),
                  pN6 = 1 - pnorm(f$c5,mean=0,sd=1),
                  pO1 = (1-f$Ro)*pnorm(f$c1,mean=f$dpri,sd=1),
                  pO2 = (1-f$Ro)*(pnorm(f$c2,mean=f$dpri,sd=1) - pnorm(f$c1,mean=f$dpri,sd=1)),
                  pO3 = (1-f$Ro)*(pnorm(f$c3,mean=f$dpri,sd=1) - pnorm(f$c2,mean=f$dpri,sd=1)),
                  pO4 = (1-f$Ro)*(pnorm(f$c4,mean=f$dpri,sd=1) - pnorm(f$c3,mean=f$dpri,sd=1)),
                  pO5 = (1-f$Ro)*(pnorm(f$c5,mean=f$dpri,sd=1) - pnorm(f$c4,mean=f$dpri,sd=1)),
                  pO6 = f$Ro + (1-f$Ro)*(1 - pnorm(f$c5,mean=f$dpri,sd=1))
                )
            }else
              if(model == "2ht"){
                #rn <- c(1,0,0,0,0,0)
                #ro <- c(0,0,0,0,0,1)
                e <- 
                  tibble( 
                    # expected old-new performance
                    epH  = f$do + (1-f$do)*f$g4 + (1-f$do)*f$g5 + (1-f$do)*f$g6,
                    epF  = (1-f$dn)*f$g4 + (1-f$dn)*f$g5 + (1-f$dn)*f$g6,
                    pN1 = f$dn*f$rn1 + (1-f$dn)*f$g1,
                    pN2 = f$dn*f$rn2 + (1-f$dn)*f$g2,
                    pN3 = f$dn*f$rn3 + (1-f$dn)*f$g3,
                    pN4 = f$dn*f$rn4 + (1-f$dn)*f$g4,
                    pN5 = f$dn*f$rn5 + (1-f$dn)*f$g5,
                    pN6 = f$dn*f$rn6 + (1-f$dn)*f$g6,
                    pO1 = f$do*f$ro1 + (1-f$do)*f$g1,
                    pO2 = f$do*f$ro2 + (1-f$do)*f$g2,
                    pO3 = f$do*f$ro3 + (1-f$do)*f$g3,
                    pO4 = f$do*f$ro4 + (1-f$do)*f$g4,
                    pO5 = f$do*f$ro5 + (1-f$do)*f$g5,
                    pO6 = f$do*f$ro6 + (1-f$do)*f$g6
                  )
              }else
                if(model == "msd"){
                  e <- 
                    tibble( 
                      # expected old-new performance
                      epH  = f$la*(1-pnorm(f$c3,mean=f$dpri,sd=1))+
                               (1-f$la)*(1-pnorm(f$c3,mean=0,sd=1)),
                      epF  = 1-pnorm(f$c3,mean=0,sd=1),
                      pN1 = pnorm(f$c1,mean=0,sd=1),
                      pN2 = pnorm(f$c2,mean=0,sd=1) - pnorm(f$c1,mean=0,sd=1),
                      pN3 = pnorm(f$c3,mean=0,sd=1) - pnorm(f$c2,mean=0,sd=1),
                      pN4 = pnorm(f$c4,mean=0,sd=1) - pnorm(f$c3,mean=0,sd=1),
                      pN5 = pnorm(f$c5,mean=0,sd=1) - pnorm(f$c4,mean=0,sd=1),
                      pN6 = 1 - pnorm(f$c5,mean=0,sd=1),
                      pO1 = f$la*pnorm(f$c1,mean=f$dpri,sd=1)+((1-f$la)*pN1),
                      pO2 = (f$la*(pnorm(f$c2,mean=f$dpri,sd=1) - pnorm(f$c1,mean=f$dpri,sd=1)))+((1-f$la)*pN2),
                      pO3 = (f$la*(pnorm(f$c3,mean=f$dpri,sd=1) - pnorm(f$c2,mean=f$dpri,sd=1)))+((1-f$la)*pN3),
                      pO4 = (f$la*(pnorm(f$c4,mean=f$dpri,sd=1) - pnorm(f$c3,mean=f$dpri,sd=1)))+((1-f$la)*pN4),
                      pO5 = (f$la*(pnorm(f$c5,mean=f$dpri,sd=1) - pnorm(f$c4,mean=f$dpri,sd=1)))+((1-f$la)*pN5),
                      pO6 = (f$la*(1 - pnorm(f$c5,mean=f$dpri,sd=1))+((1-f$la)*pN6))
                    )
                }else
                  if(model == "gumbel"){
                    e <- 
                      tibble( 
                        # expected old-new performance
                        epH  = 1-pgumbel(f$c3,loc=f$loc,scale=f$sca),
                        epF  = 1-pgumbel(f$c3,loc=0,scale=1),
                        pN1 = pgumbel(f$c1,loc=0,scale=1),
                        pN2 = pgumbel(f$c2,loc=0,scale=1) - pgumbel(f$c1,loc=0,scale=1),
                        pN3 = pgumbel(f$c3,loc=0,scale=1) - pgumbel(f$c2,loc=0,scale=1),
                        pN4 = pgumbel(f$c4,loc=0,scale=1) - pgumbel(f$c3,loc=0,scale=1),
                        pN5 = pgumbel(f$c5,loc=0,scale=1) - pgumbel(f$c4,loc=0,scale=1),
                        pN6 = 1 - pgumbel(f$c5,loc=0,scale=1),
                        pO1 = pgumbel(f$c1,loc=f$loc,scale=f$sca),
                        pO2 = pgumbel(f$c2,loc=f$loc,scale=f$sca) - pgumbel(f$c1,loc=f$loc,scale=f$sca),
                        pO3 = pgumbel(f$c3,loc=f$loc,scale=f$sca) - pgumbel(f$c2,loc=f$loc,scale=f$sca),
                        pO4 = pgumbel(f$c4,loc=f$loc,scale=f$sca) - pgumbel(f$c3,loc=f$loc,scale=f$sca),
                        pO5 = pgumbel(f$c5,loc=f$loc,scale=f$sca) - pgumbel(f$c4,loc=f$loc,scale=f$sca),
                        pO6 = 1 - pgumbel(f$c5,loc=f$loc,scale=f$sca)
                      )
                  }else
                    if(model == "logistic"){
                      e <- 
                        tibble( 
                          # expected old-new performance
                          epH  = 1-plogis(f$c3,location=f$loc,scale=f$sca),
                          epF  = 1-plogis(f$c3,location=0,scale=1),
                          pN1 = plogis(f$c1,location=0,scale=1),
                          pN2 = plogis(f$c2,location=0,scale=1) - plogis(f$c1,location=0,scale=1),
                          pN3 = plogis(f$c3,location=0,scale=1) - plogis(f$c2,location=0,scale=1),
                          pN4 = plogis(f$c4,location=0,scale=1) - plogis(f$c3,location=0,scale=1),
                          pN5 = plogis(f$c5,location=0,scale=1) - plogis(f$c4,location=0,scale=1),
                          pN6 = 1 - plogis(f$c5,location=0,scale=1),
                          pO1 = plogis(f$c1,location=f$loc,scale=f$sca),
                          pO2 = plogis(f$c2,location=f$loc,scale=f$sca) - plogis(f$c1,location=f$loc,scale=f$sca),
                          pO3 = plogis(f$c3,location=f$loc,scale=f$sca) - plogis(f$c2,location=f$loc,scale=f$sca),
                          pO4 = plogis(f$c4,location=f$loc,scale=f$sca) - plogis(f$c3,location=f$loc,scale=f$sca),
                          pO5 = plogis(f$c5,location=f$loc,scale=f$sca) - plogis(f$c4,location=f$loc,scale=f$sca),
                          pO6 = 1 - plogis(f$c5,location=f$loc,scale=f$sca)
                        )
                    }else
                      if(model == "weibull"){
                        e <- 
                          tibble( 
                            # expected old-new performance
                            epH  = 1-pweibull(f$c3,shape=f$sha,scale=f$sca),
                            epF  = 1-pweibull(f$c3,shape=3,scale=1),
                            pN1 = pweibull(f$c1,shape=3,scale=1),
                            pN2 = pweibull(f$c2,shape=3,scale=1) - pweibull(f$c1,shape=3,scale=1),
                            pN3 = pweibull(f$c3,shape=3,scale=1) - pweibull(f$c2,shape=3,scale=1),
                            pN4 = pweibull(f$c4,shape=3,scale=1) - pweibull(f$c3,shape=3,scale=1),
                            pN5 = pweibull(f$c5,shape=3,scale=1) - pweibull(f$c4,shape=3,scale=1),
                            pN6 = 1 - pweibull(f$c5,shape=3,scale=1),
                            pO1 = pweibull(f$c1,shape=f$sha,scale=f$sca),
                            pO2 = pweibull(f$c2,shape=f$sha,scale=f$sca) - pweibull(f$c1,shape=f$sha,scale=f$sca),
                            pO3 = pweibull(f$c3,shape=f$sha,scale=f$sca) - pweibull(f$c2,shape=f$sha,scale=f$sca),
                            pO4 = pweibull(f$c4,shape=f$sha,scale=f$sca) - pweibull(f$c3,shape=f$sha,scale=f$sca),
                            pO5 = pweibull(f$c5,shape=f$sha,scale=f$sca) - pweibull(f$c4,shape=f$sha,scale=f$sca),
                            pO6 = 1 - pweibull(f$c5,shape=f$sha,scale=f$sca)
                          )
                      }else
                        if(model == "lognorm"){
                          e <- 
                            tibble( 
                              # expected old-new performance
                              epH  = 1-plnorm(f$c3,meanlog=f$mu,sdlog=f$sig),
                              epF  = 1-plnorm(f$c3,meanlog=0,sdlog=0.25),
                              pN1 = plnorm(f$c1,meanlog=0,sdlog=0.25),
                              pN2 = plnorm(f$c2,meanlog=0,sdlog=0.25) - plnorm(f$c1,meanlog=0,sdlog=0.25),
                              pN3 = plnorm(f$c3,meanlog=0,sdlog=0.25) - plnorm(f$c2,meanlog=0,sdlog=0.25),
                              pN4 = plnorm(f$c4,meanlog=0,sdlog=0.25) - plnorm(f$c3,meanlog=0,sdlog=0.25),
                              pN5 = plnorm(f$c5,meanlog=0,sdlog=0.25) - plnorm(f$c4,meanlog=0,sdlog=0.25),
                              pN6 = 1 - plnorm(f$c5,meanlog=0,sdlog=0.25),
                              pO1 = plnorm(f$c1,meanlog=f$mu,sdlog=f$sig),
                              pO2 = plnorm(f$c2,meanlog=f$mu,sdlog=f$sig) - plnorm(f$c1,meanlog=f$mu,sdlog=f$sig),
                              pO3 = plnorm(f$c3,meanlog=f$mu,sdlog=f$sig) - plnorm(f$c2,meanlog=f$mu,sdlog=f$sig),
                              pO4 = plnorm(f$c4,meanlog=f$mu,sdlog=f$sig) - plnorm(f$c3,meanlog=f$mu,sdlog=f$sig),
                              pO5 = plnorm(f$c5,meanlog=f$mu,sdlog=f$sig) - plnorm(f$c4,meanlog=f$mu,sdlog=f$sig),
                              pO6 = 1 - plnorm(f$c5,meanlog=f$mu,sdlog=f$sig)
                            )
                        }else
                          if(model == "expo"){
                            e <- 
                              tibble( 
                                # expected old-new performance
                                epH  = 1-pexp(f$c3,rate=f$rateO),
                                epF  = 1-pexp(f$c3,rate=1),
                                pN1 = pexp(f$c1,rate=1),
                                pN2 = pexp(f$c2,rate=1) - pexp(f$c1,rate=1),
                                pN3 = pexp(f$c3,rate=1) - pexp(f$c2,rate=1),
                                pN4 = pexp(f$c4,rate=1) - pexp(f$c3,rate=1),
                                pN5 = pexp(f$c5,rate=1) - pexp(f$c4,rate=1),
                                pN6 = 1 - pexp(f$c5,rate=1),
                                pO1 = pexp(f$c1,rate=f$rateO),
                                pO2 = pexp(f$c2,rate=f$rateO) - pexp(f$c1,rate=f$rateO),
                                pO3 = pexp(f$c3,rate=f$rateO) - pexp(f$c2,rate=f$rateO),
                                pO4 = pexp(f$c4,rate=f$rateO) - pexp(f$c3,rate=f$rateO),
                                pO5 = pexp(f$c5,rate=f$rateO) - pexp(f$c4,rate=f$rateO),
                                pO6 = 1 - pexp(f$c5,rate=f$rateO)
                              )
                          }else
                            if(model == "gamma"){
                              e <- 
                                tibble( 
                                  # expected old-new performance
                                  epH  = 1-pgamma(f$c3,shape=f$sha,scale=f$sca),
                                  epF  = 1-pgamma(f$c3,shape=2,scale=1),
                                  pN1 = pgamma(f$c1,shape=2,scale=1),
                                  pN2 = pgamma(f$c2,shape=2,scale=1) - pgamma(f$c1,shape=2,scale=1),
                                  pN3 = pgamma(f$c3,shape=2,scale=1) - pgamma(f$c2,shape=2,scale=1),
                                  pN4 = pgamma(f$c4,shape=2,scale=1) - pgamma(f$c3,shape=2,scale=1),
                                  pN5 = pgamma(f$c5,shape=2,scale=1) - pgamma(f$c4,shape=2,scale=1),
                                  pN6 = 1 - pgamma(f$c5,shape=2,scale=1),
                                  pO1 = pgamma(f$c1,shape=f$sha,scale=f$sca),
                                  pO2 = pgamma(f$c2,shape=f$sha,scale=f$sca) - pgamma(f$c1,shape=f$sha,scale=f$sca),
                                  pO3 = pgamma(f$c3,shape=f$sha,scale=f$sca) - pgamma(f$c2,shape=f$sha,scale=f$sca),
                                  pO4 = pgamma(f$c4,shape=f$sha,scale=f$sca) - pgamma(f$c3,shape=f$sha,scale=f$sca),
                                  pO5 = pgamma(f$c5,shape=f$sha,scale=f$sca) - pgamma(f$c4,shape=f$sha,scale=f$sca),
                                  pO6 = 1 - pgamma(f$c5,shape=f$sha,scale=f$sca)
                                )
                            }
        
        # Expected frequencies
        e <- 
          e %>% 
          mutate(eN1 = pN1 * f$Nnew,
                 eN2 = pN2 * f$Nnew,
                 eN3 = pN3 * f$Nnew,
                 eN4 = pN4 * f$Nnew,
                 eN5 = pN5 * f$Nnew,
                 eN6 = pN6 * f$Nnew,
                 eO1 = pO1 * f$Nold,
                 eO2 = pO2 * f$Nold,
                 eO3 = pO3 * f$Nold,
                 eO4 = pO4 * f$Nold,
                 eO5 = pO5 * f$Nold,
                 eO6 = pO6 * f$Nold)
        
        # Add expected values for second recognition task (2AFC or single-item)
        
        if(model == "evsd"){
          e <-
            e %>% 
            mutate( # expected 2AFC accuracy
              # by simulation
              nsim2AFC = nsim2AFC,
              sim_eFCpcor1 = simFCpcorr_evsd(n=nsim2AFC,a=-Inf,b=f$c1,dpri=f$dpri),
              sim_eFCpcor2 = simFCpcorr_evsd(n=nsim2AFC,a=f$c1,b=f$c2,dpri=f$dpri),
              sim_eFCpcor3 = simFCpcorr_evsd(n=nsim2AFC,a=f$c2,b=f$c3,dpri=f$dpri),
              sim_eFCpcor4 = simFCpcorr_evsd(n=nsim2AFC,a=f$c3,b=f$c4,dpri=f$dpri),
              sim_eFCpcor5 = simFCpcorr_evsd(n=nsim2AFC,a=f$c4,b=f$c5,dpri=f$dpri),
              sim_eFCpcor6 = simFCpcorr_evsd(n=nsim2AFC,a=f$c5,b=Inf, dpri=f$dpri),
              # by integration function
              eFCpcor1 = tryCatch(integrate(intFCpcorr_evsd, lower=-Inf,upper=f$c1,dpri=f$dpri,a=-Inf,b=f$c1)$value, error=function(err) NA),
              eFCpcor2 = tryCatch(integrate(intFCpcorr_evsd, lower=f$c1,upper=f$c2,dpri=f$dpri,a=f$c1,b=f$c2)$value, error=function(err) NA),
              eFCpcor3 = tryCatch(integrate(intFCpcorr_evsd, lower=f$c2,upper=f$c3,dpri=f$dpri,a=f$c2,b=f$c3)$value, error=function(err) NA),
              eFCpcor4 = tryCatch(integrate(intFCpcorr_evsd, lower=f$c3,upper=f$c4,dpri=f$dpri,a=f$c3,b=f$c4)$value, error=function(err) NA),
              eFCpcor5 = tryCatch(integrate(intFCpcorr_evsd, lower=f$c4,upper=f$c5,dpri=f$dpri,a=f$c4,b=f$c5)$value, error=function(err) NA),
              eFCpcor6 = tryCatch(integrate(intFCpcorr_evsd, lower=f$c5,upper=Inf, dpri=f$dpri,a=f$c5,b=Inf)$value, error=function(err) NA),
              eStrengthN1 = etruncnorm(a=-Inf, b=f$c1, mean=0, sd=1),
              eStrengthN2 = etruncnorm(a=f$c1, b=f$c2, mean=0, sd=1),
              eStrengthN3 = etruncnorm(a=f$c2, b=f$c3, mean=0, sd=1),
              eStrengthN4 = etruncnorm(a=f$c3, b=f$c4, mean=0, sd=1),
              eStrengthN5 = etruncnorm(a=f$c4, b=f$c5, mean=0, sd=1),
              eStrengthN6 = etruncnorm(a=f$c5, b=Inf,  mean=0, sd=1),
              eStrengthO1 = etruncnorm(a=-Inf, b=f$c1, mean=f$dpri, sd=1),
              eStrengthO2 = etruncnorm(a=f$c1, b=f$c2, mean=f$dpri, sd=1),
              eStrengthO3 = etruncnorm(a=f$c2, b=f$c3, mean=f$dpri, sd=1),
              eStrengthO4 = etruncnorm(a=f$c3, b=f$c4, mean=f$dpri, sd=1),
              eStrengthO5 = etruncnorm(a=f$c4, b=f$c5, mean=f$dpri, sd=1),
              eStrengthO6 = etruncnorm(a=f$c5, b=Inf,  mean=f$dpri, sd=1))
        }else
          if(model == "uvsd"){
            e <- 
              e %>% 
              mutate( # expected 2AFC accuracy
                # using simulation function
                nsim2AFC = nsim2AFC,
                nsim=nsim2AFC,
                sim_eFCpcor1 = simFCpcorr_uvsd(n=nsim2AFC,a=-Inf,b=f$c1,Mo=f$muo,SDo=f$sigo),
                sim_eFCpcor2 = simFCpcorr_uvsd(n=nsim2AFC,a=f$c1,b=f$c2,Mo=f$muo,SDo=f$sigo),
                sim_eFCpcor3 = simFCpcorr_uvsd(n=nsim2AFC,a=f$c2,b=f$c3,Mo=f$muo,SDo=f$sigo),
                sim_eFCpcor4 = simFCpcorr_uvsd(n=nsim2AFC,a=f$c3,b=f$c4,Mo=f$muo,SDo=f$sigo),
                sim_eFCpcor5 = simFCpcorr_uvsd(n=nsim2AFC,a=f$c4,b=f$c5,Mo=f$muo,SDo=f$sigo),
                sim_eFCpcor6 = simFCpcorr_uvsd(n=nsim2AFC,a=f$c5,b=Inf, Mo=f$muo,SDo=f$sigo),
                # using integration function
                eFCpcor1 = tryCatch(integrate(intFCpcorr_uvsd, lower=-Inf,upper=f$c1,Mo=f$muo,SDo=f$sigo,a=-Inf,b=f$c1)$value, error=function(err) NA),
                eFCpcor2 = tryCatch(integrate(intFCpcorr_uvsd, lower=f$c1,upper=f$c2,Mo=f$muo,SDo=f$sigo,a=f$c1,b=f$c2)$value, error=function(err) NA),
                eFCpcor3 = tryCatch(integrate(intFCpcorr_uvsd, lower=f$c2,upper=f$c3,Mo=f$muo,SDo=f$sigo,a=f$c2,b=f$c3)$value, error=function(err) NA),
                eFCpcor4 = tryCatch(integrate(intFCpcorr_uvsd, lower=f$c3,upper=f$c4,Mo=f$muo,SDo=f$sigo,a=f$c3,b=f$c4)$value, error=function(err) NA),
                eFCpcor5 = tryCatch(integrate(intFCpcorr_uvsd, lower=f$c4,upper=f$c5,Mo=f$muo,SDo=f$sigo,a=f$c4,b=f$c5)$value, error=function(err) NA),
                eFCpcor6 = tryCatch(integrate(intFCpcorr_uvsd, lower=f$c5,upper=Inf,Mo=f$muo,SDo=f$sigo, a=f$c5,b=Inf)$value, error=function(err) NA),
                eStrengthN1 = etruncnorm(a=-Inf, b=f$c1, mean=0, sd=1),
                eStrengthN2 = etruncnorm(a=f$c1, b=f$c2, mean=0, sd=1),
                eStrengthN3 = etruncnorm(a=f$c2, b=f$c3, mean=0, sd=1),
                eStrengthN4 = etruncnorm(a=f$c3, b=f$c4, mean=0, sd=1),
                eStrengthN5 = etruncnorm(a=f$c4, b=f$c5, mean=0, sd=1),
                eStrengthN6 = etruncnorm(a=f$c5, b=Inf,  mean=0, sd=1),
                eStrengthO1 = etruncnorm(a=-Inf, b=f$c1, mean=f$muo, sd=f$sigo),
                eStrengthO2 = etruncnorm(a=f$c1, b=f$c2, mean=f$muo, sd=f$sigo),
                eStrengthO3 = etruncnorm(a=f$c2, b=f$c3, mean=f$muo, sd=f$sigo),
                eStrengthO4 = etruncnorm(a=f$c3, b=f$c4, mean=f$muo, sd=f$sigo),
                eStrengthO5 = etruncnorm(a=f$c4, b=f$c5, mean=f$muo, sd=f$sigo),
                eStrengthO6 = etruncnorm(a=f$c5, b=Inf,  mean=f$muo, sd=f$sigo))
          }else
            if(model == "dpsd"){
              e <-
                e %>% 
                mutate( # expected 2AFC accuracy
                  # by simulation
                  nsim2AFC = nsim2AFC,
                  nsim=nsim2AFC,
                  sim_eFCpcor1 = simFCpcorr_dpsd(n=nsim2AFC,a=-Inf,b=f$c1,dpri=f$dpri,Ro=f$Ro),
                  sim_eFCpcor2 = simFCpcorr_dpsd(n=nsim2AFC,a=f$c1,b=f$c2,dpri=f$dpri,Ro=f$Ro),
                  sim_eFCpcor3 = simFCpcorr_dpsd(n=nsim2AFC,a=f$c2,b=f$c3,dpri=f$dpri,Ro=f$Ro),
                  sim_eFCpcor4 = simFCpcorr_dpsd(n=nsim2AFC,a=f$c3,b=f$c4,dpri=f$dpri,Ro=f$Ro),
                  sim_eFCpcor5 = simFCpcorr_dpsd(n=nsim2AFC,a=f$c4,b=f$c5,dpri=f$dpri,Ro=f$Ro),
                  sim_eFCpcor6 = simFCpcorr_dpsd(n=nsim2AFC,a=f$c5,b=Inf, dpri=f$dpri,Ro=f$Ro),
                  # using integration function
                  eFCpcor1 = tryCatch(integrate(intFCpcorr_dpsd, lower=-Inf,upper=f$c1,dpri=f$dpri,Ro=f$Ro,a=-Inf,b=f$c1)$value, error=function(err) NA),
                  eFCpcor2 = tryCatch(integrate(intFCpcorr_dpsd, lower=f$c1,upper=f$c2,dpri=f$dpri,Ro=f$Ro,a=f$c1,b=f$c2)$value, error=function(err) NA),
                  eFCpcor3 = tryCatch(integrate(intFCpcorr_dpsd, lower=f$c2,upper=f$c3,dpri=f$dpri,Ro=f$Ro,a=f$c2,b=f$c3)$value, error=function(err) NA),
                  eFCpcor4 = tryCatch(integrate(intFCpcorr_dpsd, lower=f$c3,upper=f$c4,dpri=f$dpri,Ro=f$Ro,a=f$c3,b=f$c4)$value, error=function(err) NA),
                  eFCpcor5 = tryCatch(integrate(intFCpcorr_dpsd, lower=f$c4,upper=f$c5,dpri=f$dpri,Ro=f$Ro,a=f$c4,b=f$c5)$value, error=function(err) NA),
                  eFCpcor6 = tryCatch(f$Ro + (1-f$Ro)*integrate(intFCpcorr_dpsd, lower=f$c5,upper=Inf,dpri=f$dpri,Ro=f$Ro,a=f$c5,b=Inf)$value, error=function(err) NA), # CB analytic solution
                  eStrengthN1 = etruncnorm(a=-Inf, b=f$c1, mean=0, sd=1),
                  eStrengthN2 = etruncnorm(a=f$c1, b=f$c2, mean=0, sd=1),
                  eStrengthN3 = etruncnorm(a=f$c2, b=f$c3, mean=0, sd=1),
                  eStrengthN4 = etruncnorm(a=f$c3, b=f$c4, mean=0, sd=1),
                  eStrengthN5 = etruncnorm(a=f$c4, b=f$c5, mean=0, sd=1),
                  eStrengthN6 = etruncnorm(a=f$c5, b=Inf,  mean=0, sd=1),
                  eStrengthO1 = etruncnorm(a=-Inf, b=f$c1, mean=f$dpri, sd=1),
                  eStrengthO2 = etruncnorm(a=f$c1, b=f$c2, mean=f$dpri, sd=1),
                  eStrengthO3 = etruncnorm(a=f$c2, b=f$c3, mean=f$dpri, sd=1),
                  eStrengthO4 = etruncnorm(a=f$c3, b=f$c4, mean=f$dpri, sd=1),
                  eStrengthO5 = etruncnorm(a=f$c4, b=f$c5, mean=f$dpri, sd=1),
                  eStrengthO6 = etruncnorm(a=f$c5, b=Inf,  mean=f$dpri, sd=1)) # expected familiarity (not taking into account R)
            }else
              if(model == "2ht"){
                e <- 
                  e %>% 
                  mutate( # expected 2AFC accuracy (by equations)
                    eFCpcor1 = FCpcorr_2ht_jn(dn=f$dn,g=f$g1,rn=f$rn1),
                    eFCpcor2 = FCpcorr_2ht_jn(dn=f$dn,g=f$g2,rn=f$rn2),
                    eFCpcor3 = FCpcorr_2ht_jn(dn=f$dn,g=f$g3,rn=f$rn3),
                    eFCpcor4 = FCpcorr_2ht_jo(do=f$do,g=f$g4,ro=f$ro4),
                    eFCpcor5 = FCpcorr_2ht_jo(do=f$do,g=f$g5,ro=f$ro5),
                    eFCpcor6 = FCpcorr_2ht_jo(do=f$do,g=f$g6,ro=f$ro6))
              }else
                if(model == "msd"){
                  e <- 
                    e %>% 
                    mutate( # expected 2AFC accuracy
                      # using simulation function
                      nsim2AFC = nsim2AFC,
                      nsim=nsim2AFC,
                      sim_eFCpcor1 = simFCpcorr_msd(n=nsim2AFC,a=-Inf,b=f$c1,dpri=f$dpri,la=f$la),
                      sim_eFCpcor2 = simFCpcorr_msd(n=nsim2AFC,a=f$c1,b=f$c2,dpri=f$dpri,la=f$la),
                      sim_eFCpcor3 = simFCpcorr_msd(n=nsim2AFC,a=f$c2,b=f$c3,dpri=f$dpri,la=f$la),
                      sim_eFCpcor4 = simFCpcorr_msd(n=nsim2AFC,a=f$c3,b=f$c4,dpri=f$dpri,la=f$la),
                      sim_eFCpcor5 = simFCpcorr_msd(n=nsim2AFC,a=f$c4,b=f$c5,dpri=f$dpri,la=f$la),
                      sim_eFCpcor6 = simFCpcorr_msd(n=nsim2AFC,a=f$c5,b=Inf, dpri=f$dpri,la=f$la),
                      # using integration function (not formulated, so can't do)
                      #eFCpcor1 = tryCatch(integrate(intFCpcorr_msd, lower=-Inf,upper=f$c1,dpri=f$dpri,la=f$la,a=-Inf,b=f$c1)$value, error=function(err) NA),
                      #eFCpcor2 = tryCatch(integrate(intFCpcorr_msd, lower=f$c1,upper=f$c2,dpri=f$dpri,la=f$la,a=f$c1,b=f$c2)$value, error=function(err) NA),
                      #eFCpcor3 = tryCatch(integrate(intFCpcorr_msd, lower=f$c2,upper=f$c3,dpri=f$dpri,la=f$la,a=f$c2,b=f$c3)$value, error=function(err) NA),
                      #eFCpcor4 = tryCatch(integrate(intFCpcorr_msd, lower=f$c3,upper=f$c4,dpri=f$dpri,la=f$la,a=f$c3,b=f$c4)$value, error=function(err) NA),
                      #eFCpcor5 = tryCatch(integrate(intFCpcorr_msd, lower=f$c4,upper=f$c5,dpri=f$dpri,la=f$la,a=f$c4,b=f$c5)$value, error=function(err) NA),
                      #eFCpcor6 = tryCatch(integrate(intFCpcorr_msd, lower=f$c5,upper=Inf,dpri=f$dpri,la=f$la, a=f$c5,b=Inf)$value, error=function(err) NA))
                      eStrengthN1 = etruncnorm(a=-Inf, b=f$c1, mean=0, sd=1),
                      eStrengthN2 = etruncnorm(a=f$c1, b=f$c2, mean=0, sd=1),
                      eStrengthN3 = etruncnorm(a=f$c2, b=f$c3, mean=0, sd=1),
                      eStrengthN4 = etruncnorm(a=f$c3, b=f$c4, mean=0, sd=1),
                      eStrengthN5 = etruncnorm(a=f$c4, b=f$c5, mean=0, sd=1),
                      eStrengthN6 = etruncnorm(a=f$c5, b=Inf,  mean=0, sd=1),
                      eStrengthO1 = simExpectVal_msd(n=nsim,a=-Inf,b=f$c1,dpri=f$dpri,la=f$la),
                      eStrengthO2 = simExpectVal_msd(n=nsim,a=f$c1,b=f$c2,dpri=f$dpri,la=f$la),
                      eStrengthO3 = simExpectVal_msd(n=nsim,a=f$c2,b=f$c3,dpri=f$dpri,la=f$la),
                      eStrengthO4 = simExpectVal_msd(n=nsim,a=f$c3,b=f$c4,dpri=f$dpri,la=f$la),
                      eStrengthO5 = simExpectVal_msd(n=nsim,a=f$c4,b=f$c5,dpri=f$dpri,la=f$la),
                      eStrengthO6 = simExpectVal_msd(n=nsim,a=f$c5,b=Inf,dpri=f$dpri,la=f$la)) 
                }else
                  if(model == "gumbel"){
                    e <- 
                      e %>% 
                      mutate( # expected 2AFC accuracy
                        # using simulation function
                        nsim2AFC = nsim2AFC,
                        nsim=nsim2AFC,
                        sim_eFCpcor1 = tryCatch(simFCpcorr_gumbel(n=nsim2AFC,a=-Inf,b=f$c1,loc=f$loc,sca=f$sca),error=function(err) NA),
                        sim_eFCpcor2 = tryCatch(simFCpcorr_gumbel(n=nsim2AFC,a=f$c1,b=f$c2,loc=f$loc,sca=f$sca),error=function(err) NA),
                        sim_eFCpcor3 = tryCatch(simFCpcorr_gumbel(n=nsim2AFC,a=f$c2,b=f$c3,loc=f$loc,sca=f$sca),error=function(err) NA),
                        sim_eFCpcor4 = tryCatch(simFCpcorr_gumbel(n=nsim2AFC,a=f$c3,b=f$c4,loc=f$loc,sca=f$sca),error=function(err) NA),
                        sim_eFCpcor5 = tryCatch(simFCpcorr_gumbel(n=nsim2AFC,a=f$c4,b=f$c5,loc=f$loc,sca=f$sca),error=function(err) NA),
                        sim_eFCpcor6 = tryCatch(simFCpcorr_gumbel(n=nsim2AFC,a=f$c5,b=Inf, loc=f$loc,sca=f$sca),error=function(err) NA),
                        # using integration function (not formulated, so can't do)
                        #eFCpcor1 = tryCatch(integrate(intFCpcorr_gumbel, lower=-Inf,upper=f$c1,loc=f$loc,sca=f$sca,a=-Inf,b=f$c1)$value, error=function(err) NA),
                        #eFCpcor2 = tryCatch(integrate(intFCpcorr_gumbel, lower=f$c1,upper=f$c2,loc=f$loc,sca=f$sca,a=f$c1,b=f$c2)$value, error=function(err) NA),
                        #eFCpcor3 = tryCatch(integrate(intFCpcorr_gumbel, lower=f$c2,upper=f$c3,loc=f$loc,sca=f$sca,a=f$c2,b=f$c3)$value, error=function(err) NA),
                        #eFCpcor4 = tryCatch(integrate(intFCpcorr_gumbel, lower=f$c3,upper=f$c4,loc=f$loc,sca=f$sca,a=f$c3,b=f$c4)$value, error=function(err) NA),
                        #eFCpcor5 = tryCatch(integrate(intFCpcorr_gumbel, lower=f$c4,upper=f$c5,loc=f$loc,sca=f$sca,a=f$c4,b=f$c5)$value, error=function(err) NA),
                        #eFCpcor6 = tryCatch(integrate(intFCpcorr_gumbel, lower=f$c5,upper=Inf,loc=f$loc,sca=f$sca, a=f$c5,b=Inf)$value, error=function(err) NA))
                        eStrengthN1 = tryCatch(simExpectVal_gumbel(n=nsim,a=-Inf,b=f$c1,loc=0,sca=1),error=function(err) NA),
                        eStrengthN2 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c1,b=f$c2,loc=0,sca=1),error=function(err) NA),
                        eStrengthN3 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c2,b=f$c3,loc=0,sca=1),error=function(err) NA),
                        eStrengthN4 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c3,b=f$c4,loc=0,sca=1),error=function(err) NA),
                        eStrengthN5 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c4,b=f$c5,loc=0,sca=1),error=function(err) NA),
                        eStrengthN6 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c5,b=Inf,loc=0,sca=1),error=function(err) NA),
                        eStrengthO1 = tryCatch(simExpectVal_gumbel(n=nsim,a=-Inf,b=f$c1,loc=f$loc,sca=f$sca),error=function(err) NA),
                        eStrengthO2 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c1,b=f$c2,loc=f$loc,sca=f$sca),error=function(err) NA),
                        eStrengthO3 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c2,b=f$c3,loc=f$loc,sca=f$sca),error=function(err) NA),
                        eStrengthO4 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c3,b=f$c4,loc=f$loc,sca=f$sca),error=function(err) NA),
                        eStrengthO5 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c4,b=f$c5,loc=f$loc,sca=f$sca),error=function(err) NA),
                        eStrengthO6 = tryCatch(simExpectVal_gumbel(n=nsim,a=f$c5,b=Inf,loc=f$loc,sca=f$sca),error=function(err) NA))
                  }else
                    if(model == "logistic"){
                      e <- 
                        e %>% 
                        mutate( # expected 2AFC accuracy
                          # using simulation function
                          nsim2AFC = nsim2AFC,
                          nsim=nsim2AFC,
                          sim_eFCpcor1 = tryCatch(simFCpcorr_logistic(n=nsim2AFC,a=-Inf,b=f$c1,loc=f$loc,sca=f$sca),error=function(err) NA),
                          sim_eFCpcor2 = tryCatch(simFCpcorr_logistic(n=nsim2AFC,a=f$c1,b=f$c2,loc=f$loc,sca=f$sca),error=function(err) NA),
                          sim_eFCpcor3 = tryCatch(simFCpcorr_logistic(n=nsim2AFC,a=f$c2,b=f$c3,loc=f$loc,sca=f$sca),error=function(err) NA),
                          sim_eFCpcor4 = tryCatch(simFCpcorr_logistic(n=nsim2AFC,a=f$c3,b=f$c4,loc=f$loc,sca=f$sca),error=function(err) NA),
                          sim_eFCpcor5 = tryCatch(simFCpcorr_logistic(n=nsim2AFC,a=f$c4,b=f$c5,loc=f$loc,sca=f$sca),error=function(err) NA),
                          sim_eFCpcor6 = tryCatch(simFCpcorr_logistic(n=nsim2AFC,a=f$c5,b=Inf, loc=f$loc,sca=f$sca),error=function(err) NA),
                          # using integration function (not formulated, so can't do)
                          #eFCpcor1 = tryCatch(integrate(intFCpcorr_logistic, lower=-Inf,upper=f$c1,loc=f$loc,sca=f$sca,a=-Inf,b=f$c1)$value, error=function(err) NA),
                          #eFCpcor2 = tryCatch(integrate(intFCpcorr_logistic, lower=f$c1,upper=f$c2,loc=f$loc,sca=f$sca,a=f$c1,b=f$c2)$value, error=function(err) NA),
                          #eFCpcor3 = tryCatch(integrate(intFCpcorr_logistic, lower=f$c2,upper=f$c3,loc=f$loc,sca=f$sca,a=f$c2,b=f$c3)$value, error=function(err) NA),
                          #eFCpcor4 = tryCatch(integrate(intFCpcorr_logistic, lower=f$c3,upper=f$c4,loc=f$loc,sca=f$sca,a=f$c3,b=f$c4)$value, error=function(err) NA),
                          #eFCpcor5 = tryCatch(integrate(intFCpcorr_logistic, lower=f$c4,upper=f$c5,loc=f$loc,sca=f$sca,a=f$c4,b=f$c5)$value, error=function(err) NA),
                          #eFCpcor6 = tryCatch(integrate(intFCpcorr_logistic, lower=f$c5,upper=Inf,loc=f$loc,sca=f$sca, a=f$c5,b=Inf)$value, error=function(err) NA))
                          eStrengthN1 = tryCatch(simExpectVal_logistic(n=nsim,a=-Inf,b=f$c1,loc=0,sca=1),error=function(err) NA),
                          eStrengthN2 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c1,b=f$c2,loc=0,sca=1),error=function(err) NA),
                          eStrengthN3 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c2,b=f$c3,loc=0,sca=1),error=function(err) NA),
                          eStrengthN4 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c3,b=f$c4,loc=0,sca=1),error=function(err) NA),
                          eStrengthN5 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c4,b=f$c5,loc=0,sca=1),error=function(err) NA),
                          eStrengthN6 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c5,b=Inf,loc=0,sca=1),error=function(err) NA),
                          eStrengthO1 = tryCatch(simExpectVal_logistic(n=nsim,a=-Inf,b=f$c1,loc=f$loc,sca=f$sca),error=function(err) NA),
                          eStrengthO2 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c1,b=f$c2,loc=f$loc,sca=f$sca),error=function(err) NA),
                          eStrengthO3 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c2,b=f$c3,loc=f$loc,sca=f$sca),error=function(err) NA),
                          eStrengthO4 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c3,b=f$c4,loc=f$loc,sca=f$sca),error=function(err) NA),
                          eStrengthO5 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c4,b=f$c5,loc=f$loc,sca=f$sca),error=function(err) NA),
                          eStrengthO6 = tryCatch(simExpectVal_logistic(n=nsim,a=f$c5,b=Inf,loc=f$loc,sca=f$sca),error=function(err) NA))
                    }else
                      if(model == "weibull"){
                        e <- 
                          e %>% 
                          mutate( # expected 2AFC accuracy
                            # using simulation function
                            nsim2AFC = nsim2AFC,
                            nsim=nsim2AFC,
                            sim_eFCpcor1 = tryCatch(simFCpcorr_weibull(n=nsim2AFC,a=-Inf,b=f$c1,sha=f$sha,sca=f$sca),error=function(err) NA),
                            sim_eFCpcor2 = tryCatch(simFCpcorr_weibull(n=nsim2AFC,a=f$c1,b=f$c2,sha=f$sha,sca=f$sca),error=function(err) NA),
                            sim_eFCpcor3 = tryCatch(simFCpcorr_weibull(n=nsim2AFC,a=f$c2,b=f$c3,sha=f$sha,sca=f$sca),error=function(err) NA),
                            sim_eFCpcor4 = tryCatch(simFCpcorr_weibull(n=nsim2AFC,a=f$c3,b=f$c4,sha=f$sha,sca=f$sca),error=function(err) NA),
                            sim_eFCpcor5 = tryCatch(simFCpcorr_weibull(n=nsim2AFC,a=f$c4,b=f$c5,sha=f$sha,sca=f$sca),error=function(err) NA),
                            sim_eFCpcor6 = tryCatch(simFCpcorr_weibull(n=nsim2AFC,a=f$c5,b=Inf, sha=f$sha,sca=f$sca),error=function(err) NA),
                            # using integration function (not formulated, so can't do)
                            #eFCpcor1 = tryCatch(integrate(intFCpcorr_weibull, lower=-Inf,upper=f$c1,sha=f$sha,sca=f$sca,a=-Inf,b=f$c1)$value, error=function(err) NA),
                            #eFCpcor2 = tryCatch(integrate(intFCpcorr_weibull, lower=f$c1,upper=f$c2,sha=f$sha,sca=f$sca,a=f$c1,b=f$c2)$value, error=function(err) NA),
                            #eFCpcor3 = tryCatch(integrate(intFCpcorr_weibull, lower=f$c2,upper=f$c3,sha=f$sha,sca=f$sca,a=f$c2,b=f$c3)$value, error=function(err) NA),
                            #eFCpcor4 = tryCatch(integrate(intFCpcorr_weibull, lower=f$c3,upper=f$c4,sha=f$sha,sca=f$sca,a=f$c3,b=f$c4)$value, error=function(err) NA),
                            #eFCpcor5 = tryCatch(integrate(intFCpcorr_weibull, lower=f$c4,upper=f$c5,sha=f$sha,sca=f$sca,a=f$c4,b=f$c5)$value, error=function(err) NA),
                            #eFCpcor6 = tryCatch(integrate(intFCpcorr_weibull, lower=f$c5,upper=Inf,sha=f$sha,sca=f$sca, a=f$c5,b=Inf)$value, error=function(err) NA))
                            eStrengthN1 = tryCatch(simExpectVal_weibull(n=nsim,a=-Inf,b=f$c1,sha=3,sca=1),error=function(err) NA),
                            eStrengthN2 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c1,b=f$c2,sha=3,sca=1),error=function(err) NA),
                            eStrengthN3 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c2,b=f$c3,sha=3,sca=1),error=function(err) NA),
                            eStrengthN4 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c3,b=f$c4,sha=3,sca=1),error=function(err) NA),
                            eStrengthN5 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c4,b=f$c5,sha=3,sca=1),error=function(err) NA),
                            eStrengthN6 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c5,b=Inf,sha=3,sca=1),error=function(err) NA),
                            eStrengthO1 = tryCatch(simExpectVal_weibull(n=nsim,a=-Inf,b=f$c1,sha=f$sha,sca=f$sca),error=function(err) NA),
                            eStrengthO2 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c1,b=f$c2,sha=f$sha,sca=f$sca),error=function(err) NA),
                            eStrengthO3 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c2,b=f$c3,sha=f$sha,sca=f$sca),error=function(err) NA),
                            eStrengthO4 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c3,b=f$c4,sha=f$sha,sca=f$sca),error=function(err) NA),
                            eStrengthO5 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c4,b=f$c5,sha=f$sha,sca=f$sca),error=function(err) NA),
                            eStrengthO6 = tryCatch(simExpectVal_weibull(n=nsim,a=f$c5,b=Inf,sha=f$sha,sca=f$sca),error=function(err) NA))
                        
                      }else
                        if(model == "lognorm"){
                          e <- 
                            e %>% 
                            mutate( # expected 2AFC accuracy
                              # using simulation function
                              nsim2AFC = nsim2AFC,
                              nsim=nsim2AFC,
                              sim_eFCpcor1 = tryCatch(simFCpcorr_lognorm(n=nsim2AFC,a=0,b=f$c1,mu=f$mu,sig=f$sig),error=function(err) NA),
                              sim_eFCpcor2 = tryCatch(simFCpcorr_lognorm(n=nsim2AFC,a=f$c1,b=f$c2,mu=f$mu,sig=f$sig),error=function(err) NA),
                              sim_eFCpcor3 = tryCatch(simFCpcorr_lognorm(n=nsim2AFC,a=f$c2,b=f$c3,mu=f$mu,sig=f$sig),error=function(err) NA),
                              sim_eFCpcor4 = tryCatch(simFCpcorr_lognorm(n=nsim2AFC,a=f$c3,b=f$c4,mu=f$mu,sig=f$sig),error=function(err) NA),
                              sim_eFCpcor5 = tryCatch(simFCpcorr_lognorm(n=nsim2AFC,a=f$c4,b=f$c5,mu=f$mu,sig=f$sig),error=function(err) NA),
                              sim_eFCpcor6 = tryCatch(simFCpcorr_lognorm(n=nsim2AFC,a=f$c5,b=Inf, mu=f$mu,sig=f$sig),error=function(err) NA),
                              eStrengthN1 = tryCatch(simExpectVal_lognorm(n=nsim,a=0,b=f$c1,mu=0,sig=0.25),error=function(err) NA),
                              eStrengthN2 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c1,b=f$c2,mu=0,sig=0.25),error=function(err) NA),
                              eStrengthN3 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c2,b=f$c3,mu=0,sig=0.25),error=function(err) NA),
                              eStrengthN4 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c3,b=f$c4,mu=0,sig=0.25),error=function(err) NA),
                              eStrengthN5 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c4,b=f$c5,mu=0,sig=0.25),error=function(err) NA),
                              eStrengthN6 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c5,b=Inf,mu=0,sig=0.25),error=function(err) NA),
                              eStrengthO1 = tryCatch(simExpectVal_lognorm(n=nsim,a=0,b=f$c1,mu=f$mu,sig=f$sig),error=function(err) NA),
                              eStrengthO2 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c1,b=f$c2,mu=f$mu,sig=f$sig),error=function(err) NA),
                              eStrengthO3 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c2,b=f$c3,mu=f$mu,sig=f$sig),error=function(err) NA),
                              eStrengthO4 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c3,b=f$c4,mu=f$mu,sig=f$sig),error=function(err) NA),
                              eStrengthO5 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c4,b=f$c5,mu=f$mu,sig=f$sig),error=function(err) NA),
                              eStrengthO6 = tryCatch(simExpectVal_lognorm(n=nsim,a=f$c5,b=Inf,mu=f$mu,sig=f$sig),error=function(err) NA))
                        }else
                          if(model == "expo"){
                            e <- 
                              e %>% 
                              mutate( # expected 2AFC accuracy
                                # using simulation function
                                nsim2AFC = nsim2AFC,
                                nsim=nsim2AFC,
                                sim_eFCpcor1 = tryCatch(simFCpcorr_expo(n=nsim2AFC,a=0,b=f$c1,rate=f$rateO),error=function(err) NA),
                                sim_eFCpcor2 = tryCatch(simFCpcorr_expo(n=nsim2AFC,a=f$c1,b=f$c2,rate=f$rateO),error=function(err) NA),
                                sim_eFCpcor3 = tryCatch(simFCpcorr_expo(n=nsim2AFC,a=f$c2,b=f$c3,rate=f$rateO),error=function(err) NA),
                                sim_eFCpcor4 = tryCatch(simFCpcorr_expo(n=nsim2AFC,a=f$c3,b=f$c4,rate=f$rateO),error=function(err) NA),
                                sim_eFCpcor5 = tryCatch(simFCpcorr_expo(n=nsim2AFC,a=f$c4,b=f$c5,rate=f$rateO),error=function(err) NA),
                                sim_eFCpcor6 = tryCatch(simFCpcorr_expo(n=nsim2AFC,a=f$c5,b=Inf,rate=f$rateO),error=function(err) NA),
                                eStrengthN1 = tryCatch(simExpectVal_expo(n=nsim,a=0,b=f$c1,rate=1),error=function(err) NA),
                                eStrengthN2 = tryCatch(simExpectVal_expo(n=nsim,a=f$c1,b=f$c2,rate=1),error=function(err) NA),
                                eStrengthN3 = tryCatch(simExpectVal_expo(n=nsim,a=f$c2,b=f$c3,rate=1),error=function(err) NA),
                                eStrengthN4 = tryCatch(simExpectVal_expo(n=nsim,a=f$c3,b=f$c4,rate=1),error=function(err) NA),
                                eStrengthN5 = tryCatch(simExpectVal_expo(n=nsim,a=f$c4,b=f$c5,rate=1),error=function(err) NA),
                                eStrengthN6 = tryCatch(simExpectVal_expo(n=nsim,a=f$c5,b=Inf,rate=1),error=function(err) NA),
                                eStrengthO1 = tryCatch(simExpectVal_expo(n=nsim,a=0,b=f$c1,rate=f$rateO),error=function(err) NA),
                                eStrengthO2 = tryCatch(simExpectVal_expo(n=nsim,a=f$c1,b=f$c2,rate=f$rateO),error=function(err) NA),
                                eStrengthO3 = tryCatch(simExpectVal_expo(n=nsim,a=f$c2,b=f$c3,rate=f$rateO),error=function(err) NA),
                                eStrengthO4 = tryCatch(simExpectVal_expo(n=nsim,a=f$c3,b=f$c4,rate=f$rateO),error=function(err) NA),
                                eStrengthO5 = tryCatch(simExpectVal_expo(n=nsim,a=f$c4,b=f$c5,rate=f$rateO),error=function(err) NA),
                                eStrengthO6 = tryCatch(simExpectVal_expo(n=nsim,a=f$c5,b=Inf,rate=f$rateO),error=function(err) NA))
                          }else
                            if(model == "gamma"){
                              e <- 
                                e %>% 
                                mutate( # expected 2AFC accuracy
                                  # using simulation function
                                  nsim2AFC = nsim2AFC,
                                  nsim=nsim2AFC,
                                  sim_eFCpcor1 = tryCatch(simFCpcorr_gamma(n=nsim2AFC,a=0,b=f$c1,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  sim_eFCpcor2 = tryCatch(simFCpcorr_gamma(n=nsim2AFC,a=f$c1,b=f$c2,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  sim_eFCpcor3 = tryCatch(simFCpcorr_gamma(n=nsim2AFC,a=f$c2,b=f$c3,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  sim_eFCpcor4 = tryCatch(simFCpcorr_gamma(n=nsim2AFC,a=f$c3,b=f$c4,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  sim_eFCpcor5 = tryCatch(simFCpcorr_gamma(n=nsim2AFC,a=f$c4,b=f$c5,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  sim_eFCpcor6 = tryCatch(simFCpcorr_gamma(n=nsim2AFC,a=f$c5,b=Inf, sha=f$sha,sca=f$sca),error=function(err) NA),
                                  # using integration function (not formulated, so can't do)
                                  #eFCpcor1 = tryCatch(integrate(intFCpcorr_gamma, lower=-Inf,upper=f$c1,sha=f$sha,sca=f$sca,a=-Inf,b=f$c1)$value, error=function(err) NA),
                                  #eFCpcor2 = tryCatch(integrate(intFCpcorr_gamma, lower=f$c1,upper=f$c2,sha=f$sha,sca=f$sca,a=f$c1,b=f$c2)$value, error=function(err) NA),
                                  #eFCpcor3 = tryCatch(integrate(intFCpcorr_gamma, lower=f$c2,upper=f$c3,sha=f$sha,sca=f$sca,a=f$c2,b=f$c3)$value, error=function(err) NA),
                                  #eFCpcor4 = tryCatch(integrate(intFCpcorr_gamma, lower=f$c3,upper=f$c4,sha=f$sha,sca=f$sca,a=f$c3,b=f$c4)$value, error=function(err) NA),
                                  #eFCpcor5 = tryCatch(integrate(intFCpcorr_gamma, lower=f$c4,upper=f$c5,sha=f$sha,sca=f$sca,a=f$c4,b=f$c5)$value, error=function(err) NA),
                                  #eFCpcor6 = tryCatch(integrate(intFCpcorr_gamma, lower=f$c5,upper=Inf,sha=f$sha,sca=f$sca, a=f$c5,b=Inf)$value, error=function(err) NA))
                                  eStrengthN1 = tryCatch(simExpectVal_gamma(n=nsim,a=-Inf,b=f$c1,sha=2,sca=1),error=function(err) NA),
                                  eStrengthN2 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c1,b=f$c2,sha=2,sca=1),error=function(err) NA),
                                  eStrengthN3 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c2,b=f$c3,sha=2,sca=1),error=function(err) NA),
                                  eStrengthN4 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c3,b=f$c4,sha=2,sca=1),error=function(err) NA),
                                  eStrengthN5 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c4,b=f$c5,sha=2,sca=1),error=function(err) NA),
                                  eStrengthN6 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c5,b=Inf,sha=2,sca=1),error=function(err) NA),
                                  eStrengthO1 = tryCatch(simExpectVal_gamma(n=nsim,a=-Inf,b=f$c1,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  eStrengthO2 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c1,b=f$c2,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  eStrengthO3 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c2,b=f$c3,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  eStrengthO4 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c3,b=f$c4,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  eStrengthO5 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c4,b=f$c5,sha=f$sha,sca=f$sca),error=function(err) NA),
                                  eStrengthO6 = tryCatch(simExpectVal_gamma(n=nsim,a=f$c5,b=Inf,sha=f$sha,sca=f$sca),error=function(err) NA))
                              
                            }
     

        # G-squared test for single-item recog
        do_g <- {}
        
        # add G-sq to e
        do_g <- g_test(d$N1,d$N2,d$N3,d$N4,d$N5,d$N6,d$O1,d$O2,d$O3,d$O4,d$O5,d$O6,
                         e$eN1,e$eN2,e$eN3,e$eN4,e$eN5,e$eN6,e$eO1,e$eO2,e$eO3,e$eO4,e$eO5,e$eO6,f$freep) 
        
        e <- 
          e %>% 
          mutate(ChiSq = do_g$ChiSq,
                 G = do_g$G,
                 df = do_g$df,
                 pG = do_g$pG) %>% 
          relocate(.after=eO6)
        
        
        if(to_fit=="simulated"){
          # compare the generative parameters with estimated
          
          gen_paras <- p %>% filter(ppt==fit)
          
          if(model=="evsd"){
            f <-
              f %>% 
              mutate(gen_dpri=gen_paras$dpri,
                     gen_c1=gen_paras$C1,
                     gen_c2=gen_paras$C2,
                     gen_c3=gen_paras$C3,
                     gen_c4=gen_paras$C4,
                     gen_c5=gen_paras$C5,
                     diff_dpri=gen_paras$dpri-f$dpri,
                     diff_c1=gen_paras$C1-f$c1,
                     diff_c2=gen_paras$C2-f$c2,
                     diff_c3=gen_paras$C3-f$c3,
                     diff_c4=gen_paras$C4-f$c4,
                     diff_c5=gen_paras$C5-f$c5)
          }else
            if(model=="uvsd"){
              f <-
                f %>% 
                mutate(gen_muo=gen_paras$muo,
                       gen_sigo=gen_paras$sigo,
                       gen_c1=gen_paras$C1,
                       gen_c2=gen_paras$C2,
                       gen_c3=gen_paras$C3,
                       gen_c4=gen_paras$C4,
                       gen_c5=gen_paras$C5,
                       diff_muo=gen_paras$muo-f$muo,
                       diff_sigo=gen_paras$sigo-f$sigo,
                       diff_c1=gen_paras$C1-f$c1,
                       diff_c2=gen_paras$C2-f$c2,
                       diff_c3=gen_paras$C3-f$c3,
                       diff_c4=gen_paras$C4-f$c4,
                       diff_c5=gen_paras$C5-f$c5)
            }else
              if(model=="dpsd"){
                f <-
                  f %>% 
                  mutate(gen_dpri=gen_paras$dpri,
                         gen_Ro=gen_paras$Ro,
                         gen_c1=gen_paras$C1,
                         gen_c2=gen_paras$C2,
                         gen_c3=gen_paras$C3,
                         gen_c4=gen_paras$C4,
                         gen_c5=gen_paras$C5,
                         diff_dpri=gen_paras$dpri-f$dpri,
                         diff_Ro=gen_paras$Ro-f$Ro,
                         diff_c1=gen_paras$C1-f$c1,
                         diff_c2=gen_paras$C2-f$c2,
                         diff_c3=gen_paras$C3-f$c3,
                         diff_c4=gen_paras$C4-f$c4,
                         diff_c5=gen_paras$C5-f$c5)
              }else
                if(model=="2ht"){
                  f <-
                    f %>% 
                    mutate(gen_do=gen_paras$do,
                           gen_dn=gen_paras$dn,
                           gen_g1=gen_paras$g1,
                           gen_g2=gen_paras$g2,
                           gen_g3=gen_paras$g3,
                           gen_g4=gen_paras$g4,
                           gen_g5=gen_paras$g5,
                           diff_do=gen_paras$do-f$do,
                           diff_dn=gen_paras$dn-f$dn,
                           diff_g1=gen_paras$g1-f$g1,
                           diff_g2=gen_paras$g2-f$g2,
                           diff_g3=gen_paras$g3-f$g3,
                           diff_g4=gen_paras$g4-f$g4,
                           diff_g5=gen_paras$g5-f$g5,
                           diff_g6=gen_paras$g6-f$g6)
                }else
                  if(model=="msd"){
                    f <-
                      f %>% 
                      mutate(gen_dpri=gen_paras$dpri,
                             gen_la=gen_paras$la,
                             gen_c1=gen_paras$C1,
                             gen_c2=gen_paras$C2,
                             gen_c3=gen_paras$C3,
                             gen_c4=gen_paras$C4,
                             gen_c5=gen_paras$C5,
                             diff_dpri=gen_paras$dpri-f$dpri,
                             diff_la=gen_paras$la-f$la,
                             diff_c1=gen_paras$C1-f$c1,
                             diff_c2=gen_paras$C2-f$c2,
                             diff_c3=gen_paras$C3-f$c3,
                             diff_c4=gen_paras$C4-f$c4,
                             diff_c5=gen_paras$C5-f$c5)
                  }else
                    if(model=="gumbel"){
                      f <-
                        f %>% 
                        mutate(gen_loc=gen_paras$loc,
                               gen_sca=gen_paras$sca,
                               gen_c1=gen_paras$C1,
                               gen_c2=gen_paras$C2,
                               gen_c3=gen_paras$C3,
                               gen_c4=gen_paras$C4,
                               gen_c5=gen_paras$C5,
                               diff_loc=gen_paras$loc-f$loc,
                               diff_sca=gen_paras$sca-f$sca,
                               diff_c1=gen_paras$C1-f$c1,
                               diff_c2=gen_paras$C2-f$c2,
                               diff_c3=gen_paras$C3-f$c3,
                               diff_c4=gen_paras$C4-f$c4,
                               diff_c5=gen_paras$C5-f$c5)
                    }else
                      if(model=="logistic"){
                        f <-
                          f %>% 
                          mutate(gen_loc=gen_paras$loc,
                                 gen_sca=gen_paras$sca,
                                 gen_c1=gen_paras$C1,
                                 gen_c2=gen_paras$C2,
                                 gen_c3=gen_paras$C3,
                                 gen_c4=gen_paras$C4,
                                 gen_c5=gen_paras$C5,
                                 diff_loc=gen_paras$loc-f$loc,
                                 diff_sca=gen_paras$sca-f$sca,
                                 diff_c1=gen_paras$C1-f$c1,
                                 diff_c2=gen_paras$C2-f$c2,
                                 diff_c3=gen_paras$C3-f$c3,
                                 diff_c4=gen_paras$C4-f$c4,
                                 diff_c5=gen_paras$C5-f$c5)
                      }else
                        if(model=="weibull"){
                          f <-
                            f %>% 
                            mutate(gen_sha=gen_paras$sha,
                                   gen_sca=gen_paras$sca,
                                   gen_c1=gen_paras$C1,
                                   gen_c2=gen_paras$C2,
                                   gen_c3=gen_paras$C3,
                                   gen_c4=gen_paras$C4,
                                   gen_c5=gen_paras$C5,
                                   diff_sha=gen_paras$sha-f$sha,
                                   diff_sca=gen_paras$sca-f$sca,
                                   diff_c1=gen_paras$C1-f$c1,
                                   diff_c2=gen_paras$C2-f$c2,
                                   diff_c3=gen_paras$C3-f$c3,
                                   diff_c4=gen_paras$C4-f$c4,
                                   diff_c5=gen_paras$C5-f$c5)
                        }else
                          if(model=="lognorm"){
                            f <-
                              f %>% 
                              mutate(gen_mu=gen_paras$mu,
                                     gen_sig=gen_paras$sig,
                                     gen_c1=gen_paras$C1,
                                     gen_c2=gen_paras$C2,
                                     gen_c3=gen_paras$C3,
                                     gen_c4=gen_paras$C4,
                                     gen_c5=gen_paras$C5,
                                     diff_mu=gen_paras$mu-f$mu,
                                     diff_sig=gen_paras$sig-f$sig,
                                     diff_c1=gen_paras$C1-f$c1,
                                     diff_c2=gen_paras$C2-f$c2,
                                     diff_c3=gen_paras$C3-f$c3,
                                     diff_c4=gen_paras$C4-f$c4,
                                     diff_c5=gen_paras$C5-f$c5)
                          }else
                            if(model=="expo"){
                              f <-
                                f %>% 
                                mutate(gen_rateO=gen_paras$rateO,
                                       gen_c1=gen_paras$C1,
                                       gen_c2=gen_paras$C2,
                                       gen_c3=gen_paras$C3,
                                       gen_c4=gen_paras$C4,
                                       gen_c5=gen_paras$C5,
                                       diff_rateO=gen_paras$rateO-f$rateO,
                                       diff_c1=gen_paras$C1-f$c1,
                                       diff_c2=gen_paras$C2-f$c2,
                                       diff_c3=gen_paras$C3-f$c3,
                                       diff_c4=gen_paras$C4-f$c4,
                                       diff_c5=gen_paras$C5-f$c5)
                            }else
                              if(model=="gamma"){
                                f <-
                                  f %>% 
                                  mutate(gen_sha=gen_paras$sha,
                                         gen_sca=gen_paras$sca,
                                         gen_c1=gen_paras$C1,
                                         gen_c2=gen_paras$C2,
                                         gen_c3=gen_paras$C3,
                                         gen_c4=gen_paras$C4,
                                         gen_c5=gen_paras$C5,
                                         diff_sha=gen_paras$sha-f$sha,
                                         diff_sca=gen_paras$sca-f$sca,
                                         diff_c1=gen_paras$C1-f$c1,
                                         diff_c2=gen_paras$C2-f$c2,
                                         diff_c3=gen_paras$C3-f$c3,
                                         diff_c4=gen_paras$C4-f$c4,
                                         diff_c5=gen_paras$C5-f$c5)
                              }
                  
        }
        
        
        if(model != "2ht"){
          e <- 
            e %>% 
            mutate(d_eStrength1 = tryCatch(eStrengthO1 - eStrengthN1,error=function(err) NA),
                   d_eStrength2 = tryCatch(eStrengthO2 - eStrengthN2,error=function(err) NA),
                   d_eStrength3 = tryCatch(eStrengthO3 - eStrengthN3,error=function(err) NA),
                   d_eStrength4 = tryCatch(eStrengthO4 - eStrengthN4,error=function(err) NA),
                   d_eStrength5 = tryCatch(eStrengthO5 - eStrengthN5,error=function(err) NA),
                   d_eStrength6 = tryCatch(eStrengthO6 - eStrengthN6,error=function(err) NA)
                   )
        }
        
        # store data, fits, expected vals
        results<- 
          bind_cols(d,  
                    f,  
                    e)  
        
      } else {
        # store data (only) from nonfittable ppt
        results<- 
          bind_cols(d)
      }
      
      # store results for fitppt in larger list
      pptlist[[fitppt]] <- results
      
    }
    
    # compile the summary model fit results from each ppt
    AllFits <- dplyr::bind_rows(pptlist)
    if(length(do_models)==1){AllFits %>% glimpse()}
    if(wrt_file == TRUE ){
      fname <- paste0("3_",model,"_results_",to_fit,".csv")
      write_csv(AllFits,fname)
      cat("         File created:", fname, "\n")  # show message
      }
  } #for each model
} #for each dataset


