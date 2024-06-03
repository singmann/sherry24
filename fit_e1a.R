library("tidyverse")
library("brms")
library("rstan")
options(mc.cores = parallel::detectCores())

theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom"))
source("uvsdt6p.R")
load("de1a.rda")

str(e1a_reco)

e1a_reco <- e1a_reco %>% 
  mutate(is_old = as.numeric(cond == "old"))

afex::set_treatment_contrasts()
uvsdt_formula <- brmsformula(
  rating16 | vint(is_old)  ~ cond + (0 + cond|p|idx) +
    (cond|stimID), 
  discsignal ~ 1,
  crc ~ (1|p|idx), 
  crlm ~ (1|p|idx), crll ~ (1|p|idx), 
  crhm ~ (1|p|idx), crhh ~ (1|p|idx),
  family = uvsdt6p_family, cmc = FALSE
)

get_prior(uvsdt_formula, data = e1a_reco)

uvsdt_priors <- prior(normal(0,0.5), class = Intercept, dpar = "crc") + 
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crlm") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crll") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crhm") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crhh") +
  prior(student_t(3, 0.5, 1), class = Intercept, dpar = "discsignal") +
  prior(student_t(3, 0.5, 2), class = b)

stancode(uvsdt_formula, data = e1a_reco, stanvars = stanvars, prior = uvsdt_priors)

fe1a_uvsdt_1 <- brm(
  uvsdt_formula, data = e1a_reco, 
  stanvars = stanvars, prior = uvsdt_priors, 
  file = "fe1a_uvsdt_1", init_r = 0.5
)
  
### gumbel evsdt
gumbel_formula <- brmsformula(
  rating16 | vint(is_old)  ~ cond + (0 + cond|p|idx) +
    (cond|stimID), 
  crc ~ (1|p|idx), 
  crlm ~ (1|p|idx), crll ~ (1|p|idx), 
  crhm ~ (1|p|idx), crhh ~ (1|p|idx),
  family = gumbel6p_family, cmc = FALSE
)

get_prior(gumbel_formula, data = e1a_reco)

gumbel_priors <- prior(normal(0,0.5), class = Intercept, dpar = "crc") + 
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crlm") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crll") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crhm") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crhh") +
  prior(student_t(3, 0, 2), class = b)

stancode(gumbel_formula, data = e1a_reco, 
         stanvars = sv_gumbel6p, 
         prior = gumbel_priors)

fe1a_gumbel_1 <- brm(
  gumbel_formula, data = e1a_reco, 
  stanvars = sv_gumbel6p, 
  prior = gumbel_priors,
  file = "fe1a_gumbel_1", init_r = 0.5
)

gumbel_formula2 <- brmsformula(
  rating16 | vint(is_old)  ~ 0 + cond + (0 + cond|p|idx) +
    (cond|stimID), 
  crc ~ (1|p|idx), 
  crlm ~ (1|p|idx), crll ~ (1|p|idx), 
  crhm ~ (1|p|idx), crhh ~ (1|p|idx),
  family = gumbel6p_family, cmc = FALSE
)
fe1a_gumbel_2 <- brm(
  gumbel_formula2, data = e1a_reco, 
  stanvars = sv_gumbel6p, 
  prior = gumbel_priors,
  file = "fe1a_gumbel_2", init_r = 0.5
)
  