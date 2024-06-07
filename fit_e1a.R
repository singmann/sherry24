library("tidyverse")
library("brms")
library("rstan")
options(mc.cores = parallel::detectCores())

theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom"))
source("uvsdt6p.R")
source("gumbel6p.R")
source("gumbelmix6p.R")
load("de1a.rda")


str(e1a_reco)

e1a_reco <- e1a_reco %>% 
  mutate(is_old = as.numeric(cond == "old"))

dagg <- e1a_reco %>% 
  group_by(cond) %>% 
  count(rating16) %>% 
  mutate(n = n/sum(n))


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
  prior(student_t(3, 0, 2), class = b)

stancode(uvsdt_formula, data = e1a_reco, stanvars = stanvars, prior = uvsdt_priors)

fe1a_uvsdt_1 <- brm(
  uvsdt_formula, data = e1a_reco, 
  stanvars = stanvars, prior = uvsdt_priors, 
  file = "fe1a_uvsdt_1", init_r = 0.5
)

uvsdt_formula2 <- brmsformula(
  rating16 | vint(is_old)  ~ 0 + cond + (0 + cond|p|idx) +
    (cond|stimID), 
  discsignal ~ 1,
  crc ~ (1|p|idx), 
  crlm ~ (1|p|idx), crll ~ (1|p|idx), 
  crhm ~ (1|p|idx), crhh ~ (1|p|idx),
  family = uvsdt6p_family, cmc = FALSE, center = FALSE
)


fe1a_uvsdt_2 <- brm(
  uvsdt_formula2, data = e1a_reco, 
  stanvars = stanvars, prior = uvsdt_priors, 
  file = "fe1a_uvsdt_2", init_r = 0.5
)

  
pred_uvsd <- posterior_predict(fe1a_uvsdt_2)

d_pred_uvsd <- bind_cols(e1a_reco, as.data.frame(t(pred_uvsd)))
d_pred_uvsd2 <- d_pred_uvsd %>% 
  group_by(cond) %>% 
  summarise(across(starts_with("V"), 
                   ~list(as.data.frame(prop.table(table(.)))[,"Freq", drop = FALSE]))) %>%
  unnest(cols = -cond, names_sep = "_")

d_pred_uvsd2 <- d_pred_uvsd2 %>% 
  mutate(rating16 = rep(1:6, 2)) %>% 
  select(cond, rating16, everything())

d_pred_uvsd2_l <- d_pred_uvsd2 %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(cond, rating16) %>% 
  summarise(uvsdt_m = mean(value), 
            uvsdt_l = quantile(value, 0.025), 
            uvsdt_h = quantile(value, 0.975))
d_pred_uvsd2_l

dagg2 <- left_join(dagg, d_pred_uvsd2_l) %>% 
  rename(data = n, model = uvsdt_m, upper = uvsdt_l, lower = uvsdt_h)

p1 <- dagg2 %>% 
  pivot_wider(names_from = cond, values_from = c(data, model, upper, lower)) %>% 
  arrange(desc(rating16)) %>% 
  mutate(across(-rating16, cumsum)) %>% 
  filter(rating16 != 1) %>% 
  ggplot(aes(x = data_new, y = data_old)) +
  geom_point(aes(x = model_new, y = model_old), shape = 1) + 
  geom_linerange(aes(x = model_new, y = model_old, ymin = lower_old, ymax = upper_old), 
                 colour = "grey") +
  geom_linerange(aes(x = model_new, y = model_old, xmin = lower_new, xmax = upper_new), 
                 colour = "grey") +
  geom_abline(slope = 1, intercept = 0) +
  geom_line(aes(group = 1)) +
  geom_point() +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  ggtitle("uvsd fit")

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

pred_gumbel <- posterior_predict(fe1a_gumbel_2)
d_pred_uvsd <- bind_cols(e1a_reco, as.data.frame(t(pred_gumbel)))
d_pred_uvsd2 <- d_pred_uvsd %>% 
  group_by(cond) %>% 
  summarise(across(starts_with("V"), 
                   ~list(as.data.frame(prop.table(table(.)))[,"Freq", drop = FALSE]))) %>%
  unnest(cols = -cond, names_sep = "_")

d_pred_uvsd2 <- d_pred_uvsd2 %>% 
  mutate(rating16 = rep(1:6, 2)) %>% 
  select(cond, rating16, everything())

d_pred_uvsd2_l <- d_pred_uvsd2 %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(cond, rating16) %>% 
  summarise(uvsdt_m = mean(value), 
            uvsdt_l = quantile(value, 0.025), 
            uvsdt_h = quantile(value, 0.975))
d_pred_uvsd2_l

dagg3 <- left_join(dagg, d_pred_uvsd2_l) %>% 
  rename(data = n, model = uvsdt_m, upper = uvsdt_l, lower = uvsdt_h)

p2 <- dagg3 %>% 
  pivot_wider(names_from = cond, values_from = c(data, model, upper, lower)) %>% 
  arrange(desc(rating16)) %>% 
  mutate(across(-rating16, cumsum)) %>% 
  filter(rating16 != 1) %>% 
  ggplot(aes(x = data_new, y = data_old)) +
  geom_point(aes(x = model_new, y = model_old), shape = 1) + 
  geom_linerange(aes(x = model_new, y = model_old, ymin = lower_old, ymax = upper_old), 
                 colour = "grey") +
  geom_linerange(aes(x = model_new, y = model_old, xmin = lower_new, xmax = upper_new), 
                 colour = "grey") +
  geom_abline(slope = 1, intercept = 0) +
  geom_line(aes(group = 1)) +
  geom_point() +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  ggtitle("Gumbel fit")

  
### mixture
gumbelmix_formula <- brmsformula(
  rating16 | vint(is_old)  ~ 0 + cond + (0 + cond|p|idx) +
    (cond|stimID), 
  crc ~ (1|p|idx), 
  crlm ~ (1|p|idx), crll ~ (1|p|idx), 
  crhm ~ (1|p|idx), crhh ~ (1|p|idx),
  mix ~ (1|p|idx),
  family = gumbelmix6p_family, cmc = FALSE, center = FALSE
)

get_prior(gumbelmix_formula, data = e1a_reco)

gumbelmix_priors <- prior(normal(0,0.5), class = Intercept, dpar = "crc") + 
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crlm") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crll") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crhm") +
  prior(normal(-0.5,0.5), class = Intercept, dpar = "crhh") +
  prior(student_t(3, 0, 2), class = b) +
  prior(normal(0,1), class = Intercept, dpar = "mix")

stancode(gumbelmix_formula, data = e1a_reco, 
  stanvars = sv_gumbelmix6p, 
  prior = gumbelmix_priors)

fe1a_gumbelmix_2 <- brm(
  gumbelmix_formula, data = e1a_reco, 
  stanvars = sv_gumbelmix6p, 
  prior = gumbelmix_priors,
  file = "fe1a_gumbelmix_2", init_r = 0.5
)

pred_gumbel_mix <- posterior_predict(fe1a_gumbelmix_2)
# pptmp <- prepare_predictions(fe1a_gumbelmix_2)
#posterior_predict_gumbelmix6p(2, pptmp)

d_pred_uvsd <- bind_cols(e1a_reco, as.data.frame(t(pred_gumbel_mix)))
d_pred_uvsd2 <- d_pred_uvsd %>% 
  group_by(cond) %>% 
  summarise(across(starts_with("V"), 
                   ~list(as.data.frame(prop.table(table(.)))[,"Freq", drop = FALSE]))) %>%
  unnest(cols = -cond, names_sep = "_")

d_pred_uvsd2 <- d_pred_uvsd2 %>% 
  mutate(rating16 = rep(1:6, 2)) %>% 
  select(cond, rating16, everything())

d_pred_uvsd2_l <- d_pred_uvsd2 %>% 
  pivot_longer(cols = starts_with("V")) %>% 
  group_by(cond, rating16) %>% 
  summarise(uvsdt_m = mean(value), 
            uvsdt_l = quantile(value, 0.025), 
            uvsdt_h = quantile(value, 0.975))
d_pred_uvsd2_l

dagg4 <- left_join(dagg, d_pred_uvsd2_l) %>% 
  rename(data = n, model = uvsdt_m, upper = uvsdt_l, lower = uvsdt_h)

p3 <- dagg4 %>% 
  pivot_wider(names_from = cond, values_from = c(data, model, upper, lower)) %>% 
  arrange(desc(rating16)) %>% 
  mutate(across(-rating16, cumsum)) %>% 
  filter(rating16 != 1) %>% 
  ggplot(aes(x = data_new, y = data_old)) +
  geom_point(aes(x = model_new, y = model_old), shape = 1) + 
  geom_linerange(aes(x = model_new, y = model_old, ymin = lower_old, ymax = upper_old), 
                 colour = "grey") +
  geom_linerange(aes(x = model_new, y = model_old, xmin = lower_new, xmax = upper_new), 
                 colour = "grey") +
  geom_abline(slope = 1, intercept = 0) +
  geom_line(aes(group = 1)) +
  geom_point() +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  ggtitle("Gumbel Mixture Fit")

cowplot::plot_grid(p1, p2, p3, nrow = 1)
ggsave("fit_comparison.pdf", width = 14, height = 5)
