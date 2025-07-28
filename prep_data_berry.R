
library("tidyverse")
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom"))
e1a_reco_in <- read_csv("data-berry-osf/2_e1a_recog_data_N72.csv") %>% 
  mutate(
    cond = factor(cond, levels = c(2, 1), labels = c("new", "old")),
    hmfc = factor(hmfc, levels = c("hit", "miss", "fa", "cr")),
    idx = factor(idx)
  )


e1_wide <- e1a_reco_in |> 
  mutate(conf = factor(rating16, 1:6)) |> 
  count(exp, idx, cond, conf, .drop = FALSE) |> 
  pivot_wider(id_cols = c(exp, idx), names_from = c(cond, conf), values_from = n) |> 
  as.data.frame()

e1a_reco_in


with(e1a_reco_in, table(cbcond, cond))
length(unique(e1a_reco_in$idx)) ## 72 as in paper

e1a_reco <- e1a_reco_in

save(e1a_reco, file = "de1a.rda")

e2a_reco_in <- read_csv("data-berry-osf/2_e2_recog_data_N72.csv") %>% 
  mutate(
    cond = factor(cond, levels = c(2, 1), labels = c("new", "old")),
    hmfc = factor(hmfc, levels = c("hit", "miss", "fa", "cr")),
    idx = factor(idx),
    conf = factor(rating16, 1:6)
  )

e2_wide <- e2a_reco_in |> 
  count(exp, idx, cond, conf, .drop = FALSE) |> 
  pivot_wider(id_cols = c(exp, idx), names_from = c(cond, conf), values_from = n) |> 
  as.data.frame()

e3a_reco_in <- read_csv("data-berry-osf/2_e3_recog_data_N72.csv") %>% 
  mutate(
    cond = factor(cond, levels = c(2, 1), labels = c("new", "old")),
    hmfc = factor(hmfc, levels = c("hit", "miss", "fa", "cr")),
    idx = factor(idx),
    conf = factor(rating16, 1:6)
  )

e3_wide <- e3a_reco_in |> 
  mutate(conf = factor(rating16, 1:6)) |> 
  count(idx, cond, conf, .drop = FALSE) |> 
  pivot_wider(id_cols = c(idx), names_from = c(cond, conf), values_from = n) |> 
  as.data.frame()
e3_wide <- e3a_reco_in |> 
  select(exp, idx) |> 
  unique() |> 
  right_join(e3_wide) |> 
  as.data.frame()
e3_wide

all_data_sherry <- bind_rows(e1_wide, e2_wide, e3_wide)
write.csv(all_data_sherry, file = "all_dat_sherry.csv")
