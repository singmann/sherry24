
library("tidyverse")
theme_set(theme_bw(base_size = 15) + 
            theme(legend.position="bottom"))
e1a_reco_in <- read_csv("data-berry-osf/2_e1a_recog_data_N72.csv") %>% 
  mutate(
    cond = factor(cond, levels = c(1, 2), labels = c("old", "new")),
    hmfc = factor(hmfc, levels = c("hit", "miss", "fa", "cr")),
    idx = factor(idx)
  )

with(e1a_reco_in, table(cbcond, cond))
length(unique(e1a_reco_in$idx)) ## 72 as in paper


