library(tidyverse)
library(metagam)
library(mgcv)
library(patchwork)

dat0 <- gamSim(verbose = FALSE, scale = 2) %>%
  as_tibble() %>%
  mutate(
    Study = sample(c("A", "B", "C", "D"), size = nrow(.), replace = TRUE),
    y = if_else(Study %in% c("A", "B"), -y, y)
  ) %>%
  select(x1, y, Study) %>%
  rename(x = x1) %>%
  nest_by(Study)

models <- pmap(dat0, function(Study, data){
  mod <- gam(y ~ s(x, bs = "cr"), data = data, method = "REML")
  strip_rawdata(mod)
})
names(models) <- c("A", "B", "C", "D")

devtools::load_all()

grid = NULL
grid_size = 100
type = "iterms"
terms = NULL
method = "FE"
nsim = NULL
ci_alpha = 0.05
restrict_range = NULL
nsim <- 1000
mm <- metagam(models, nsim = 1000)

plot(mm, ci = "pointwise") + plot(mm, ci = "simultaneous")


mm$simulation_results$`s(x)`
mm$pvals
