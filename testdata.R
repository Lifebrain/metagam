library(tidyverse)
library(mgcv)


dat <- mgcv::gamSim(n = 1000, scale = 7)

dat1 <- dat[1:300, ]
dat2 <- dat[301:500, ]
dat3 <- dat[501:1000, ]

# Fit a model
fits <- lapply(list(dat1, dat2, dat3), function(d){
  fit <- mgcv::gam(y ~ x0 + s(x1) + s(x2), data = d, method = "REML")
  metagam::prepare_meta(fit)
})

#grid <- expand.grid(replicate(3, seq(from = 0, to = 1, by = .01), simplify = FALSE))
#colnames(grid) <- c("x0", "x1", "x2")
grid <- tibble(x0 = 0, x1 = 0, x2 = seq(from = 0, to = 1, by = .1))

fit <- fits[[1]]
terms <- "s(x2)"
metafit <- metagam(fits, grid, type = "iterms")

# Compare with using all data at once
fullfit <- metagam(list(gam(y ~ x0 + s(x1) + s(x2), data = dat, method = "REML")), grid, type = "iterms")

bind_rows(
  meta = metafit$prediction,
  full = fullfit$prediction,
  .id = "type"
) %>%
  filter(x0 == 0, x1 == 0, term == "s(x2)") %>%
  ggplot(aes(x = x2, y = fit, ymin = fit - 2 * se, ymax = fit + 2 * se,
             group = type, color = type)) +
  geom_line() +
  geom_ribbon(alpha = .2)
