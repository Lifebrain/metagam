library(tidyverse)
library(mgcv)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x) - 1
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
  (10 * x)^3 * (1 - x)^10
f3 <- function(x) 0 * x

dat <- tibble(
  x1 = runif(1000),
  x2 = runif(1000),
  z = factor(sample(2, 1000, replace = TRUE)),
  f1 = f1(x1),
  f2 = f2(x2),
  y = f1 + f2 + rnorm(1000, sd =30)
)

dat2 <- dat[dat$x2 < .5, ]
dat1 <- dat[301:500, ]
dat3 <- dat[501:1000, ]

form <- as.formula(y ~ z + s(x1, bs = 'cr', pc = 0) + s(x2, k = 6, bs = 'cr', pc = 0))

# Fit a model
fits <- lapply(list(dat1, dat2, dat3), function(d){
  fit <- mgcv::gam(form, data = d, method = "REML")
  metagam::prepare_meta(fit)
})

predict(fits[[1]], newdata = tibble(x1 = .5, z = factor(1, levels=1:2)), type = "terms", terms = "s(x1)", newdata.guaranteed = TRUE)

#grid <- expand.grid(replicate(3, seq(from = 0, to = 1, by = .01), simplify = FALSE))
#colnames(grid) <- c("x0", "x1", "x2")
grid <- tibble(x0 = 0, x1 = 0, x2 = seq(from = .0, to = 1, by = .1), z = factor(1, levels = c(1, 2)))

fit <- fits[[1]]
terms <- "s(x2)"
sig_level <- NULL
nmc_sig_test <- NULL
type <- "iterms"
restrict_max = NULL; restrict_min = NULL; intercept = TRUE
method <- "fixed"

metafit <- metagam(fits, grid, type = "iterms", terms = "s(x2)")

# Compare with using all data at once
#fullfit <- metagam(list(gam(form, data = dat, method = "REML")), grid, type = "iterms")

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

