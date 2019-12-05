


dat <- mgcv::gamSim(n = 1000, scale = 7)
dat1 <- dat[1:300, ]
dat2 <- dat[301:500, ]
dat3 <- dat[501:1000, ]



models <- lapply(list(dat1, dat2, dat3), function(d){
  model <- mgcv::gam(y ~ s(x2), data = d, method = "REML")
  singlegam(model, nmc = 100, freq = FALSE, unconditional = FALSE)
})
metafit <- metagam(models)

library(tidyverse)
metafit$all_posteriors$x2 %>%
  filter(quantity == "mean") %>%
  ggplot(aes(x = predictor_value, y = value)) + geom_line()

