dat <- mgcv::gamSim(n = 1000)

dat$f <- factor(cut(dat$x0, breaks = 3, labels = FALSE))
dat1 <- dat[1:300, ]
dat2 <- dat[301:500, ]
dat3 <- dat[501:1000, ]

# grid <- list(
#   x1 = seq(from = 0, to = 1, length.out = 101),
#   x2 = seq(from = 0, to = 1, length.out = 101),
#   f = factor(c(1, 2, 3))
# )

models <- lapply(list(dat1, dat2, dat3), function(d){
  fit <- mgcv::gam(y ~ s(x2) + x1 + f, data = d)
  singlegam(fit)
})


metafit <- metagam(models)

# Reshape and plot for each variable
var <- "x2"
df <- reshape(metafit$all_posteriors[[var]], v.names = "value", idvar = "predictor_value",
        timevar = "quantity", direction = "wide")

library(ggplot2)
ggplot(df, aes(x = predictor_value, y = value.mean)) +
  geom_line() +
  geom_line(aes(y = `value.2.5%`), linetype = "dashed") +
  geom_line(aes(y = `value.97.5%`), linetype = "dashed")
