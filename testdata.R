dat <- mgcv::gamSim(n = 1000)

dat$f <- factor(cut(dat$x0, breaks = 3, labels = FALSE))
dat1 <- dat[1:300, ]
dat2 <- dat[301:500, ]
dat3 <- dat[501:1000, ]

grid <- list(
  x1 = seq(from = 0, to = 1, length.out = 11),
  x2 = seq(from = 0, to = 1, length.out = 11),
  f = factor(c(1, 2, 3))
)

model1 <- mgcv::gam(y ~ s(x2) + x1 + f, data = dat1)
fit1 <- singlegam(model1, grid = grid)

model2 <- mgcv::gam(y ~ s(x2) + x1 + f, data = dat2)
fit2 <- singlegam(model2, grid = grid)

models <- list(fit1, fit2)
