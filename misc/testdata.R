library(gamm4)
set.seed(123)
ndat <- 3
n <- 100
fullfits_gamm4 <- lapply(1:ndat, function(x){
  dat <- gamSim(n = n, verbose = FALSE)
  dat$ID <- sample(1:3, size = n, replace = TRUE)
  b <- gamm4(y ~ s(x0, bs = 'cr') + s(x1, bs = 'cr'), data = dat,
             random = ~(1|ID))
})

saveRDS(fullfits_gamm4, "tests/testdata/fullfits_gamm4.rda")
