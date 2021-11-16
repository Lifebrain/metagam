library(metagam)
library(mgcv)

## Create 5 datasets
set.seed(1234)
datasets <- lapply(1:5, function(x) gamSim(scale = 5, verbose = FALSE))

## Fit a GAM in each dataset, then use strip_rawdata() to remove
## individual participant data
models <- lapply(datasets, function(dat){
  ## This uses the gam() function from mgcv
  dat$grp <- factor(sample(1:3, size = nrow(dat), replace = TRUE))
  model <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + grp, data = dat)
  ## This uses strip_rawdata() from metagam
  strip_rawdata(model)
})

grid = NULL
grid_size = 10
type = "iterms"
terms = NULL
method = "FE"
nsim = 100
ci_alpha = 0.05

restrict_range = NULL
devtools::load_all()

meta_analysis <- metagam(models, terms = c("s(x0)", "s(x1)"), grid_size = 30, nsim = 10)

set.seed(123)
datasets <- lapply(1:5, function(x) gamSim(eg = 2, n = 50, verbose = FALSE)$data)

models <- lapply(datasets, function(dat){
  b <- gam(y ~ te(x, z), data = dat)
  strip_rawdata(b)
})


metafit <- metagam(models, grid_size = 10)
expect_s3_class(metafit, "metagam")
expect_equal(round(metafit$meta_estimates$ci.lb[1:4], 10),
             c(-0.2802061066, -0.0279839076, -0.1800437287, -0.290758912))

expect_equal(round(metafit$meta_estimates$estimate[40:50], 10),
             c(-0.0618375179, 0.4791554907, 0.4050419988, 0.3156138636, 0.272017788,
               0.1986136027, 0.0434866897, -0.1151385155, -0.1873485645, 0.0244868197,
               0.2823631963))

expect_error(metagam(fits, grid_size = 10, nsim = 100))
