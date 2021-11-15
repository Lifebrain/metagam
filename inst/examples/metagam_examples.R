library(metagam)
library(mgcv)

## Create 5 datasets
set.seed(1234)
datasets <- lapply(1:5, function(x) gamSim(scale = 5, verbose = FALSE))

## Fit a GAM in each dataset, then use strip_rawdata() to remove
## individual participant data
models <- lapply(datasets, function(dat){
  ## This uses the gam() function from mgcv
  model <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr") + s(x2, bs = "cr"), data = dat)
  ## This uses strip_rawdata() from metagam
  strip_rawdata(model)
})

## Next, we meta-analyze the models.
## It is often most convenient to analyze a single term at a time. We focus on s(x1).
meta_analysis <- metagam(models, terms = "s(x1)", grid_size = 30)

## We can print some information
summary(meta_analysis)

## We can plot the meta-analytic fit together with the individual fits
plot(meta_analysis)

## We can also compute p-values and simultaneous confidence intervals, by setting the nsim argument.
## For details, see the separate vignette.
\dontrun{
  meta_analysis <- metagam(models, terms = "s(x0)", grid_size = 30, nsim = 1000)
  summary(meta_analysis)
}
