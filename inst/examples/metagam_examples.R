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

## We can plot the fit
plot(meta_analysis)

## Meta analysis can also be performed in parallel, using future
\dontrun{
  library(future)
  plan(multisession, workers = 2)
  meta_analysis <- metagam(models, terms = "s(x1)", grid_size = 30)
  plan("default")
}
