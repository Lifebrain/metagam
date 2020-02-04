library(mgcv)

set.seed(123)
# Generate some fits
ndat <- 3
n <- 100
fits <- lapply(1:ndat, function(x){
  dat <- gamSim(n = n, verbose = FALSE)
  b <- gam(y ~ s(x0, bs = "cr") + s(x1, bs = "cr"), data = dat)
  strip_rawdata(b)
})

# Meta-analyze
test_that("metagam works", {
  metafits <- metagam(fits, grid_size = 10)
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, "s(x0)")
  # Check that predictions are correct
  # Vector to compare with has been generate by dput in a manual first run
  expect_equal(round(metafits$meta_estimates$estimate, 8),
               c(-0.22194647, -0.13913644, -0.04638216, 0.04226372, 0.10112179,
                 0.11856284, 0.09830259, 0.04701996, -0.03726664, -0.14627162))

  # Check that standard errors are correct
  expect_equal(round(metafits$meta_estimates$se, 8),
               c(0.42231386, 0.32479295, 0.26294788, 0.23210114, 0.22074271,
                 0.22037242, 0.23108217, 0.26216805, 0.32538673, 0.42494948))

  # Now with second term
  metafits <- metagam(fits, grid_size = 10, terms = "s(x1)")
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, "s(x1)")

  # Check that predictions are correct
  # Vector to compare with has been generate by dput in a manual first run
  expect_equal(round(metafits$meta_estimates$estimate, 8),
               c(-1.90254055, -1.54695893, -1.18290861, -0.74264595, -0.28591313,
                 0.40713456, 0.76885555, 1.37551416, 2.08311411, 2.8655438))

  # Check that standard errors are correct
  expect_equal(round(metafits$meta_estimates$se, 8),
               c(0.45711194, 0.34464661, 0.28070113, 0.25022086, 0.24067681,
                 0.24936844, 0.27250813, 0.3185325, 0.3812979, 0.50396543))

  # Now with both terms
  grid <- data.frame(x0 = seq(0, 1, by = .1), x1 = seq(0, 1, by = .1))
  metafits <- metagam(fits, grid = grid, terms = c("s(x0)", "s(x1)"))
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, c("s(x0)", "s(x1)"))

  # Check that predictions are correct
  # Vector to compare with has been generate by dput in a manual first run
  expect_equal(round(metafits$meta_estimates$estimate, 8),
               c(-0.22238192, -1.90845939, -0.14829188, -1.54892801, -0.06563728,
                 -1.31104563, 0.01759225, -0.82227955, 0.08236379, -0.537348,
                 0.11504366, 0.07442868, 0.1144476, 0.59027953, 0.08619289, 0.93966274,
                 0.03228429, 1.51758374, -0.04820967, 2.21008333, -0.14742953,
                 2.88682888))

  # Check that standard errors are correct
  expect_equal(round(metafits$meta_estimates$se, 8),
               c(0.42295604, 0.45839506, 0.33330303, 0.35355031, 0.27261384,
                 0.29159583, 0.23866624, 0.25726103, 0.22347574, 0.24035071, 0.21934645,
                 0.24135044, 0.2229785, 0.25485699, 0.23775134, 0.28412049, 0.27228278,
                 0.32960658, 0.3344465, 0.38952799, 0.42611871, 0.51139217))

  # Use link function
  metafits <- metagam(fits, grid_size = 10, type = "link")
  expect_s3_class(metafits, "metagam")


  # Check that predictions are correct
  # Vector to compare with has been generate by dput in a manual first run
  expect_equal(round(metafits$meta_estimates$estimate, 8),
               c(7.33333206, 7.42838454, 7.53540961, 7.62928429, 7.68308529,
                 7.69497625, 7.67490442, 7.62635562, 7.54026725, 7.42558443))

  # Check that standard errors are correct
  expect_equal(round(metafits$meta_estimates$se, 8),
               c(0.42231386, 0.32479295, 0.26294788, 0.23210114, 0.22074271,
                 0.22037242, 0.23108217, 0.26216805, 0.32538673, 0.42494948))

})
