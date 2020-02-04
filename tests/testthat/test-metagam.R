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


set.seed(1234)
# Try with tensor interaction terms
fits <- lapply(1:ndat, function(x){
  dat <- gamSim(n = n, verbose = FALSE)
  b <- gam(y ~ ti(x0, x1, bs = "cr"), data = dat)
  strip_rawdata(b)
})


test_that("metagam works with 2D smooths", {
  metafits <- metagam(fits, grid_size = 10)
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, "ti(x0,x1)")
  # Check that predictions are correct
  # Vector to compare with has been generate by dput in a manual first run
  expect_equal(round(metafits$meta_estimates$estimate, 5),
               c(-0.35363, 0.16681, -0.01954, -0.25012, 0.02217, 0.03871, -0.09302,
                 -0.18379, 0.38172, 1.3722, -0.40418, 0.01603, -0.08342, -0.21858,
                 0.01693, 0.05269, 0.02255, -0.01552, 0.38038, 1.09986, -0.45251,
                 -0.09281, -0.13132, -0.19717, 0.01181, 0.06634, 0.09583, 0.09546,
                 0.38865, 0.97229, -0.31462, -0.0385, -0.07675, -0.13738, 0.00756,
                 0.066, 0.06713, 0.05154, 0.26997, 0.7718, -0.01306, 0.07087,
                 0.02707, -0.03777, 0.00346, 0.0222, -0.05224, -0.06346, 0.04089,
                 0.27343, 0.0553, 0.0255, 0.02217, 0.01752, -0.00112, -0.01213,
                 -0.01465, -0.01904, -0.05699, -0.11064, 0.19598, 0.03085, 0.05656,
                 0.08402, -0.00575, -0.02904, -0.01934, -0.01076, -0.18652, -0.49292,
                 0.50362, 0.17591, 0.17559, 0.18425, -0.00974, -0.07641, -0.1362,
                 -0.16166, -0.44173, -0.96976, 0.77404, 0.26339, 0.26373, 0.2788,
                 -0.014, -0.13004, -0.22034, -0.25356, -0.67692, -1.52066, 0.91309,
                 0.23141, 0.27884, 0.34636, -0.01879, -0.18197, -0.22731, -0.2245,
                 -0.82061, -2.04294))

  # Check that standard errors are correct
  expect_equal(round(metafits$meta_estimates$se, 5),
               c(0.83679, 0.58432, 0.41086, 0.23395, 0.01563, 0.11483, 0.33421,
                 0.4907, 0.61506, 0.91728, 0.61107, 0.43192, 0.30497, 0.17566,
                 0.01233, 0.07477, 0.23144, 0.35008, 0.44656, 0.65388, 0.43401,
                 0.30694, 0.21729, 0.12618, 0.00905, 0.05164, 0.16192, 0.2461,
                 0.31461, 0.46058, 0.29919, 0.20514, 0.14532, 0.08488, 0.00579,
                 0.04177, 0.11941, 0.17165, 0.21176, 0.32543, 0.12679, 0.07894,
                 0.05716, 0.03656, 0.00252, 0.03107, 0.05688, 0.06896, 0.07811,
                 0.13938, 0.05522, 0.0404, 0.027, 0.01346, 0.00077, 0.01429, 0.02814,
                 0.04067, 0.05234, 0.0694, 0.24831, 0.17685, 0.12191, 0.06552,
                 0.00405, 0.03976, 0.10726, 0.15736, 0.19923, 0.28567, 0.39819,
                 0.28594, 0.1994, 0.11047, 0.00732, 0.05127, 0.15599, 0.23797,
                 0.30711, 0.43673, 0.58399, 0.41629, 0.29045, 0.16128, 0.0106,
                 0.07793, 0.23226, 0.34983, 0.44744, 0.64396, 0.80472, 0.56651,
                 0.39434, 0.21783, 0.0139, 0.12061, 0.33599, 0.49104, 0.61681,
                 0.90566))



})
