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
  expect_equal(round(metafits$meta_estimates$estimate, 8),
               c(-0.35362898, 0.16681359, -0.01954203, -0.25011999, 0.02217025,
                 0.03871327, -0.09301766, -0.18378699, 0.38171787, 1.37219847,
                 -0.4041782, 0.01603265, -0.0834234, -0.21858126, 0.01693329,
                 0.05269035, 0.02254787, -0.01551633, 0.38037816, 1.09985712,
                 -0.45250799, -0.09281327, -0.13131509, -0.19716963, 0.01181456,
                 0.06634176, 0.09583011, 0.09545653, 0.38865469, 0.97229083, -0.31461814,
                 -0.03850261, -0.07674807, -0.13737989, 0.00756489, 0.06599991,
                 0.06713145, 0.0515417, 0.26997154, 0.77179766, -0.01306388, 0.07087405,
                 0.02707263, -0.03777408, 0.00345696, 0.02220316, -0.05223952,
                 -0.06346193, 0.0408906, 0.27342696, 0.05530029, 0.02549755, 0.02217486,
                 0.01752446, -0.00112159, -0.01213486, -0.01465371, -0.01903965,
                 -0.0569937, -0.11063661, 0.19598204, 0.03084706, 0.05656154,
                 0.0840247, -0.00575236, -0.02904297, -0.01934121, -0.01075522,
                 -0.18652328, -0.49292054, 0.50361505, 0.17591011, 0.1755856,
                 0.18425331, -0.00974117, -0.07641293, -0.13619511, -0.16166139,
                 -0.44172927, -0.96975773, 0.7740444, 0.26339053, 0.2637263, 0.27880138,
                 -0.01400093, -0.13003648, -0.22033797, -0.25355949, -0.67692069,
                 -1.52065779, 0.91309128, 0.23140973, 0.27884281, 0.34636006,
                 -0.01879029, -0.18196788, -0.22731084, -0.22450263, -0.82060672,
                 -2.04294139))

  # Check that standard errors are correct
  expect_equal(round(metafits$meta_estimates$se, 8),
               c(0.83679226, 0.5843209, 0.41086429, 0.23395136, 0.01562784,
                 0.11483168, 0.33421385, 0.49069517, 0.61505578, 0.9172847, 0.61107181,
                 0.43192166, 0.30497493, 0.17566263, 0.01232981, 0.07477112, 0.23144036,
                 0.35008341, 0.44655754, 0.65387885, 0.43400629, 0.30693626, 0.21728598,
                 0.12617778, 0.0090502, 0.05164416, 0.16191944, 0.24609676, 0.31460543,
                 0.46058133, 0.2991943, 0.20513967, 0.14532081, 0.08488437, 0.00579015,
                 0.04176531, 0.11941364, 0.17165202, 0.2117607, 0.32542687, 0.12678613,
                 0.07894333, 0.05715619, 0.0365636, 0.00251681, 0.03107011, 0.05687829,
                 0.06895902, 0.07810659, 0.13938469, 0.05521852, 0.0404005, 0.02699821,
                 0.01346054, 0.00076878, 0.01429075, 0.02813864, 0.04066856, 0.05233521,
                 0.0694026, 0.24830819, 0.1768464, 0.12191227, 0.06552261, 0.00405159,
                 0.0397575, 0.10726329, 0.15736398, 0.19922754, 0.28567288, 0.39819074,
                 0.2859399, 0.19940145, 0.1104742, 0.00732229, 0.05127077, 0.15598952,
                 0.23797038, 0.30711493, 0.43672682, 0.58398652, 0.41629422, 0.29044947,
                 0.16127562, 0.01060372, 0.07793008, 0.23225556, 0.34983098, 0.44743906,
                 0.64396063, 0.80472186, 0.5665076, 0.39433923, 0.21782701, 0.01389563,
                 0.12061289, 0.33598746, 0.49103688, 0.61680651, 0.90565704))



})
