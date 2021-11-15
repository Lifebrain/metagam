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
  expect_equal(round(metafits$meta_estimates$estimate, 7),
               c(-0.2219465, -0.1391364, -0.0463822, 0.0422637, 0.1011218, 0.1185628,
                 0.0983026, 0.04702, -0.0372666, -0.1462716))

  expect_equal(round(metafits$meta_estimates$ci.lb - metafits$meta_estimates$ci.ub, 7),
               c(-1.6554399, -1.273165, -1.0307368, -0.9098197, -0.8652955,
                 -0.863844, -0.9058255, -1.0276799, -1.2754925, -1.6657713))

  # Now with second term
  metafits <- metagam(fits, grid_size = 10, terms = "s(x1)")
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, "s(x1)")

  expect_equal(round(metafits$meta_estimates$estimate, 7),
               c(-1.9025406, -1.5469589, -1.1829086, -0.742646, -0.2859131,
                 0.4071346, 0.7688556, 1.3755142, 2.0831141, 2.8655438))

  expect_equal(round(metafits$meta_estimates$ci.lb - metafits$meta_estimates$ci.ub, 7),
               c(-1.7918459, -1.3509899, -1.1003282, -0.9808477, -0.9434358,
                 -0.9775063, -1.0682122, -1.2486244, -1.4946603, -1.9755082))



  # Now with both terms
  grid <- data.frame(x0 = seq(0, 1, by = .1), x1 = seq(0, 1, by = .1))
  metafits <- metagam(fits, grid = grid, terms = c("s(x0)", "s(x1)"))
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, c("s(x0)", "s(x1)"))

  expect_equal(round(metafits$meta_estimates$se, 7),
               c(0.422956, 0.4583951, 0.333303, 0.3535503, 0.2726138, 0.2915958,
                 0.2386662, 0.257261, 0.2234757, 0.2403507, 0.2193465, 0.2413504,
                 0.2229785, 0.254857, 0.2377513, 0.2841205, 0.2722828, 0.3296066,
                 0.3344465, 0.389528, 0.4261187, 0.5113922))

  expect_equal(round(metafits$meta_estimates$estimate, 7),
               c(-0.2223819, -1.9084594, -0.1482919, -1.548928, -0.0656373,
                 -1.3110456, 0.0175923, -0.8222796, 0.0823638, -0.537348, 0.1150437,
                 0.0744287, 0.1144476, 0.5902795, 0.0861929, 0.9396627, 0.0322843,
                 1.5175837, -0.0482097, 2.2100833, -0.1474295, 2.8868289))



  # Use link function
  metafits <- metagam(fits, grid_size = 10, type = "link")
  expect_s3_class(metafits, "metagam")

  expect_equal(round(metafits$meta_estimates$estimate, 7),
               c(5.4670439, 5.7723492, 6.1029476, 6.4767596, 6.8171863, 7.7584157,
                 7.9920007, 8.6347526, 9.4090775, 10.1784335, 5.4603792, 5.8320497,
                 6.1783152, 6.556845, 6.9222145, 7.7935504, 8.0858335, 8.7259267,
                 9.4847858, 10.2147992, 5.4925588, 5.9016763, 6.2565914, 6.6371903,
                 7.0195478, 7.8306297, 8.1738725, 8.8071819, 9.5560275, 10.2656416,
                 5.5356558, 5.9653099, 6.322799, 6.7050007, 7.0960468, 7.8684949,
                 8.2413983, 8.8659739, 9.6132786, 10.3179346, 5.5606556, 6.0021303,
                 6.3592153, 6.7440403, 7.1391939, 7.8908995, 8.2761389, 8.8922455,
                 9.6449272, 10.3525207, 5.5540881, 6.0022618, 6.3596585, 6.7476161,
                 7.1432022, 7.8869917, 8.2736605, 8.8826804, 9.6445652, 10.3573547,
                 5.5142751, 5.966043, 6.3264453, 6.7159831, 7.1045934, 7.8562124,
                 8.2337472, 8.8372124, 9.6115683, 10.3288324, 5.4568217, 5.9079231,
                 6.2713917, 6.6570928, 7.0246674, 7.8106076, 8.1638039, 8.7639087,
                 9.5548457, 10.276442, 5.4022208, 5.8411981, 6.201669, 6.5759941,
                 6.9069528, 7.7603956, 8.0706786, 8.6709875, 9.4816958, 10.2119747,
                 5.3704719, 5.7720239, 6.1198474, 6.4802027, 6.7701134, 7.7079963,
                 7.9639806, 8.5676176, 9.3974111, 10.1488514))

  expect_equal(round(metafits$meta_estimates$se, 7),
               c(0.6212525, 0.5227932, 0.4784549, 0.4677086, 0.4712104, 0.4680064,
                 0.4766576, 0.5039774, 0.5374773, 0.6477414, 0.5473416, 0.4442344,
                 0.3937846, 0.3781297, 0.3781147, 0.3779597, 0.3899214, 0.4234775,
                 0.4655392, 0.5801284, 0.5067553, 0.3985886, 0.3432005, 0.3227193,
                 0.3189348, 0.321146, 0.3362372, 0.374445, 0.4243672, 0.5413418,
                 0.4885136, 0.3766332, 0.3184565, 0.2948228, 0.2886396, 0.2922408,
                 0.3091526, 0.3500004, 0.405287, 0.5232533, 0.481306, 0.3677175,
                 0.3084393, 0.2831542, 0.2761019, 0.2807459, 0.2985007, 0.3406904,
                 0.3987934, 0.517028, 0.4798064, 0.3663818, 0.3070064, 0.2810508,
                 0.2742238, 0.2798647, 0.2978336, 0.3405659, 0.3993471, 0.5175652,
                 0.4846057, 0.3730722, 0.3147795, 0.2895572, 0.2841506, 0.2907002,
                 0.3078789, 0.3502918, 0.4073627, 0.5251356, 0.5031818, 0.3949292,
                 0.3397313, 0.3176375, 0.3150789, 0.3219654, 0.336287, 0.3769985,
                 0.4289029, 0.545014, 0.5479021, 0.4431684, 0.3930118, 0.3759822,
                 0.3768565, 0.3833389, 0.3925727, 0.4301771, 0.4731965, 0.5856043,
                 0.628013, 0.5251978, 0.4806462, 0.4684822, 0.4718402, 0.4779048,
                 0.4816784, 0.5155386, 0.5481428, 0.6546901))



})

test_that("metagam runs with categorical covariates", {
  models <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$z <- factor(sample(1:3, size = nrow(dat), replace = TRUE))
    b <- gam(y ~ s(x0, bs = "cr") + z, data = dat)
    strip_rawdata(b)
  })
  expect_s3_class(metagam(models, grid_size = 10), "metagam")

  models <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$z <- factor(sample(1:3, size = nrow(dat), replace = TRUE))
    b <- gam(y ~ s(x0, by = z, bs = "cr"), data = dat)
    strip_rawdata(b)
  })
  expect_s3_class(metagam(models, grid_size = 10), "metagam")

  models <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$z <- ordered(sample(1:3, size = nrow(dat), replace = TRUE))
    b <- gam(y ~ s(x0, bs = "cr") + z, data = dat)
    strip_rawdata(b)
  })
  expect_s3_class(metagam(models, grid_size = 10), "metagam")

  models <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$grp <- ordered(sample(1:3, size = nrow(dat), replace = TRUE))
    dat$z <- ordered(sample(1:3, size = nrow(dat), replace = TRUE))
    b <- gam(y ~ s(x0, by = grp, bs = "cr") + z, data = dat)
    strip_rawdata(b)
  })
  expect_s3_class(metagam(models, grid_size = 10), "metagam")
})

test_that("metagam fails with wrong input", {
  expect_error(metagam(fits, grid_size = 10, type = "knil"))
  expect_error(metagam(fits, grid_size = 10, terms = "abbb"))

})

test_that("metagam works with tensor interactions", {
  set.seed(123)
  datasets <- lapply(1:5, function(x) gamSim(eg = 2, n = 50, verbose = FALSE)$data)

  fits <- lapply(datasets, function(dat){
    b <- gam(y ~ te(x, z), data = dat)
    strip_rawdata(b)
  })

  metafit <- metagam(fits, grid_size = 10)
  expect_s3_class(metafit, "metagam")
  expect_equal(round(metafit$meta_estimates$ci.lb[1:4], 10),
               c(-0.2802061066, -0.0279839076, -0.1800437287, -0.290758912))

  expect_equal(round(metafit$meta_estimates$estimate[40:50], 10),
               c(-0.0618375179, 0.4791554907, 0.4050419988, 0.3156138636, 0.272017788,
                 0.1986136027, 0.0434866897, -0.1151385155, -0.1873485645, 0.0244868197,
                 0.2823631963))

  expect_error(metagam(fits, grid_size = 10, nsim = 100))


})


test_that("metagam accepts strange variable names", {
  fits <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$var__with.many3things <- dat$x1
    b <- gam(y ~ s(x0, bs = "cr") + s(var__with.many3things, bs = "cr"), data = dat)
    strip_rawdata(b)
  })

  expect_s3_class(metagam(fits, nsim = 3), "metagam")

})
