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
  metafits <- metagam(fits, grid_size = 10, nsim = 3)
  expect_s3_class(metafits, "metagam")
  expect_equal(
    metafits$term_list,
    list(`s(x0)` = structure(list(xvars = "x0", terms = "s(x0)"), row.names = "s(x0)", class = "data.frame"))
    )
  expect_equal(round(metafits$meta_models$`s(x0)`$predictions$estimate, 7),
               c(-0.2219465, -0.1391364, -0.0463822, 0.0422637, 0.1011218, 0.1185628,
                 0.0983026, 0.04702, -0.0372666, -0.1462716))

  expect_equal(round(metafits$meta_models$`s(x0)`$predictions$se, 7),
               c(0.4223139, 0.324793, 0.2629479, 0.2321011, 0.2207427, 0.2203724,
                 0.2310822, 0.262168, 0.3253867, 0.4249495))

  # Now with second term
  metafits <- metagam(fits, grid_size = 10, terms = "s(x1)")
  expect_s3_class(metafits, "metagam")
  expect_equal(
    metafits$term_list,
    list(`s(x1)` = structure(list(xvars = "x1", terms = "s(x1)"), row.names = "s(x1)", class = "data.frame"))
    )

  expect_equal(round(metafits$meta_models$`s(x1)`$predictions$estimate, 7),
               c(-1.9025406, -1.5469589, -1.1829086, -0.742646, -0.2859131,
                 0.4071346, 0.7688556, 1.3755142, 2.0831141, 2.8655438))

  expect_equal(round(metafits$meta_models$`s(x1)`$predictions$se, 7),
               c(0.4571119, 0.3446466, 0.2807011, 0.2502209, 0.2406768, 0.2493684,
                 0.2725081, 0.3185325, 0.3812979, 0.5039654))


  # Now with both terms
  grid <- data.frame(x0 = seq(0, 1, by = .2), x1 = seq(0, 1, by = .2))
  metafits <- metagam(fits, grid = grid, terms = c("s(x0)", "s(x1)"))
  expect_s3_class(metafits, "metagam")
  expect_equal(
    metafits$term_list,
    list(`s(x0)` = structure(list(xvars = "x0", terms = "s(x0)"), row.names = "s(x0)", class = "data.frame"),
         `s(x1)` = structure(list(xvars = "x1", terms = "s(x1)"), row.names = "s(x1)", class = "data.frame"))
    )

  expect_equal(
    metafits$meta_models$`s(x0)`$predictions,
    structure(list(x1 = c(0, 0, 0, 0, 0, 0), x0 = c(0, 0.2, 0.4,
                                                    0.6, 0.8, 1), estimate = c(-0.222381918040233, -0.0656372750190169,
                                                                               0.0823637929417119, 0.114447602667918, 0.0322842862355441, -0.147429526724377
                                                    ), se = c(0.422956038761128, 0.272613840438719, 0.223475741957481,
                                                              0.222978498276503, 0.272282784175936, 0.426118709797298)), class = "data.frame", row.names = c("1",
                                                                                                                                                             "2", "3", "4", "5", "6"))
  )

  # Use link function
  metafits <- metagam(fits, grid_size = 10, type = "link")
  expect_s3_class(metafits, "metagam")

  expect_equal(
    round(metafits$meta_models$predictions[30:33, ], 7),
    structure(list(x0 = c(0.9988826, 0.0006248, 0.1115423, 0.2224599
    ), x1 = c(0.2220238, 0.3324399, 0.3324399, 0.3324399), estimate = c(6.1198474,
                                                                        6.4767596, 6.556845, 6.6371903), se = c(0.4806462, 0.4677086,
                                                                                                                0.3781297, 0.3227193)), row.names = c("30", "31", "32", "33"), class = "data.frame")
  )



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
  expect_equal(
    round(metafit$meta_models$`te(x,z)`$predictions, 7)[95:97,],
    structure(list(x = c(0.4452226, 0.5555015, 0.6657805), z = c(0.992944,
                                                                 0.992944, 0.992944), estimate = c(0.2823632, 0.5081124, 0.4287619
                                                                 ), se = c(0.3568622, 0.3613073, 0.386653)), row.names = c("95",
                                                                                                                           "96", "97"), class = "data.frame")
    )

  metafit <- metagam(fits, grid_size = 10, type = "link")
  expect_equal(
    round(metafit$meta_models$predictions[22:25, ], 7),
    structure(list(x = c(0.1143859, 0.2246648, 0.3349437, 0.4452226
    ), z = c(0.2210162, 0.2210162, 0.2210162, 0.2210162), estimate = c(1.1678738,
                                                                       1.1519874, 1.0549315, 0.930571), se = c(0.3005395, 0.2596968,
                                                                                                               0.2349767, 0.2238625)), row.names = c("22", "23", "24", "25"), class = "data.frame")
  )
})


test_that("metagam accepts strange variable names", {
  fits <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$var__with.many3things <- dat$x1
    b <- gam(y ~ s(x0, bs = "cr") + s(var__with.many3things, bs = "cr"), data = dat)
    strip_rawdata(b)
  })

  expect_s3_class(metagam(fits, nsim = 3, grid_size = 5), "metagam")

})

test_that("plots work", {
  metafits <- metagam(fits, grid_size = 10, nsim = 3)
  expect_invisible(plot(metafits))
  expect_invisible(plot(metafits, ci = "both"))
  expect_invisible(plot(metafits, ci = "simultaneous"))
  expect_invisible(plot(metafits, ci = "pointwise"))

  metafits <- metagam(fits, grid_size = 10, nsim = 3, type = "link")
  expect_invisible(plot(metafits))
  expect_invisible(plot(metafits, ci = "both"))
  expect_invisible(plot(metafits, ci = "simultaneous"))
  expect_invisible(plot(metafits, ci = "pointwise"))

  metafits <- metagam(fits, grid_size = 10, nsim = 3, type = "response")
  expect_invisible(plot(metafits))
  expect_invisible(plot(metafits, ci = "both"))
  expect_invisible(plot(metafits, ci = "simultaneous"))
  expect_invisible(plot(metafits, ci = "pointwise"))
})
