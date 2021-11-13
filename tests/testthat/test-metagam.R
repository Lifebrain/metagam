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

  # Now with second term
  metafits <- metagam(fits, grid_size = 10, terms = "s(x1)")
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, "s(x1)")



  # Now with both terms
  grid <- data.frame(x0 = seq(0, 1, by = .1), x1 = seq(0, 1, by = .1))
  metafits <- metagam(fits, grid = grid, terms = c("s(x0)", "s(x1)"))
  expect_s3_class(metafits, "metagam")
  expect_equal(metafits$terms, c("s(x0)", "s(x1)"))



  # Use link function
  metafits <- metagam(fits, grid_size = 10, type = "link")
  expect_s3_class(metafits, "metagam")



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


test_that("metagam accepts strange variable names", {
  fits <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$var__with.many3things <- dat$x1
    b <- gam(y ~ s(x0, bs = "cr") + s(var__with.many3things, bs = "cr"), data = dat)
    strip_rawdata(b)
  })

  expect_s3_class(metagam(fits, nsim = 3), "metagam")

})
