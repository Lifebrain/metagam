library(mgcv)
# Create a basic model with mgcv
set.seed(123)
ndat <- 3
n <- 100

test_predictions <- function(fullfits, stripfits){
  mapply(function(x, y) {
    # Check that strip_rawdata has removed something
    expect_gt(utils::object.size(x), utils::object.size(y))

    # Check that predictions are identical
    grid <- data.frame(x0 = runif(10), x1 = runif(10))
    fullpred <- predict(x, newdata = grid)
    strippred <- predict(y, newdata = grid)
    expect_equal(fullpred, strippred)

    fullpred <- predict(x, newdata = grid, se.fit = TRUE, type = "iterms")
    strippred <- predict(y, newdata = grid, se.fit = TRUE, type = "iterms")
    expect_equal(fullpred, strippred)

    return(0)
  }, fullfits, stripfits)
}

test_that("strip_rawdata.gam works", {
  fullfits <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    gam(y ~ s(x0, bs = 'cr') + s(x1, bs = 'cr'), data = dat)
  })

  stripfits <- lapply(fullfits, strip_rawdata)
  test_predictions(fullfits, stripfits)
  lapply(stripfits, function(x) expect_s3_class(x, "striprawdata"))
})

test_that("strip_rawdata.bam works", {
  fullfits <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    bam(y ~ s(x0, bs = 'cr') + s(x1, bs = 'cr'), data = dat)
  })

  stripfits <- lapply(fullfits, strip_rawdata)
  test_predictions(fullfits, stripfits)
  lapply(stripfits, function(x) expect_s3_class(x, "striprawdata"))
})

test_that("strip_rawdata.gamm works", {

  fullfits <- lapply(1:ndat, function(x){
    dat <- gamSim(n = n, verbose = FALSE)
    dat$ID <- sample(1:3, size = n, replace = TRUE)
    b <- gamm(y ~ s(x0, bs = 'cr') + s(x1, bs = 'cr'), data = dat,
              random = list(ID =~ 1))
  })

  stripfits <- lapply(fullfits, strip_rawdata)

  test_predictions(lapply(fullfits, function(x) x$gam), stripfits)
  lapply(stripfits, function(x) expect_s3_class(x, "striprawdata"))

})


test_that("strip_rawdata.gamm4 works", {
  fullfits_gamm4 <- readRDS("../testdata/fullfits_gamm4.rda")

  stripfits <- lapply(fullfits_gamm4, strip_rawdata)

  test_predictions(lapply(fullfits_gamm4, function(x) x$gam), stripfits)
  lapply(stripfits, function(x) expect_s3_class(x, "striprawdata"))
})


