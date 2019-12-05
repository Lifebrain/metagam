library(ggplot2)

metafits <- lapply(1:10, function(i){
  dat <- mgcv::gamSim(n = 1000, scale = 7)
  dat1 <- dat[1:300, ]
  dat2 <- dat[301:500, ]
  dat3 <- dat[501:1000, ]

  metafits <- lapply(
    list(c(TRUE, TRUE), c(FALSE, TRUE), c(FALSE, FALSE)),
    function(x){
      models <- lapply(list(dat1, dat2, dat3), function(d){
        model <- mgcv::gam(y ~ s(x2), data = d, method = "REML")
        singlegam(model, nmc = 1000, freq = x[[1]], unconditional = x[[2]])
      })
      metafit <- metagam(models)

      # Fit with full data. Need this each time because of grid
      fullfit <- mgcv::gam(y ~ s(x2), data = dat, method = "REML")
      pred_df <- metafit$all_posteriors$x2[metafit$all_posteriors$x2$quantity == "mean", "predictor_value", drop = FALSE]
      names(pred_df) <- "x2"
      fullpred <- stats::predict(fullfit, newdata = pred_df)

      df <- reshape(metafit$all_posteriors[["x2"]], v.names = "value",
                    idvar = "predictor_value", timevar = "quantity",
                    direction = "wide")
      df$value.full <- fullpred
      df$condition <- paste0(x, collapse = "_")

      return(df)
    })

  metafits <- do.call(rbind, metafits)
  metafits$iteration <- i

  return(metafits)
})

metafits <- do.call(rbind, metafits)

# Interpolate mean onto common grid
x2_grid <- seq(from = 0, to = 1, by = .01)

df <- split(metafits, metafits[, c("iteration", "condition")])
df <- lapply(df, function(subdf){
  mean_appr <- approx(x = subdf$predictor_value, y = subdf$value.mean, xout = x2_grid, rule = 2)$y
  full_appr <- approx(x = subdf$predictor_value, y = subdf$value.full, xout = x2_grid, rule = 2)$y
  data.frame(
    iteration = unique(subdf$iteration),
    condition = unique(subdf$condition),
    x = x2_grid,
    y_mean = mean_appr,
    y_full = full_appr
  )
})

df <- do.call(rbind, df)
rownames(df) <- NULL

library(dplyr)
library(tidyr)
df2 <- df %>%
  group_by(condition, x) %>%
  summarise_at(vars(y_mean, y_full), mean) %>%
  pivot_longer(cols = c("y_mean", "y_full"))


ggplot(df2, aes(x = x, y = value, group = name, color = name)) +
  geom_line() +
  facet_wrap(vars(condition))


