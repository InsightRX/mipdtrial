test_that("can get t>mic at steady state", {
  md <- list(
    prediction = list(
      quantity_from_variable = list(
        t_gt_mic = list(
          variable = "TGTMIC",
          per_interval = TRUE,
          diff = TRUE,
          percentage_time = TRUE,
          rounding = 0L
        ),
        t_gt_4mic = list(
          variable = "TGT4MIC",
          per_interval = TRUE,
          diff = TRUE,
          percentage_time = TRUE,
          rounding = 0L
        )
      )
    )
  )
  var <- "t_gt_mic"
  times <- c(112, 120)
  comp <- "obs"
  sim <- data.frame(
    t = c(112, 120),
    TGTMIC = c(107.36499999991, 115.364999999898),
    TGT4MIC = c(28.6373333333247, 31.3939999999893),
    comp = as.factor(c("obs", "obs"))
  ) # simplified simulation results

  res1 <- get_quantity_from_variable("t_gt_mic", sim, md, times, comp)
  expect_equal(tail(res1, 1), 100)
  res2 <- get_quantity_from_variable("t_gt_4mic", sim, md, times, comp)
  expect_equal(tail(res2, 1), 34)
})

test_that("Calculates correctly when corrected for interval, especially last 2 values at end of regimen", {
  md <- list(
    prediction = list(
      quantity_from_variable = list(
        auc12_hcr35 = list(
          variable = "AUC_HCR35",
          per_interval = TRUE,
          diff = TRUE,
          percentage_time = FALSE,
          correct_interval = 12,
          rounding = 1L
        )
      )
    )
  )
  var <- "auc12_hcr35"
  times <- c(0, 24, 48, 72, 120)
  comp <- "obs"
  sim <- data.frame(
    t = c(0, 24, 48, 72, 120),
    AUC_HCR35 = cumsum(rep(20, 5)),
    comp = rep("obs", 5)
  )

  res1 <- get_quantity_from_variable("auc12_hcr35", sim, md, times, comp)
  expect_equal(res1, c(0, 10, 10, 10, 5))
})

