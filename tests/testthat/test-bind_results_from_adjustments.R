test_that("bind_results_from_adjustments works correctly", {

  # Setup test data
  regimen <- list(dose_times = c(0, 24, 48, 72))
  adjust_at_dose <- c(2, 3)
  j <- 1
  auc_final <- 100
  trough_final <- 5
  dose_before_update <- 500
  
  # Test case 1: out has new_dose and new_interval
  out1 <- list(new_dose = 600, new_interval = 12)
  
  result1 <- bind_results_from_adjustments(
    out1, j, regimen, adjust_at_dose, 
    dose_before_update, auc_final, trough_final
  )
  
  expect_equal(result1$t, 24)
  expect_equal(result1$dose_before_update, 600)
  expect_equal(result1$interval_before_update, 12)
  expect_equal(result1$auc_before_update, 100)
  expect_equal(result1$trough_before_update, 5)
  expect_true(is.na(result1$dose_update))
  expect_true(is.na(result1$t_adjust))
  
  # Test case 2: out has NULL new_dose and new_interval
  out2 <- list(new_dose = NULL, new_interval = NULL)
  
  result2 <- bind_results_from_adjustments(
    out2, j, regimen, adjust_at_dose, 
    dose_before_update, auc_final, trough_final
  )
  
  expect_equal(result2$t, 24)
  expect_true(is.na(result2$dose_before_update))
  expect_true(is.na(result2$interval_before_update))
  expect_equal(result2$auc_before_update, 100)
  expect_equal(result2$trough_before_update, 5)
  
  # Test case 3: empty adjust_at_dose
  adjust_at_dose_empty <- c()
  auc_final_vec <- c(100, 110)
  
  result3 <- bind_results_from_adjustments(
    out1, j, regimen, adjust_at_dose_empty, 
    dose_before_update, auc_final_vec, trough_final
  )
  
  expect_equal(length(result3$t), 2)
  expect_true(all(is.na(result3$t)))
  expect_equal(result3$dose_before_update, c(600, 600))
  expect_equal(result3$auc_before_update, c(100, 110))
  
  # Test case 4: verify data.frame structure
  expect_s3_class(result1, "data.frame")
  expected_cols <- c("t", "dose_update", "t_adjust", "dose_before_update", 
                     "interval_before_update", "auc_before_update", "trough_before_update")
  expect_equal(names(result1), expected_cols)
})
