test_that("Update regimen updates correctly (new_dose)", {
  original <- PKPDsim::new_regimen(
    amt = 1,
    n = 5,
    interval = 24,
    type = "oral"
  )
  new_reg <- update_regimen(original, new_dose = 3, dose_update_number = 4)
  expect_true(inherits(new_reg, "regimen"))
  expect_equal(new_reg$dose_amts, c(1, 1, 1, 3, 3))
  constant_cols <- setdiff(names(original), "dose_amts")
  expect_true(
    all(unlist(lapply(constant_cols, \(x) all(new_reg[[x]] == original[[x]]))))
  )
})

test_that("Update regimen updates correctly (new_interval)", {
  original <- PKPDsim::new_regimen(
    amt = 1,
    n = 5,
    interval = 24,
    type = "oral"
  )
  new_reg <- update_regimen(original, new_interval = 12, dose_update_number = 3)
  expect_true(inherits(new_reg, "regimen"))
  expect_equal(new_reg$dose_amts, c(1, 1, 1, 1, 1))
  constant_cols <- setdiff(names(original), "dose_amts")
  expect_equal(new_reg$dose_times, c(0, 24, 48, 60, 72))
})

test_that("Update regimen updates correctly (new_dose + new_interval)", {
  original <- PKPDsim::new_regimen(
    amt = 1,
    n = 5,
    interval = 24,
    type = "oral"
  )
  new_reg <- update_regimen(original, new_dose = 3, new_interval = 12, dose_update_number = 3)
  expect_true(inherits(new_reg, "regimen"))
  expect_equal(new_reg$dose_amts, c(1, 1, 3, 3, 3))
  constant_cols <- setdiff(names(original), "dose_amts")
  expect_equal(new_reg$dose_times, c(0, 24, 48, 60, 72))
})
