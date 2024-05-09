test_that("Update regimen updates correctly", {
  original <- PKPDsim::new_regimen(
    amt = 1,
    n = 5,
    interval = 24,
    type = "oral"
  )
  new_reg <- update_regimen(original, 3, 4)
  expect_true(inherits(new_reg, "regimen"))
  expect_equal(new_reg$dose_amts, c(1, 1, 1, 3, 3))
  constant_cols <- setdiff(names(original), "dose_amts")
  expect_true(
    all(unlist(lapply(constant_cols, \(x) all(new_reg[[x]] == original[[x]]))))
  )
})
