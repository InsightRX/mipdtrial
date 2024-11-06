test_that("weight-based dosing works", {
  covs <- list(WT = PKPDsim::new_covariate(70))
  init_reg <- create_initial_regimen_design(
    method = weight_based_starting_dose,
    mgkg = 15,
    regimen = list(
      interval = 12,
      t_inf = 1.5,
      n = 10,
      type = "infusion"
    )
  )
  out <- weight_based_starting_dose(covs, list(initial_regimen = init_reg))
  expect_true("regimen" %in% class(out))
  expect_equal(length(out$dose_amts), init_reg$regimen$n)
  expect_equal(out$dose_amts[1], 15 * 70)
  expect_equal(out$t_inf, rep(init_reg$regimen$t_inf, 10))
  expect_equal(out$type, rep("infusion", 10))
})

test_that("errors on missing weight", {
  covs <- list(HT = PKPDsim::new_covariate(170))
  init_reg <- create_initial_regimen_design(
    method = weight_based_starting_dose,
    mgkg = 15,
    regimen = list(
      interval = 12,
      t_inf = 1.5,
      n = 10,
      type = "infusion"
    )
  )
  expect_error(
    weight_based_starting_dose(covs, list(initial_regimen = init_reg)),
    "WT"
  )
})

test_that("errors on missing mg/kg", {
  covs <- list(WT = PKPDsim::new_covariate(170))
  init_reg <- create_initial_regimen_design(
    method = weight_based_starting_dose,
    regimen = list(
      interval = 12,
      t_inf = 1.5,
      n = 10,
      type = "infusion"
    )
  )
  expect_error(
    weight_based_starting_dose(covs, list(initial_regimen = init_reg)),
    "mgkg"
  )
})

