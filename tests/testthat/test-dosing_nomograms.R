test_that("weight-based dosing works", {
  covs <- list(WT = PKPDsim::new_covariate(70))
  cov_map <- c(WT = "tbw")
  init_reg <- create_initial_regimen_design(
    method = weight_based_starting_dose,
    mgkg = 15,
    dosing_weight = "tbw",
    regimen = list(
      interval = 12,
      t_inf = 1.5,
      n = 10,
      type = "infusion"
    )
  )
  design <- list(initial_regimen = init_reg)
  out <- weight_based_starting_dose(covs, design, cov_map)
  expect_true("regimen" %in% class(out))
  expect_equal(length(out$dose_amts), init_reg$regimen$n)
  expect_equal(out$dose_amts[1], 15 * 70)
  expect_equal(out$t_inf, rep(init_reg$regimen$t_inf, 10))
  expect_equal(out$type, rep("infusion", 10))
})

test_that("errors on missing weight covariate", {
  covs <- list(HT = PKPDsim::new_covariate(170))
  cov_map <- c(WT = "tbw", HT = "height")
  init_reg <- create_initial_regimen_design(
    method = weight_based_starting_dose,
    mgkg = 15,
    dosing_weight = "tbw",
    regimen = list(
      interval = 12,
      t_inf = 1.5,
      n = 10,
      type = "infusion"
    )
  )
  design <- list(initial_regimen = init_reg)
  expect_error(
    weight_based_starting_dose(covs, design, cov_map),
    "Dosing weight not available in data set"
  )
})

test_that("errors on missing mg/kg", {
  covs <- list(WT = PKPDsim::new_covariate(170))
  cov_map <- c(WT = "tbw")
  init_reg <- create_initial_regimen_design(
    method = weight_based_starting_dose,
    dosing_weight = "tbw",
    regimen = list(
      interval = 12,
      t_inf = 1.5,
      n = 10,
      type = "infusion"
    )
  )
  design <- list(initial_regimen = init_reg)
  expect_error(
    weight_based_starting_dose(covs, design, cov_map),
    "mgkg"
  )
})

test_that("errors on missing dosing_weight", {
  covs <- list(WT = PKPDsim::new_covariate(170))
  cov_map <- c(WT = "tbw")
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
  design <- list(initial_regimen = init_reg)
  expect_error(
    weight_based_starting_dose(covs, design, cov_map),
    "dosing_weight ="
  )
})

test_that("errors on missing dosing_weight in cov_mapping", {
  covs <- list(WT = PKPDsim::new_covariate(170))
  cov_map <- c(WT = "tbw")
  init_reg <- create_initial_regimen_design(
    method = weight_based_starting_dose,
    mgkg = 15,
    dosing_weight = "ibw",
    regimen = list(
      interval = 12,
      t_inf = 1.5,
      n = 10,
      type = "infusion"
    )
  )
  design <- list(initial_regimen = init_reg)
  expect_error(
    weight_based_starting_dose(covs, design, cov_map),
    "dosing_weight covariate missing"
  )
})

