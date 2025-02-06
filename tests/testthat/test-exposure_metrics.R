test_that("calc_concentration_from_regimen: correct peak calculated", {
  peak_target <- create_target_design(
    targettype = "peak",
    targetmin = 10,
    targetmax = 20,
    at = 6,
    anchor = "day"
  )
  regimen <- PKPDsim::new_regimen(
    amt = 200,
    n = 20,
    interval = 12,
    type = "infusion"
  )
  expect_equal(
    calc_concentration_from_regimen(
      regimen = regimen,
      parameters = list(CL = 5, V = 50),
      model = mod_1cmt_iv, # defined in setup
      target = peak_target
    ),
    5.4471447
  )
})

test_that("calc_concentration_from_regimen: correct trough calculated", {
  trough_target <- create_target_design(
    targettype = "trough",
    targetmin = 1,
    targetmax = 5,
    at = 6,
    anchor = "day"
  )
  regimen <- PKPDsim::new_regimen(
    amt = 200,
    n = 20,
    interval = 12,
    type = "infusion"
  )
  expect_equal(
    calc_concentration_from_regimen(
      regimen = regimen,
      parameters = list(CL = 5, V = 50),
      model = mod_1cmt_iv,
      target = trough_target
    ),
    1.81319695
  )
})

test_that("calc_auc_from_sim gets AUC", {
  sim_output <- data.frame(
    t = rep(c(0, 24, 48), 4),
    comp = rep(c(1, 2, 3, "obs"), each = 3),
    y = c(0, 10, 20, 0, 2, 4, 0, 100, 300, 0, 10, 20)
  )
  expect_equal(calc_auc_from_sim(sim_output, 3, c(24, 48), "auc24"), c(100, 200))
  expect_equal(calc_auc_from_sim(sim_output, 2, c(24, 48), "auc24"), c(2, 2))
})

test_that("when passed one obs, calc_auc_from_sim give cumulative AUC", {
  sim_output <- data.frame(
    t = rep(96, 4),
    comp = c(1, 2, 3, "obs"),
    y = c(20, 50, 1000, 20)
  )
  expect_equal(calc_auc_from_sim(sim_output, 3, 96, "auc_cum"), 1000)
  expect_equal(calc_auc_from_sim(sim_output, 2, 96, "auc_cum"), 50)
})

test_that("calc_auc_from_regimen: parameter mismatch raises error", {
  expect_error(
    calc_auc_from_regimen(
      regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
      parameters = list(CL = 5, V = 50), # missing V2, Q
      model = pkbusulfanmccune::model(),
      target_time = c(48, 72)
    ),
    "Model/parameter mismatch"
  )
})

test_that("calc_auc_from_regimen: correct AUC calculated", {
  # parameters as list
  target <- create_target_design(
    targettype = "auc24",
    targetvalue = 10,
    time = c(72)
  )
  expect_equal(
    calc_auc_from_regimen(
      regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
      parameters = list(CL = 5, V = 50),
      model = mod_1cmt_iv,
      target = target
    ),
    19.984296
  )
  # parameters as data.frame
  expect_equal(
    calc_auc_from_regimen(
      regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
      parameters = data.frame(ID = 1, CL = 5, V = 50),
      model = mod_1cmt_iv, # defined in setup
      target = target
    ),
    19.984296
  )
  # parameters as named vector
  expect_equal(
    calc_auc_from_regimen(
      regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
      parameters = c(CL = 5, V = 50),
      model = mod_1cmt_iv, # defined in setup
      target = target
    ),
    19.984296
  )
})

test_that("handles IOV correctly", {
  regimen <- PKPDsim::new_regimen(
    amt = 200,
    interval = 24,
    times = 4,
    t_inf = 3,
    type = "infusion"
  )
  mod <- pkbusulfanmccune::model()
  pars <- pkbusulfanmccune::parameters()
  covs <- list(AGE = 15, WT = 70, HT = 150, SEX = 0, T_CL_EFF = 0)
  target_a <- create_target_design(
    targettype = "cum_auc", targetvalue = 10, time = c(24)
  )
  target_b <- create_target_design(
    targettype = "cum_auc", targetvalue = 10, time = c(72)
  )
  result1a <- calc_auc_from_regimen(
    regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
    parameters = pars,
    model = mod,
    target = target_a,
    covariates = covs
  )
  result1b <- calc_auc_from_regimen(
    regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
    parameters = pars,
    model = mod,
    target_b,
    covariates = covs
  )
  pars$kappa_CL_3 <- 2
  pars$kappa_CL_4 <- 2
  result2a <- calc_auc_from_regimen(
    regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
    parameters = pars,
    model = mod,
    target = target_a,
    covariates = covs
  )
  result2b <- calc_auc_from_regimen(
    regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
    parameters = pars,
    model = mod,
    target = target_b,
    covariates = covs
  )
  expect_equal(result1a, result2a) # iov is the same on day 1
  expect_false(result2a == result2b) # iov is different after day 3
})

test_that("calc_time_to_target: correct time to target calculated", {
  mod_1cmt_iv <- PKPDsim::new_ode_model("pk_1cmt_iv_auc")
  regimen <- PKPDsim::new_regimen(
    amt = 150,
    n = 20,
    interval = 12,
    type = "infusion"
  )

  trough_target <- create_target_design(
    targettype = "trough",
    targetmin = 1,
    targetmax = 5,
    at = 6,
    anchor = "day"
  )
  time_to_target_trough <- calc_time_to_target(
    regimen = regimen,
    target_design = trough_target,
    auc_comp = NULL,
    model = mod_1cmt_iv,
    covariates = NULL,
    parameters = list(CL = 5, V = 50)
  )
  expect_equal(time_to_target_trough, 13)

  trough_target_point <- create_target_design(
    targettype = "trough",
    targetvalue = 1.5,
    at = 6,
    anchor = "day"
  )
  time_to_target_trough_point <- calc_time_to_target(
    regimen = regimen,
    target_design = trough_target_point,
    auc_comp = NULL,
    model = mod_1cmt_iv,
    covariates = NULL,
    parameters = list(CL = 5, V = 50)
  )

  expect_equal(time_to_target_trough_point, 13)

  auc24_target <- create_target_design(
    targettype = "auc24",
    targetmin = 55,
    targetmax = 75,
    at = 6,
    anchor = "day"
  )
  time_to_target_auc24 <- calc_time_to_target(
    regimen = regimen,
    target_design = auc24_target,
    auc_comp = attr(mod_1cmt_iv, "size"),
    model = mod_1cmt_iv,
    covariates = NULL,
    parameters = list(CL = 5, V = 50)
  )
  expect_equal(time_to_target_auc24, 25)

  auc12_target <- create_target_design(
    targettype = "auc12",
    targetmin = 25,
    targetmax = 40,
    at = 6,
    anchor = "day"
  )
  time_to_target_auc12 <- calc_time_to_target(
    regimen = regimen,
    target_design = auc12_target,
    auc_comp = attr(mod_1cmt_iv, "size"),
    model = mod_1cmt_iv,
    covariates = NULL,
    parameters = list(CL = 5, V = 50)
  )
  expect_equal(time_to_target_auc12, 13)
})

test_that("calc_time_to_target: return NA when unsupported type", {
  mod_1cmt_iv <- PKPDsim::new_ode_model("pk_1cmt_iv_auc")
  regimen <- PKPDsim::new_regimen(
    amt = 150,
    n = 20,
    interval = 12,
    type = "infusion"
  )
  unsupported_target <- create_target_design(
    targettype = "cum_auc",
    targetmin = 1,
    targetmax = 5,
    at = 6,
    anchor = "day"
  )
  time_to_target_unsupported <- calc_time_to_target(
    regimen = regimen,
    target_design = unsupported_target,
    auc_comp = NULL,
    model = mod_1cmt_iv,
    covariates = NULL,
    parameters = list(CL = 5, V = 50)
  )
  expect_equal(is.na(time_to_target_unsupported), TRUE)
})
