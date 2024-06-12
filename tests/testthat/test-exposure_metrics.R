test_that("calc_auc_from_sim gets AUC", {
  sim_output <- data.frame(
    t = rep(c(0, 24, 48), 4),
    comp = rep(c(1, 2, 3, "obs"), each = 3),
    y = c(0, 10, 20, 0, 2, 4, 0, 100, 300, 0, 10, 20)
  )
  expect_equal(calc_auc_from_sim(sim_output, 3), c(100, 200))
  expect_equal(calc_auc_from_sim(sim_output, 2), c(2, 2))
})

test_that("when passed one obs, calc_auc_from_sim give cumulative AUC", {
  sim_output <- data.frame(
    t = rep(96, 4),
    comp = c(1, 2, 3, "obs"),
    y = c(20, 50, 1000, 20)
  )
  expect_equal(calc_auc_from_sim(sim_output, 3), 1000)
  expect_equal(calc_auc_from_sim(sim_output, 2), 50)
})

test_that("calc_auc_from_sim: parameter mismatch raises error", {
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

test_that("calc_auc_from_sim: correct AUC calculated", {
  # parameters as list
  expect_equal(
    calc_auc_from_regimen(
      regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
      parameters = list(CL = 5, V = 50),
      model = mod_1cmt_iv,
      target_time = c(48, 72)
    ),
    19.984296
  )
  # parameters as data.frame
  expect_equal(
    calc_auc_from_regimen(
      regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
      parameters = data.frame(ID = 1, CL = 5, V = 50),
      model = mod_1cmt_iv, # defined in setup
      target_time = c(48, 72)
    ),
    19.984296
  )
  # parameters as named vector
  expect_equal(
    calc_auc_from_regimen(
      regimen = PKPDsim::new_regimen(interval = 24, type = "infusion"),
      parameters = c(CL = 5, V = 50),
      model = mod_1cmt_iv, # defined in setup
      target_time = c(48, 72)
    ),
    19.984296
  )
})
