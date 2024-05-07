suppressMessages( ## avoid message "the following objects are masked from ..."
  require("pk1cmtivauc", character.only = TRUE)
)
mod <- get("model", asNamespace("pk1cmtivauc"))()
par <- list(CL = 1, V = 10)
omega <- c(0.1, 0.05, 0.1)
intv <- 12
t_inf <- 2
n <- 5
reg <- PKPDsim::new_regimen(
  amt = 100,
  n = n + 1,
  interval = intv,
  type="infusion",
  t_inf = t_inf,
  checks = FALSE
)
dose_grid <- seq(from = 50, to = 600, by = 10)

test_that("trough concentration search works", {
  dose_ctr <- dose_grid_search(
    est_model = mod,
    dose_grid = dose_grid,
    parameters = par,
    regimen = reg,
    refine = FALSE,
    t_obs = intv * n,
    return_obj = FALSE,
    target = list(value = 5, type = "conc", method = "nearest_value")
  )
  #setting a probability of 50% should be the same thing
  dose_ctr_prob1 <- dose_grid_search(
    est_model = mod,
    dose_grid = dose_grid,
    parameters = par,
    regimen = reg,
    omega = omega, # needs omega now!
    pta = list(prob = .5, type="gt"),
    t_obs = intv * n,
    return_obj = FALSE,
    target = list(value = 5, type = "conc", method = "nearest_value")
  )
  #setting a probability of 90% should require a higher dose
  dose_ctr_prob2 <- dose_grid_search(
    est_model = mod,
    dose_grid = dose_grid,
    parameters = par,
    regimen = reg,
    omega = omega, # needs omega now!
    pta = list(prob = .9, type="gt"),
    t_obs = intv * n,
    return_obj = FALSE,
    target = list(value = 5, type = "conc", method = "nearest_value")
  )
  #setting a probability of 90% + ruv should require an even higher dose
  dose_ctr_prob3 <- dose_grid_search(
    est_model = mod,
    dose_grid = dose_grid,
    parameters = par,
    regimen = reg,
    omega = omega, # needs omega now!
    pta = list(prob = .9, type="gt"),
    ruv = list(prop = .1, add = .5),
    t_obs = intv * n,
    return_obj = FALSE,
    target = list(value = 5, type = "conc", method = "nearest_value")
  )

  expect_lt(abs(dose_ctr - 105)/105, 0.01)
  expect_equal(dose_ctr_prob1, dose_ctr)
  expect_lt(dose_ctr_prob1, dose_ctr_prob2)
  expect_lt(dose_ctr_prob2, dose_ctr_prob3)
  expect_lt(abs(dose_ctr_prob2 - 243)/243, 0.01)
  expect_lt(abs(dose_ctr_prob3 - 252)/252, 0.01)
})


test_that("peak concentration search works", {
  dose_cpeak <- dose_grid_search(
    est_model = mod,
    dose_grid = seq(from = 50, to = 300, by = (300 - 50) / 10 ),
    parameters = par,
    regimen = reg,
    refine = FALSE,
    t_obs = intv * (n-1) + t_inf,
    return_obj = FALSE,
    target = list(value = 10, type = "conc", method = "nearest_value")
  )
  expect_lt(abs(dose_cpeak - 77)/77, 0.01)
})

test_that("AUC search works", {
  dose_cum_auc <- dose_grid_search(
    est_model = mod,
    dose_grid = dose_grid,
    parameters = par,
    regimen = reg,
    obs_comp = 2,
    refine = FALSE,
    t_obs = 5*intv,
    target = list(
      value = 1500,
      type = "cum_auc",
      method = "nearest_value"
    )
  )
  # probability of AUC>target (at 50% prob it should be same as before)
  dose_auc_prob1 <- dose_grid_search(
    est_model = mod,
    dose_grid = dose_grid,
    parameters = par,
    regimen = reg,
    pta = list(prob = .5, type="gt"),
    obs_comp = 2,  # take AUC, not conc!
    omega = omega,
    ruv = list(prop = .1, add = .5),
    t_obs = intv * n,
    target = list(
      value = 1500,
      type = "cum_auc",
      method = "nearest_value"
    )
  )
  # probability of AUC>target (at 90% prob it should be higher)
  dose_auc_prob2 <- dose_grid_search(
    est_model = mod,
    dose_grid = dose_grid,
    parameters = par,
    regimen = reg,
    pta = list(prob = .9, type="gt"),
    obs_comp = 2,  # take AUC, not conc!
    omega = omega,
    t_obs = intv * n,
    target = list(
      value = 1500,
      type = "cum_auc",
      method = "nearest_value"
    )
  )

  expect_lt(abs(dose_cum_auc - 332)/332, 0.01)
  expect_equal(dose_cum_auc, dose_auc_prob1)
  expect_lt(dose_auc_prob1, dose_auc_prob2)
  expect_lt(abs(dose_auc_prob2 - 531)/531, 0.01)
})

test_that("Probability: less than target", {
  dose_auc_prob3 <- dose_grid_search(
    est_model = mod,
    dose_grid = seq(from = 50, to = 600, by = (500 - 50) / 10 ),
    parameters = par,
    regimen = reg,
    pta = list(prob = .9,
               type="lt"), ## less than!
    obs_comp = 2,  # take AUC, not conc!
    omega = omega,
    dose_resolution = 5,
    t_obs = intv * n,
    return_obj = FALSE,
    target = list(
      value = 1500,
      type = "cum_auc"
    )
  )
  expect_lt(abs(dose_auc_prob3 - 240)/240, 0.01)
})



test_that('nonlinear models have refining activated rather than defaulting to linear interpolation', {
  suppressMessages( ## avoid message "the following objects are masked from ..."
    require("pkvoriconazolefriberg", character.only = TRUE)
  )
  model <- get("model", asNamespace("pkvoriconazolefriberg"))()

  regimen_new <- PKPDsim::new_regimen(
    n = 16,
    amt = 1000,
    interval = 12,
    t_inf = 1,
    type = "infusion"
  )

  ind_est <- list(
    KA = 1.19,
    VMAX1 = 114L,
    KM = 400,
    CL = 0.01,
    Q = 15.5,
    V = 79L,
    V2 = 103L,
    F1 = 0.585,
    T50 = 2.41,
    TLAG = 0.949,
    BCF = 0.367
  )

  target_def <- list(
    value = 225,
    range = c(200, 250),
    type = "conc"
  )

  covs <- list(
    WT = PKPDsim::new_covariate(value = 10),
    AGE = PKPDsim::new_covariate(value = 4.5),
    CYP2C19unknown = PKPDsim::new_covariate(value = 0),
    CYP2C19a1a2 = PKPDsim::new_covariate(value = 0),
    CYP2C19a2a2 = PKPDsim::new_covariate(value = 0),
    CYP2C19a2a3 = PKPDsim::new_covariate(value = 0),
    CYP2C19a1a3 = PKPDsim::new_covariate(value = 0),
    CYP2C19a3a3 = PKPDsim::new_covariate(value = 1)
  )

  no_refine <- dose_grid_search(
    est_model = model,
    dose_grid = seq(from = 0.5, to = 10000, by = (1000 - 0.5) / 10 ),
    check_boundaries = TRUE,
    refine = FALSE,
    refine_range = c(0.85, 1.15),
    regimen = regimen_new,
    interval = 12,
    dose_update = 1,
    parameters = ind_est,
    obs_comp = 2,
    target = target_def,
    t_obs = 24,
    iov_bins = NULL,
    dose_resolution = 1,
    max_dose = 10000,
    min_dose = 0.5,
    covariates = covs
  )
  refine <- dose_grid_search(
    est_model = model,
    dose_grid = seq(from = 0.5, to = 10000, by = (1000 - 0.5) / 10 ),
    check_boundaries = TRUE,
    refine_range = c(0.85, 1.15),
    regimen = regimen_new,
    interval = 12,
    dose_update = 1,
    parameters = ind_est,
    obs_comp = 2,
    target = target_def,
    t_obs = 24,
    iov_bins = NULL,
    dose_resolution = 1,
    max_dose = 10000,
    min_dose = 0.5,
    covariates = covs
  )
  expect_equal(no_refine, 54)
  expect_equal(refine, 56)
})
