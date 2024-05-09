suppressMessages( ## avoid message "the following objects are masked from ..."
  require("pk1cmtivauc", character.only = TRUE)
)
mod <- get("model", asNamespace("pk1cmtivauc"))()
par <- list(CL = 1, V = 10)
omega <- c(0.1, 0.05, 0.1)

regimen <- PKPDsim::new_regimen(
  amt = 2000,
  n = 4,
  interval = 24
)

test_that("trial by dose change works", {

  out <- sample_and_adjust_by_dose(
    adjust_at_dose = c(2, 4), # adjust 2nd and 4th (last) dose
    tdm_times = c(20, 64),
    regimen = regimen,
    pars_true_i = generate_iiv(mod, omega, par, seed = 1),
    sim_model = mod,
    sim_ruv = list(prop = 0.1, add = 1),
    est_model = mod,
    est_parameters = par,
    est_omega = omega,
    est_ruv = list(prop = 0.1, add = 1),
    target_time = 4 * 24,
    target = list(type = "conc", value = 15)
  )

  # expected structure
  expect_true(inherits(out, "list"))
  expect_true(
    all(c("final_regimen", "final_estimates", "tdms") %in% names(out))
  )

  # expected doses are changed
  final_doses <- out$final_regimen
  # first is unchanged
  expect_equal(final_doses$dose_amts[1], regimen$dose_amts[1])
  # 2nd and 3rd are the same
  expect_equal(final_doses$dose_amts[2], final_doses$dose_amts[3])
  # 4th is different from others
  expect_true(final_doses$dose_amts[3] != final_doses$dose_amts[4])

  # expected tdm sampling structure
  tdms <- out$tdms
  expect_equal(nrow(tdms), 2) # tdm_times is c(12, 64) -> length == 2
  expect_false(any(tdms$true_y == tdms$y))

  # expected estimate structure
  ests <- out$final_estimates
  expect_equal(sort(names(ests)), sort(names(par)))

  # reasonable output
  tmp <- PKPDsim::sim(
    mod,
    parameters = ests, # best guess
    regimen = final_doses, # final regimen
    t_obs = 4 * 24,
    only_obs = TRUE
  )
  # allow up to 5% error from goal of 15 mg/L
  expect_true(abs((tmp$y - 15)/15) < 0.05)
})

test_that("Supplying true pars as list also works", {

  out <- sample_and_adjust_by_dose(
    adjust_at_dose = c(2, 4), # adjust 2nd and 4th (last) dose
    tdm_times = c(12, 64),
    regimen = regimen,
    pars_true_i = list(CL = 1.5, V = 15),
    sim_model = mod,
    sim_ruv = list(prop = 0.1, add = 1),
    est_model = mod,
    est_parameters = par,
    est_omega = omega,
    est_ruv = list(prop = 0.1, add = 1),
    target_time = 4 * 24,
    target = list(type = "conc", value = 10)
  )

  # expected structure
  expect_true(inherits(out, "list"))
  expect_true(
    all(c("final_regimen", "final_estimates", "tdms") %in% names(out))
  )

  # expected estimate structure
  ests <- out$final_estimates
  expect_equal(sort(names(ests)), sort(names(par)))

  # reasonable output
  tmp <- PKPDsim::sim(
    mod,
    parameters = ests, # best guess
    regimen = out$final_regimen, # final regimen
    t_obs = 4 * 24,
    only_obs = TRUE
  )
  # allow up to 5% error from goal of 10 mg/L
  expect_true(abs((tmp$y - 10)/10) < 0.05)
})

test_that("errors if dose update includes dose 1", {
  expect_error(
    sample_and_adjust_by_dose(
      adjust_at_dose = c(1, 2, 4),
      tdm_times = c(12, 64),
      regimen = regimen,
      pars_true_i = generate_iiv(mod, omega, par, seed = 1),
      sim_model = mod,
      sim_ruv = list(prop = 0.1, add = 1),
      est_model = mod,
      est_parameters = par,
      est_omega = omega,
      est_ruv = list(prop = 0.1, add = 1),
      target_time = 4 * 24
    ),
    "TDM collection before the first dose is not yet supported"
  )
})

test_that("errors if dose update before first TDM", {
  expect_error(
    sample_and_adjust_by_dose(
      adjust_at_dose = c(2, 4),
      tdm_times = c(50, 64), # dose 2 is before 50 hours
      regimen = regimen,
      pars_true_i = generate_iiv(mod, omega, par, seed = 1),
      sim_model = mod,
      sim_ruv = list(prop = 0.1, add = 1),
      est_model = mod,
      est_parameters = par,
      est_omega = omega,
      est_ruv = list(prop = 0.1, add = 1),
      target_time = 4 * 24
    ),
    "At least one TDM must be collected before dose adjustment"
  )
})

test_that("errors if update doses are longer than supplied regimen", {
  expect_error(
    sample_and_adjust_by_dose(
      adjust_at_dose = c(2, 4, 6), # only 4 doses
      tdm_times = c(50, 64),
      regimen = regimen,
      pars_true_i = generate_iiv(mod, omega, par, seed = 1),
      sim_model = mod,
      sim_ruv = list(prop = 0.1, add = 1),
      est_model = mod,
      est_parameters = par,
      est_omega = omega,
      est_ruv = list(prop = 0.1, add = 1),
      target_time = 4 * 24
    ),
    "Insufficient doses in `regimen` for all dose adjustments specified."
  )
})
