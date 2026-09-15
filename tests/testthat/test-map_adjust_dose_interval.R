mod <- mod_1cmt_iv # defined in setup
par <- list(CL = 1, V = 10)
omega <- c(0.1, 0.05, 0.1)

regimen <- PKPDsim::new_regimen(
  amt = 2000,
  n = 5,
  interval = 24
)

test_that("returns valid structure", {
  out <- sample_and_adjust_by_dose(
    regimen_update_design = create_regimen_update_design(
      at = c(2, 4),
      anchor = "dose",
      dose_optimization_method = map_adjust_dose_interval,
      interval_grid = c(12, 24),
      preferred_interval = 24
    ),
    sampling_design = create_sampling_design(
      offset = c(20, 12),
      when = c("dose", "dose"),
      at = c(1, 3),
      anchor = "dose"
    ),
    regimen = regimen,
    pars_true_i = generate_iiv(mod, omega, par, seed = 1),
    sim_model = mod,
    sim_ruv = list(prop = 0.1, add = 1),
    est_model = mod,
    parameters = par,
    omega = omega,
    ruv = list(prop = 0.1, add = 1),
    target = create_target_design(
      targettype = "conc",
      targetvalue = 15,
      at = 5,
      anchor = "dose"
    )
  )

  expect_true(inherits(out, "list"))
  expect_true(
    all(c("final_regimen", "tdms", "additional_info", "gof") %in% names(out))
  )
  # regimen should be updated
  expect_false(is.null(out$final_regimen))
  expect_true(length(out$final_regimen$dose_amts) > 0)
})

test_that("tie-breaking selects preferred interval", {
  # Mock dose_grid_search to return same dose regardless of interval,
  # and mock simulate_dose_interval to return target value (perfect attainment)
  # regardless of interval. Then preferred_interval should win.
  local_mocked_bindings(
    simulate_fit = function(...) {
      list(
        parameters = par,
        pred = 1, ipred = 1, dv = 1, weights = 1
      )
    },
    dose_grid_search = function(...) 500,
    simulate_dose_interval = function(...) 15  # exact target
  )

  out <- map_adjust_dose_interval(
    tdms = data.frame(t = 20, obs_type = 1, y = 10),
    est_model = mod,
    parameters = par,
    omega = omega,
    ruv = list(prop = 0.1, add = 1),
    regimen = regimen,
    target_design = create_target_design(
      targettype = "conc",
      targetvalue = 15,
      at = 5,
      anchor = "dose"
    ),
    dose_update = 2,
    grid = seq(100, 1000, length.out = 10),
    interval_grid = c(8, 12, 24),
    preferred_interval = 12
  )

  expect_equal(out$new_interval, 12)
  expect_equal(out$new_dose, 500)
})

test_that("best attainment wins over preferred interval", {
  # Mock: interval=8 achieves target exactly, interval=12 and 24 are far off.
  # preferred_interval=24 should NOT win because 24 is outside tolerance band.
  local_mocked_bindings(
    simulate_fit = function(...) {
      list(
        parameters = par,
        pred = 1, ipred = 1, dv = 1, weights = 1
      )
    },
    dose_grid_search = function(...) 500,
    simulate_dose_interval = function(value, grid_type, dose_update, regimen, ...) {
      # Use the interval from the regimen to determine achieved value
      intv <- regimen$interval
      if (intv == 8) return(15)     # perfect
      if (intv == 12) return(10)    # far off
      if (intv == 24) return(5)     # very far off
      return(15)
    }
  )

  out <- map_adjust_dose_interval(
    tdms = data.frame(t = 20, obs_type = 1, y = 10),
    est_model = mod,
    parameters = par,
    omega = omega,
    ruv = list(prop = 0.1, add = 1),
    regimen = regimen,
    target_design = create_target_design(
      targettype = "conc",
      targetvalue = 15,
      at = 5,
      anchor = "dose"
    ),
    dose_update = 2,
    grid = seq(100, 1000, length.out = 10),
    interval_grid = c(8, 12, 24),
    preferred_interval = 24,
    attainment_tolerance = 0.05  # band = 0.75 mg/L
  )

  # interval=8 achieves exactly 15; others are 5 and 10 away, way outside band

  expect_equal(out$new_interval, 8)
})

test_that("dose constraints are respected via settings", {
  local_mocked_bindings(
    simulate_fit = function(...) {
      list(
        parameters = par,
        pred = 1, ipred = 1, dv = 1, weights = 1
      )
    },
    # dose_grid_search receives min_dose/max_dose and returns capped value
    dose_grid_search = function(..., min_dose = NULL, max_dose = NULL) {
      dose <- 1500
      if (!is.null(max_dose) && dose > max_dose) dose <- max_dose
      if (!is.null(min_dose) && dose < min_dose) dose <- min_dose
      dose
    },
    simulate_dose_interval = function(...) 15
  )

  out <- map_adjust_dose_interval(
    tdms = data.frame(t = 20, obs_type = 1, y = 10),
    est_model = mod,
    parameters = par,
    omega = omega,
    ruv = list(prop = 0.1, add = 1),
    regimen = regimen,
    target_design = create_target_design(
      targettype = "conc",
      targetvalue = 15,
      at = 5,
      anchor = "dose"
    ),
    dose_update = 2,
    grid = seq(100, 2000, length.out = 10),
    interval_grid = c(24),
    settings = list(max_dose = 1000, min_dose = 100)
  )

  expect_true(out$new_dose <= 1000)
})

test_that("single interval degenerates to dose-only optimization", {
  local_mocked_bindings(
    simulate_fit = function(...) {
      list(
        parameters = par,
        pred = 1, ipred = 1, dv = 1, weights = 1
      )
    },
    dose_grid_search = function(...) 750,
    simulate_dose_interval = function(...) 15
  )

  out <- map_adjust_dose_interval(
    tdms = data.frame(t = 20, obs_type = 1, y = 10),
    est_model = mod,
    parameters = par,
    omega = omega,
    ruv = list(prop = 0.1, add = 1),
    regimen = regimen,
    target_design = create_target_design(
      targettype = "conc",
      targetvalue = 15,
      at = 5,
      anchor = "dose"
    ),
    dose_update = 2,
    grid = seq(100, 1000, length.out = 10),
    interval_grid = c(24)
  )

  expect_equal(out$new_interval, 24)
  expect_equal(out$new_dose, 750)
})

test_that("input validation errors on bad arguments", {
  tdms <- data.frame(t = 20, obs_type = 1, y = 10)
  td <- create_target_design(
    targettype = "conc", targetvalue = 15, at = 5, anchor = "dose"
  )
  base_args <- list(
    tdms = tdms,
    est_model = mod,
    parameters = par,
    omega = omega,
    ruv = list(prop = 0.1, add = 1),
    regimen = regimen,
    target_design = td,
    dose_update = 2,
    grid = seq(100, 1000, length.out = 10)
  )

  # NULL interval_grid
  expect_error(
    do.call(map_adjust_dose_interval, c(base_args, list(interval_grid = NULL))),
    "interval_grid"
  )

  # Non-numeric interval_grid
  expect_error(
    do.call(map_adjust_dose_interval, c(base_args, list(interval_grid = "bad"))),
    "interval_grid"
  )

  # Bad preferred_interval (vector)
  expect_error(
    do.call(map_adjust_dose_interval, c(base_args, list(
      interval_grid = c(12, 24),
      preferred_interval = c(12, 24)
    ))),
    "preferred_interval"
  )

  # Bad attainment_tolerance (negative)
  expect_error(
    do.call(map_adjust_dose_interval, c(base_args, list(
      interval_grid = c(12, 24),
      attainment_tolerance = -0.1
    ))),
    "attainment_tolerance"
  )
})

test_that("integration: works end-to-end via sample_and_adjust_by_dose", {
  out <- sample_and_adjust_by_dose(
    regimen_update_design = create_regimen_update_design(
      at = c(2, 4),
      anchor = "dose",
      dose_optimization_method = map_adjust_dose_interval,
      interval_grid = c(12, 24),
      preferred_interval = 24
    ),
    sampling_design = create_sampling_design(
      offset = c(20, 12),
      when = c("dose", "dose"),
      at = c(1, 3),
      anchor = "dose"
    ),
    regimen = regimen,
    pars_true_i = generate_iiv(mod, omega, par, seed = 1),
    sim_model = mod,
    sim_ruv = list(prop = 0.1, add = 1),
    est_model = mod,
    parameters = par,
    omega = omega,
    ruv = list(prop = 0.1, add = 1),
    target = create_target_design(
      targettype = "conc",
      targetvalue = 15,
      at = 5,
      anchor = "dose"
    )
  )

  expect_true(inherits(out, "list"))
  expect_true(
    all(c("final_regimen", "tdms", "additional_info", "gof") %in% names(out))
  )

  # doses should have been adjusted
  expect_false(all(out$final_regimen$dose_amts == regimen$dose_amts[1]))

  # interval should be one of the candidate intervals
  expect_true(out$final_regimen$interval %in% c(12, 24))
})
