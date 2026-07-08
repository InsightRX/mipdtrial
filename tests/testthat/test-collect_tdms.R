mod <- PKPDsim::new_ode_model("pk_2cmt_iv")
res_var <- generate_ruv(1:5, 0.2, 1, 1, 1, seed = 2)
t_obs <- res_var$tdm_number
pars_i <- list(CL = 5, Q = 1, V = 25, V2 = 20)
reg <- PKPDsim::new_regimen(amt = 1000, interval = 12, n = 3)

test_that("errors on mismatch of t_obs and res_var", {
  expect_error(
    collect_tdms(mod, 1:5, res_var[1:4,], pars_i),
    "mismatch in # observations & residual error provided"
  )
})

test_that("errors on missing columns in res_var", {
  res_var1 <- data.frame(prop = c(0.1, 0.1))  # missing 'add'
  expect_error(
    collect_tdms(mod, 1:2, res_var1, pars_i),
    "residual variability must include proportional & additive error"
  )
})

test_that("output structure and content are correct", {
  result <- collect_tdms(
    mod,
    t_obs,
    res_var,
    pars_i,
    regimen = reg
  )
  expect_true(inherits(result, "data.frame"))
  expect_true(all(c("t", "true_y", "y", "predictive_ipred") %in% colnames(result)))
  expect_equal(nrow(result), length(t_obs))
  expect_equal(result$t, t_obs)
  expect_equal(
    (result$y - res_var$add)/res_var$prop, result$true_y
  )
})

test_that("predictive_ipred is NA when est_model not supplied", {
  result <- collect_tdms(mod, t_obs, res_var, pars_i, regimen = reg)
  expect_true("predictive_ipred" %in% colnames(result))
  expect_true(all(is.na(result$predictive_ipred)))
})

test_that("predictive_ipred is populated when est_model and est_pars_i are supplied", {
  result <- collect_tdms(
    mod,
    t_obs,
    res_var,
    pars_i,
    est_model = mod,
    est_pars_i = pars_i,
    regimen = reg
  )
  expect_true("predictive_ipred" %in% colnames(result))
  expect_true(all(!is.na(result$predictive_ipred)))
  expect_true(is.numeric(result$predictive_ipred))
  # predictive_ipred uses est_pars_i (== pars_i here), so it should
  # equal true_y (no residual error applied)
  expect_equal(result$predictive_ipred, result$true_y)
})

test_that("handles LLOQ correctly", {
  result <- collect_tdms(mod, t_obs, res_var, pars_i, lloq = 16, regimen = reg)
  expect_equal(
    round(result$y,1),
    c(26, 26.5, 25.7, 8, 8)  # last 2 below lloq
  )
  expect_equal(
    round(result$true_y,1),
    c(31.5, 24.9, 19.7, 15.6, 12.5)  # no loq handling
  )
})

test_that("collect_tdms uses different iov bins for sim vs est model", {
  # Here we will mock the models and `PKPDsim::sim` since all we care about
  # are the IOV (mis)specification. `sim_model` has 2 IOV bins and `est_model`
  # does not have IOV bins.
  sim_model <- structure(list(), class = "PKPDsim")
  attr(sim_model, "iov") <- list(
    cv = list(CL = 0.1),
    n_bins = 2,
    bins = c(0, 24, 9999)
  )
  est_model <- structure(list(), class = "PKPDsim")
  attr(est_model, "iov") <- list(n_bins = 1) # no IOV -- bins must stay NULL

  # `sim()` is called once per model. Each call must receive that model's
  # *own* IOV bins (via `PKPDsim::get_model_iov()`), not the other model's.
  testthat::local_mocked_bindings(
    sim = function(ode, t_obs, iov_bins = NULL, ...) {
      expected_bins <- attr(ode, "iov")$bins
      # this is the check we use in PKPDsim for mismatch:
      if (!identical(iov_bins, expected_bins)) {
        stop("iov_bins passed to sim() do not match the model's own IOV spec")
      }
      # return dummy data:
      data.frame(t = t_obs, obs_type = 1, y = seq_along(t_obs))
    },
    .package = "PKPDsim"
  )

  expect_no_error(
    result <- collect_tdms(
      sim_model = sim_model,
      t_obs = c(1, 2),
      res_var = data.frame(prop = c(0.1, 0.1), add = c(1, 1)),
      pars_i = list(CL = 1),
      est_model = est_model,
      est_pars_i = list(CL = 1)
    )
  )
  expect_true(all(!is.na(result$predictive_ipred)))
})


