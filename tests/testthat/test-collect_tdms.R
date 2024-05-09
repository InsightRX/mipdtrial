mod <- PKPDsim::new_ode_model("pk_2cmt_iv")
res_var <- generate_ruv(1, 1, 1:5, 0.2, 1, seed = 2)
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
  expect_true(all(c("t", "true_y", "y") %in% colnames(result)))
  expect_equal(nrow(result), length(t_obs))
  expect_equal(result$t, t_obs)
  expect_equal(
    (result$y - res_var$add)/res_var$prop, result$true_y
  )
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


