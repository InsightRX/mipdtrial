# set up model info
model <- pkbusulfanmccune::model()
parameters <- pkbusulfanmccune::parameters()
omega <- pkbusulfanmccune::omega_matrix()
ruv <- pkbusulfanmccune::ruv()

# set up patient info
covs <- list(
  AGE = PKPDsim::new_covariate(12),
  WT = PKPDsim::new_covariate(40),
  HT = PKPDsim::new_covariate(120),
  SEX = PKPDsim::new_covariate(1),
  T_CL_EFF = PKPDsim::new_covariate(0)
)
regimen <- PKPDsim::new_regimen(
  amt = 120,
  interval = 24,
  n = 4,
  t_inf = 3,
  type = "infusion"
)

tdms <- data.frame(
  t = c(3.5, 6, 10, 27.5, 30, 34), # two days of sampling
  y = c(3000, 500, 100, 3000, 700, 200) # slower clearance day 2
)


test_that("MAP fit works, with and without iov", {
  # with iov
  res_iov <- simulate_fit(model, parameters, omega, ruv, tdms, covs, regimen)
  fit_par_iov <- res_iov[["parameters"]]

  # convert to non-IOV model
  add_fixed <- names(parameters)[grepl("kappa", names(parameters))]
  attr(model, "iov") <- list(n_bins = 1)
  attr(model, "fixed") <- c(attr(model, "fixed"), add_fixed)
  res_no_iov <- suppressWarnings( # TODO: fix iov bin warning in PKPDmap
    simulate_fit(model, parameters, omega, ruv, tdms, covs, regimen)
  )
  fit_par_no_iov <- res_no_iov[["parameters"]]

  # correct structure
  expect_cols <- c("pred", "ipred", "dv", "weights")
  expect_equal(sort(names(fit_par_iov)), sort(names(parameters)))
  expect_equal(sort(names(fit_par_no_iov)), sort(names(parameters)))
  expect_true(all(expect_cols %in% names(res_iov)))
  expect_true(all(expect_cols %in% names(res_no_iov)))

  # not the same as population parameters
  expect_false(
    any(
      unlist(res_iov[c("CL", "V", "Q", "V2")]) ==
        unlist(parameters[c("CL", "V", "Q", "V2")]))
  )
  expect_false(
    any(
      unlist(res_no_iov[c("CL", "V", "Q", "V2")]) ==
        unlist(parameters[c("CL", "V", "Q", "V2")]))
  )
  # not the same as each other
  expect_false(
    any(
      unlist(res_iov[c("CL", "V", "Q", "V2")]) ==
        unlist(res_no_iov[c("CL", "V", "Q", "V2")]))
  )

  # only when iov are kappa parameters non-zero
  kappa_days1_2 <- c("kappa_CL_1", "kappa_CL_2", "kappa_V_1", "kappa_V_2")
  expect_true(all(unlist(res_iov[kappa_days1_2]) != 0))
  expect_true(all(unlist(res_no_iov[kappa_days1_2]) == 0))
  # no TDMs on days 3+, so IOV estimates should be zero for both cases
  other_kappa <- setdiff(add_fixed, kappa_days1_2)
  expect_true(all(unlist(res_iov[other_kappa]) == 0))
  expect_true(all(unlist(res_no_iov[other_kappa]) == 0))
})

test_that("iov specifications for model with IOV are correct", {
  with_iov <- get_iov_specification(model, parameters, omega)
  expect_names <- c(
    "parameters", "kappa", "omega", "fixed", "bins", "omega_type"
  )
  expect_true(all(expect_names %in% names(with_iov)))
  expect_equal(with_iov$fixed, attr(model, "fixed"))
  expect_equal(with_iov$bins, attr(model, "iov")$bins)
  expect_equal(sort(names(with_iov$parameters)), sort(names(parameters)))

  omega_type_counts <- table(with_iov$omega_type)
  expect_equal(
    omega_type_counts[["exponential"]],
    length(parameters) - length(with_iov$kappa) - length(with_iov$fixed)
  )
  expect_equal(
    omega_type_counts[["normal"]],
    length(with_iov$kappa)
  )
})

test_that("iov specifications for model without IOV are correct", {

  # convert to non-IOV model
  add_fixed <- names(parameters)[grepl("kappa", names(parameters))]
  attr(model, "iov") <- list(n_bins = 1)
  attr(model, "fixed") <- c(attr(model, "fixed"), add_fixed)

  no_iov <- get_iov_specification(model, parameters, omega)
  expect_names <- c(
    "parameters", "kappa", "omega", "fixed", "bins", "omega_type"
  )
  expect_true(all(expect_names %in% names(no_iov)))
  expect_equal(no_iov$fixed, attr(model, "fixed"))
  expect_equal(no_iov$bins, c(0, 99999))
  expect_equal(sort(names(no_iov$parameters)), sort(names(parameters)))

  omega_type_counts <- table(no_iov$omega_type)
  expect_equal(
    omega_type_counts[["exponential"]],
    length(parameters) - length(no_iov$kappa) - length(no_iov$fixed)
  )
  expect_false("normal" %in% names(omega_type_counts))
})
