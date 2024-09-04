mod <- mod_1cmt_iv # defined in setup
par <- list(CL = 1, V = 10)
omega <- c(0.1, 0.05, 0.1)

pct_err <- function(test, true) (test-true)/true

test_that("Generates IIV error for multiple individuals", {
  out <- generate_iiv(
    mod,
    omega,
    par,
    ids = letters[1:10],
    n_iter = 50,
    seed = 1
  )
  expect_true(inherits(out, "data.frame"))
  expect_equal(nrow(out), 10 * 50)
  expect_true(all(names(par) %in% colnames(out)))
  # test values are within 5% or so
  expect_true(abs(pct_err(mean(out$CL), par$CL)) < 0.05)
  expect_true(abs(pct_err(mean(out$V), par$V)) < 0.05)
})

test_that("Generates IIV and IOV error", {
  mod1 <- mod
  attr(mod1, "iov") <- list(
    n_bins = 2,
    bins = c(0, 48, 9999),
    cv = list(CL = 0.2)
  )
  new_par <- c(par, list(kappa_CL_1 = 0, kappa_CL_2 = 0))
  attr(mod1, "parameters") <- names(new_par)
  out <- generate_iiv(
    mod1,
    omega,
    new_par,
    ids = letters[1:10],
    n_iter = 50,
    seed = 1
  )
  expect_true(inherits(out, "data.frame"))
  expect_equal(nrow(out), 10 * 50)
  expect_true(all(names(new_par) %in% colnames(out)))
  # IOV terms are non-zero
  expect_true(all(out$kappa_CL_1 != 0))
  expect_true(all(out$kappa_CL_2 != 0))
})

test_that("Generates RUV error for multiple individuals", {
  out <- generate_ruv(
    ids = 1:30,
    n_iter = 40,
    tdm_sample_time = seq(2, 100, 2),
    prop = 0.2,
    add = 2.5,
    seed = 2
  )
  expect_true(inherits(out, "data.frame"))
  expect_equal(nrow(out), 30 * 40 * 50)
  # test values are within 1% or so (large data set, law of large numbers)
  expect_true(abs(pct_err(mean(out$prop), 1)) < 0.01)
  expect_true(abs(mean(out$add)) < 0.01)
})

test_that("Adds RUV to both ltbs models correctly", {
  out <- generate_ruv(
    ids = 1:30,
    n_iter = 40,
    tdm_sample_time = seq(2, 100, 2),
    prop = 0.000,
    add = 0.25,
    ltbs = TRUE,
    seed = 2
  )
  expect_true(inherits(out, "data.frame"))
  expect_equal(nrow(out), 30 * 40 * 50)
  # test values are within 1% or so (large data set, law of large numbers)
  # add error transformed to prop error in ltbs transformation
  expect_true(abs(pct_err(mean(out$prop), 1)) < 0.01)
  expect_true(abs(mean(out$add)) == 0.0)
})

test_that("Reproducible randomness: IIV", {
  out1 <- generate_iiv(mod, omega, par, ids = 1, n_iter = 1, seed = 1)
  out2 <- generate_iiv(mod, omega, par, ids = 1, n_iter = 1, seed = 2)
  out3 <- generate_iiv(mod, omega, par, ids = 1, n_iter = 1, seed = 2)
  expect_true(inherits(out1, "data.frame"))
  expect_equal(nrow(out1), 1)
  expect_false(out1$CL == out2$CL)
  expect_false(out1$V == out2$V)
  expect_identical(out2, out3)
})

test_that("No seed by default: IIV", {
  out1 <- generate_iiv(mod, omega, par, ids = 1, n_iter = 1)
  out2 <- generate_iiv(mod, omega, par, ids = 1, n_iter = 1)
  expect_false(out1$CL == out2$CL)
  expect_false(out1$V == out2$V)
})

test_that("Reproducible randomness: RUV", {
  out1 <- generate_ruv(
    ids = 1,
    n_iter = 1,
    tdm_sample_time = 1:4,
    seed = 1,
    prop = 0.2,
    add = 2.5
  )
  out2 <- generate_ruv(
    ids = 1,
    n_iter = 1,
    tdm_sample_time = 1:4,
    seed = 2,
    prop = 0.2,
    add = 2.5
  )
  out3 <- generate_ruv(
    ids = 1,
    n_iter = 1,
    tdm_sample_time = 1:4,
    seed = 2,
    prop = 0.2,
    add = 2.5
  )
  expect_true(inherits(out1, "data.frame"))
  expect_equal(nrow(out1), 1 * 1 * 4)
  expect_true(all(out1$prop != out2$prop))
  expect_true(all(out1$add != out2$add))
  expect_identical(out2, out3)
})

test_that("No seed by default: RUV", {
  out1 <- generate_ruv(
    ids = 1,
    n_iter = 1,
    tdm_sample_time = 1:4,
    prop = 0.2,
    add = 2.5
  )
  out2 <- generate_ruv(
    ids = 1,
    n_iter = 1,
    tdm_sample_time = 1:4,
    prop = 0.2,
    add = 2.5
  )
  expect_true(all(out1$prop != out2$prop))
  expect_true(all(out1$add != out2$add))
})

test_that("Reasonable dummy regimen option selected", {
  attr(mod, "cmt_mapping") <- list(drug1 = 1, drug2 = 1)
  out <- generate_iiv(
    mod,
    omega,
    par,
    ids = "a",
    n_iter = 1,
    seed = 1
  )
  expect_true(inherits(out, "data.frame"))
  expect_equal(nrow(out), 1)
  expect_true(all(names(par) %in% colnames(out)))
})

