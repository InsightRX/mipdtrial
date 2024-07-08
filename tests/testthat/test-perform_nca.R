test_that("NCA based on correct dosing interval", {
  reg <- PKPDsim::new_regimen(
    amt = c(50, 100, 150),
    interval = 24,
    t_inf = 1,
    type = "infusion"
  )
  tdms1 <- data.frame(
    t = c(1, 5, 8),
    y = c(2000, 400, 40)
  )
  tdms2 <- tdms1
  tdms3 <- tdms1
  tdms2$t <- tdms2$t + 24
  tdms3$t <- tdms3$t + 48

  res1 <- perform_nca(tdms1, reg)
  res2 <- perform_nca(tdms2, reg)
  res3 <- perform_nca(tdms3, reg)

  # correct dose used for nca
  expect_equal(res1$settings$dose, 50)
  expect_equal(res2$settings$dose, 100)
  expect_equal(res3$settings$dose, 150)

  # expected fields present
  expect_true(all(c("pk", "descriptive") %in% names(res1)))
  expect_true(all(c("pk", "descriptive") %in% names(res2)))
  expect_true(all(c("pk", "descriptive") %in% names(res3)))

  # PK / exposure estimates make sense
  expect_equal(res1$descriptive$c_max_true, tdms1$y[1]) # t_inf = 1 hr
  expect_equal(res2$descriptive$c_max_true, tdms2$y[1])
  expect_equal(res3$descriptive$c_max_true, tdms3$y[1])

  expect_true(res1$pk$cl < res2$pk$cl) # same TDMs, but increasing doses
  expect_true(res1$pk$cl < res3$pk$cl)
  expect_true(res2$pk$cl < res3$pk$cl)
})

test_that("dose_from_auc errors on unsupported target types", {
  target <- create_target_design(targettype = "cmin", targetvalue = 100)
  regimen <- PKPDsim::new_regimen(interval = 24)
  expect_error(
    dose_from_auc(target, intv_auc = 5, regimen = regimen, dose_update = 2),
    "target type cmin not yet supported!"
  )
})

test_that("dose_from_auc computes dose for 'auc' and 'auc24' target types", {
  target1 <- create_target_design(targettype = "auc", targetvalue = 10)
  target2 <- create_target_design(targettype = "auc24", targetvalue = 10)
  regimen <- PKPDsim::new_regimen(
    interval = 12,
    n = 8,
    amt = c(rep(100, 4), rep(200, 4))
  )
  expect_equal(
    dose_from_auc(target1, intv_auc = 8, regimen = regimen, dose_update = 2),
    125 # 100 * 10/8
  )
  expect_equal(
    dose_from_auc(target1, intv_auc = 8, regimen = regimen, dose_update = 7),
    250 # 200 * 10/8
  )
  expect_equal(
    dose_from_auc(target2, intv_auc = 8, regimen = regimen, dose_update = 2),
    62.5 # 100 * 10/(8 * 24/12)
  )
  expect_equal(
    dose_from_auc(target2, intv_auc = 8, regimen = regimen, dose_update = 7),
    125 # 200 * 10/(8 * 24/12)
  )
})

test_that("dose_from_auc computes dose for 'cum_auc' target type", {
  regimen <- PKPDsim::new_regimen(
    interval = 12,
    n = 8,
    amt = c(rep(100, 4), rep(200, 4))
  )
  target <- create_target_design(targettype = "cum_auc", targetvalue = 100)
  # have given 40 AUC, need to give 60 AUC over 6 doses, and 100 mg -> 20 AUC
  # so need to give 10 AUC per dose and 50 mg -> 10 AUC
  overdosed1 <- dose_from_auc(
    target = target,
    intv_auc = 20,
    regimen = regimen,
    dose_update = 3,
    cum_auc = 40
  )
  expect_equal(overdosed1, 50)

  # cum auc > target, new doses are 0
  overdosed2 <- dose_from_auc(
    target = target,
    intv_auc = 20,
    regimen = regimen,
    dose_update = 7,
    cum_auc = 110
  )
  expect_equal(overdosed2, 0)

  # have to give 40 AUC over remaining 2 doses, 200 mg -> 10 AUC
  underdosed1 <- dose_from_auc(
    target = target,
    intv_auc = 10,
    regimen = regimen,
    dose_update = 7,
    cum_auc = 60
  )
  expect_equal(underdosed1, 400)
})

test_that("Tau from inter-dose intervals", {
  reg <- PKPDsim::new_regimen(times = c(0, 6, 24, 36))
  expect_equal(tau_from_regimen(reg, 1), 6)
  expect_equal(tau_from_regimen(reg, 2), 18)
  expect_equal(tau_from_regimen(reg, 3), 12)
  expect_equal(tau_from_regimen(reg, 4), 12)
})
