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
