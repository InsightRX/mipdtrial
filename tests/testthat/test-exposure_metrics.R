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
