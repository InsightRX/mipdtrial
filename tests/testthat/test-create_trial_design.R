test_that("Function can be called with `file` argument", {
  des1 <- create_trial_design(file = system.file(package = "mipdtrial", "md/example1.yaml"))
  expect_equal(names(des1), c("target", "sampling", "regimen_update", "sim", "est"))

  ## TODO: add more tests

})
