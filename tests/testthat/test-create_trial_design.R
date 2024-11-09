test_that("Function can be called with `file` argument", {
  des1 <- create_trial_design(
    file = system.file(package = "mipdtrial", "md/example1.yaml")
  )
  expected_names <- c(
    "initial_regimen", "target", "sampling", "regimen_update", "sim", "est"
  )
  expect_true(all(expected_names %in% names(des1)))
})
