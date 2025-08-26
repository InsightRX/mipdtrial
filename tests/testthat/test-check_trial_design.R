test_that("check_trial_design works correctly", {
  
  # Helper function for creating mock design
  create_mock_design <- function(
    initial_regimen = list(method = "model_based_starting_dose"),
    sampling = list(times = c(0, 24)),
    regimen_update = list(dose_optimization_method = "map_adjust_dose"),
    sim = list(ruv = list(proportional = 0.1)),
    est = list(ruv = list(proportional = 0.1)),
    target = list(
      type = "auc24", value = 500, min = 400, max = 600, 
      scheme = structure(list(
        base = "dose", offset = 24, at = 6, anchor = "day", scatter = 0), 
        class = "data.frame", row.names = c(NA, -1L))
    )
  ) {
    list(
      initial_regimen = initial_regimen,
      sampling = sampling,
      regimen_update = regimen_update,
      sim = sim,
      est = est,
      target = target
    )
  }
  
  # Test case 1: Valid design passes without errors
  valid_design <- create_mock_design()
  result <- check_trial_design(valid_design)
  expect_equal(class(result), "list")
  expect_equal(class(result$initial_regimen$method), "function")
  
  # Test case 2: Missing initial_regimen throws error
  design_no_init <- create_mock_design(initial_regimen = NULL)
  expect_error(
    check_trial_design(design_no_init),
    "Please specify initial regimen design."
  )
  
  # Test case 3: Missing sampling gives warning
  design_no_sampling <- create_mock_design(sampling = NULL)
  expect_warning(
    check_trial_design(design_no_sampling),
    "No `sampling_design` provided, will not perform sampling."
  )
  
  # Test case 4: Missing regimen_update gives warning
  design_no_regimen_update <- create_mock_design(regimen_update = NULL)
  expect_warning(
    check_trial_design(design_no_regimen_update),
    "No `regimen_update_design` provided, will not perform regimen optimization."
  )
  
  # Test case 5: Missing sim model throws error
  design_no_sim <- create_mock_design(sim = NULL)
  expect_error(
    check_trial_design(design_no_sim),
    "A simulation model is required."
  )
  
  # Test case 6: Missing est model uses sim model and gives warning
  design_no_est <- create_mock_design(est = NULL)
  expect_warning(
    result <- check_trial_design(design_no_est),
    "No model for estimation \\(`est`\\) defined, using same model as specified for simulations."
  )
  expect_equal(result$est, result$sim)
  
  # Test case 7: Zero residual error in est model throws error
  design_zero_ruv <- create_mock_design(est = list(ruv = list(proportional = 0)))
  expect_error(
    check_trial_design(design_zero_ruv),
    "Residual error magnitude for estimation model cannot be zero."
  )
  
  # Test case 8: Character method in regimen_update gets converted to function
  # Mock a function that exists in the global environment
  mock_optimizer <- function(x) x
  assign("mock_optimizer", mock_optimizer, envir = .GlobalEnv)
  
  design_char_method <- create_mock_design(
    regimen_update = list(dose_optimization_method = "mock_optimizer")
  )
  result <- check_trial_design(design_char_method)
  expect_equal(result$regimen_update$dose_optimization_method, mock_optimizer)
  
  # Test case 9: Character method in initial_regimen gets converted to function
  mock_method <- function(x) x
  assign("mock_method", mock_method, envir = .GlobalEnv)
  
  design_char_init <- create_mock_design(
    initial_regimen = list(method = "mock_method")
  )
  result <- check_trial_design(design_char_init)
  expect_equal(result$initial_regimen$method, mock_method)
  
  # Clean up
  rm("mock_optimizer", "mock_method", envir = .GlobalEnv)
  
  # Test case 10: Complex ruv structure with zero sum throws error
  design_complex_zero_ruv <- create_mock_design(
    est = list(ruv = list(proportional = 0, additive = 0))
  )
  expect_error(
    check_trial_design(design_complex_zero_ruv),
    "Residual error magnitude for estimation model cannot be zero."
  )
  
  # Test case 11: Complex ruv structure with non-zero sum passes
  design_complex_nonzero_ruv <- create_mock_design(
    est = list(ruv = list(proportional = 0.1, additive = 0.05))
  )
  result <- check_trial_design(design_complex_nonzero_ruv)
  expect_equal(result$est$ruv$proportional, 0.1)
  expect_equal(result$est$ruv$additive, 0.05)
  
  # Test case 12: missing target design should pass fine
  design_no_target <- create_mock_design(target = NULL)
  result <- check_trial_design(design_no_target)
  
  # Test case 13: time-varying target design passes
  tv_target <- create_target_design(
    targettype = "auc24", 
    targetvalue = c(500, 300),
    at = c(5, 10),
    anchor = "dose"
  )
  dose_update_design <- create_regimen_update_design(
    at = c(3, 7),
    anchor = "dose",
    update_type = "dose",
    dose_optimization_method = map_adjust_dose
  )
  design_tv_target <- create_mock_design(target = tv_target, regimen_update = dose_update_design)
  result <- check_trial_design(design_tv_target)
  expect_equal(nrow(result$target$scheme), 2)
})

