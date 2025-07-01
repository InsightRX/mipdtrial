
test_that("Can also run trial without sampling and dose optimization (initial dose only)", {

  mod_design <- create_model_design(
    model = mod_1cmt_iv, # from setup.R
    parameters = list(CL = 1, V = 10),
    omega_matrix = c(0.1, 0.05, 0.1),
    ruv = list(prop = 0.1, add = 0.1)
  )
  target_design <- create_target_design(
    targetvalue = 100,
    targettype = "cum_auc",
    at = 8,
    anchor = "dose"
  )
  initial_method <- create_initial_regimen_design(
    function(design,
             covariates,
             cov_mapping,
             ...) {
      PKPDsim::new_regimen(
        amt = 100,
        type = "infusion",
        t_inf = 1,
        n = 10,
        interval = 24
      )
    }
  )
  expect_warning(expect_warning(
    trial <- create_trial_design(
      initial_regimen_design = initial_method,
      sampling_design = NULL,
      target_design = target_design,
      regimen_update_design = NULL,
      sim_design = mod_design,
      est = mod_design
    )
  ))
  data <- data.frame(
    ID = c(1:10),
    iteration = c(1:10),
    WT = 70
  )
  res <- run_trial(
    data = data,
    design = trial,
    cov_mapping = list(WT = "WT")
  )
  expect_equal(
    round(res$final_exposure$auc_true, 1),
    c(840.6, 905.4, 859.6, 599.8, 1053.2, 583.5, 308.2, 795.3, 753.6,
      667.7)
  )
  expect_true(all(is.na(res$final_exposure$auc_est)))
})
