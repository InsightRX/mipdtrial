# Package index

## All functions

- [`adjust_dose_checks()`](adjust_dose_checks.md) : Checks for
  dose_update_number obtained from dose_update scheme
- [`bind_results_from_adjustments()`](bind_results_from_adjustments.md)
  : Bind together the results from sampling and dose adjusting
- [`bind_sim_output()`](bind_sim_output.md) : Bind simulated
  subject-level output together
- [`calc_auc_from_sim()`](calc_auc_from_sim.md) : Get AUC from a
  simulation
- [`calc_time_to_target()`](calc_time_to_target.md) : Calculate time to
  target attainment
- [`check_trial_design()`](check_trial_design.md) : Check design
- [`check_when()`](check_when.md) : Check / clean when element
- [`collect_tdms()`](collect_tdms.md) : Simulate TDM collection
- [`create_cov_object()`](create_cov_object.md) : Create a list of
  PKPDsim covariates for modeling
- [`create_design()`](create_design.md) : Create timing designs (static
  or adaptive) for use in simulated trial, such as sampling designs,
  target designs etc.
- [`create_eval_design()`](create_eval_design.md) : Create evaluation
  object
- [`create_initial_regimen_design()`](create_initial_regimen_design.md)
  : Creates a design for the initial regimen for patients in the trial
- [`create_model_design()`](create_model_design.md) : Create a design
  for models to be used
- [`create_regimen_update_design()`](create_regimen_update_design.md) :
  Create scheme for updating dose or interval during dose optimization
  trial
- [`create_sampling_design()`](create_sampling_design.md) : Function for
  creating sampling designs.
- [`create_target_design()`](create_target_design.md) : Create target
  object
- [`create_trial_design()`](create_trial_design.md) : Combine all
  sub-designs into the overall trial design object
- [`dose_grid_search()`](dose_grid_search.md) : Perform a grid search
  for a particular target by simulating a grid of doses
- [`calc_concentration_from_regimen()`](exposure_metrics.md)
  [`calc_auc_from_regimen()`](exposure_metrics.md) : Calculate exposure
  metrics
- [`filter_rows_0_100()`](filter_rows_0_100.md) : Filter rows with
  values 0 or 100
- [`generate_iiv()`](generate_variability.md)
  [`generate_ruv()`](generate_variability.md) : Generate variability
  terms
- [`get_dose_update_core()`](get_dose_update_core.md) : Core function to
  calculate the dose update number for a row in a regimen update
  data.frame
- [`get_dose_update_numbers_from_design()`](get_dose_update_numbers_from_design.md)
  : Get dose number to update dose/interval at from the regime update
  scheme and a provided regimen.
- [`get_quantity_from_variable()`](get_quantity_from_variable.md) : Get
  quantities from variables in sim results
- [`get_sampling_time_core()`](get_sampling_time_core.md) : Core
  function to calculate the sampling time for a row in a sampling schema
  data.frame.
- [`get_sampling_times_from_scheme()`](get_sampling_times_from_scheme.md)
  : Calculate sampling times based on a given sampling schema and a
  regimen.
- [`get_single_target_design()`](get_single_target_design.md) : Get a
  single target from a potentially time-varying target design
- [`is_on_target()`](is_on_target.md) : Checks if a value (or vector of
  values) is within the specified target range
- [`is_single_valid_number()`](is_single_valid_number.md) : Checks that
  an object represents a single finite number
- [`is_valid_number()`](is_valid_number.md) : Checks that an object
  represents a single finite number
- [`is_valid_numeric_vector()`](is_valid_numeric_vector.md) : Checks
  that an object represents a vectir of finite number with no NA or NaN
  or Inf
- [`map_adjust_dose()`](map_adjust_dose.md) : Adjust doses to achieve a
  target metric using MAP Bayesian estimation.
- [`map_adjust_interval()`](map_adjust_interval.md) : Adjust intervals
  to achieve a target metric using MAP Bayesian estimation by adapting
  the dosing interval
- [`mipd_target_types()`](mipd_target_types.md) : Accepted PK/PD
  exposure targets
- [`mipdtrial`](mipdtrial-package.md)
  [`mipdtrial-package`](mipdtrial-package.md) : MIPDtrial package
- [`model_based_starting_dose()`](model_based_starting_dose.md) :
  Model-based starting dose
- [`parse_spec_file_to_trial_design()`](parse_spec_file_to_trial_design.md)
  : Parse YAML spec file to trial design
- [`round_to_multiple()`](round_to_multiple.md) : Round to a multiple of
  any number (e.g. round to the nearest 5, 10, 100)
- [`run_trial()`](run_trial.md) : Run an MIPD trial
- [`sample_and_adjust_by_dose()`](sample_and_adjust_by_dose.md) : Adjust
  dosing using MIPD on TDMs at specified dose numbers
- [`sim_subject()`](sim_subject.md) : Core function to simulate a single
  subject
- [`simulate_dose_interval()`](simulate_dose_interval.md) : Simulate
  different doses/intervals in a dose/interval grid
- [`simulate_fit()`](simulate_fit.md) : Get MAP Bayesian parameters
- [`update_regimen()`](update_regimen.md) : Update a regimen with a new
  dose
- [`weight_based_starting_dose()`](weight_based_starting_dose.md) :
  Weight-based starting dose (e.g., mg/kg)
