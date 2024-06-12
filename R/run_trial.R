#' Run an MIPD trial
#'
#' This function is a wrapper function to make it easier to run a basic MIPD
#' trial with minimal function calls. For fine-tune control over logic, it may
#' be easier to compose each step of the precision-dosing workflow yourself. For
#' an example, see the "Busulfan MIPD" vignette.
#'
#' This function performs the following steps:
#'
#' 1. Dose the patient using the function specified in `initial_dose_method`.
#' 2. Collect levels at the specified times (`tdm_times`).
#' 3. Adjust dosing at each specified dose (`adjust_doses_at`) using the logic
#'    specified in `dose_optimization_method`
