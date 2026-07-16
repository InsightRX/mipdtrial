# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Commands

``` r

# Load package during development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
devtools::load_all(); testthat::test_file("tests/testthat/test-collect_tdms.R")

# Run a specific test by name
devtools::load_all(); testthat::test_file("tests/testthat/test-collect_tdms.R", filter = "LLOQ")

# Check the package (includes R CMD check)
devtools::check()

# Regenerate documentation from roxygen comments
devtools::document()
```

## Architecture

**mipdtrial** simulates Model-Informed Precision Dosing (MIPD) trials:
given a virtual patient population, it iteratively collects drug levels
(TDMs), fits a PK model via MAP Bayesian estimation, and adjusts doses
to hit a pharmacokinetic target (AUC, trough concentration, etc.).

### Call hierarchy

    run_trial()                          # user entry point; parallelises over subjects
    └─ sim_subject()                     # one subject
       ├─ create_cov_object()            # data row → PKPDsim covariate list
       ├─ initial_regimen$method()       # e.g. model_based_starting_dose()
       └─ sample_and_adjust_by_dose()    # main dose-adjustment loop
          ├─ collect_tdms()              # simulate drug levels + residual error
          ├─ map_adjust_dose() / map_adjust_interval()
          │  ├─ map_fit()                # MAP Bayesian estimation via PKPDmap
          │  └─ dose_grid_search()       # find optimal dose/interval
          └─ update_regimen()            # apply new dose to PKPDsim regimen

### The design system

Everything is configured via a **trial design** object (built by
[`create_trial_design()`](reference/create_trial_design.md)), which
holds six sub-designs:

| Sub-design | Key function | Controls |
|----|----|----|
| `sim` / `est` | [`create_model_design()`](reference/create_model_design.md) | PKPDsim models and parameters for simulation (“truth”) and estimation |
| `sampling` | [`create_sampling_design()`](reference/create_sampling_design.md) | When TDMs are collected; adaptive (peak/trough/dose-relative) or fixed times; LLOQ |
| `target` | [`create_target_design()`](reference/create_target_design.md) | PK/PD target (AUC, trough, `%T>MIC`, …); can be time-varying across updates |
| `regimen_update` | [`create_regimen_update_design()`](reference/create_regimen_update_design.md) | Which doses trigger updates; which optimisation function to use and its arguments |
| `initial_regimen` | [`create_initial_regimen_design()`](reference/create_initial_regimen_design.md) | Starting dose method |
| `evaluation` | [`create_eval_design()`](reference/create_eval_design.md) | Non-target metrics computed post-hoc |

Trial designs can also be loaded from a YAML file via
`create_trial_design(file = "spec.yaml")`.

**Important:** optimization functions (e.g. `map_adjust_dose`) are
**copied by value** into the design object. If you edit one of those
functions you must re-run
[`create_regimen_update_design()`](reference/create_regimen_update_design.md)
to pick up the change.

### Key data structures

**`tdms` data frame** (output of `collect_tdms`, accumulated in
`sample_and_adjust_by_dose`): - `t`, `obs_type`, `true_y` – simulated
truth - `y` – measured level (truth + residual error; LLOQ-censored if
applicable) - `predictive_ipred` – prediction from the estimation model
using current parameter estimates (population prior on dose 1, MAP
estimates thereafter); `NA` when `est_design` is not supplied

**`design$est` / `design$sim`** both have: `model` (PKPDsim ODE object),
`parameters` (named list), `omega_matrix`, `ruv` (list with
`prop`/`add`).

**[`run_trial()`](reference/run_trial.md) return value** (class
`mipdtrial_results`): a list of data frames — `tdms`, `dose_updates`,
`final_reg`, `additional_info` (MAP estimates per update), `gof`,
`final_exposure`, `eval_exposure`.

### Sampling and target time anchoring

[`create_sampling_design()`](reference/create_sampling_design.md) and
[`create_target_design()`](reference/create_target_design.md) both
accept `anchor = "dose"` or `anchor = "day"`, and `when` values such as
`"dose"`, `"peak"`, `"trough"` which are resolved to absolute times at
runtime by
[`get_sampling_times_from_scheme()`](reference/get_sampling_times_from_scheme.md).
This is why all timing-related functions receive the *current* `regimen`
object — absolute times can change as doses are updated.

### Separate sim/est models

`sim_model` and `est_model` can differ to simulate model
misspecification. True patient parameters (`pars_true_i`) must match
`sim_model`; estimation uses `design$est` parameters as the population
prior. Covariates must be compatible with both models.

### Parallelism

[`run_trial()`](reference/run_trial.md) uses
[`furrr::future_map()`](https://furrr.futureverse.org/reference/future_map.html)
— parallelism is controlled by the caller setting a `future` plan before
calling [`run_trial()`](reference/run_trial.md). Use
`future::plan(multisession)` to enable parallel execution.

## Dependencies

- **PKPDsim**: ODE-based PK/PD simulation (all models, regimens,
  covariates)
- **PKPDmap**: MAP Bayesian estimation (`simulate_fit`, underlying
  estimation engine)
- **yaml**: YAML-based design spec loading
- Literature model packages (e.g. `pkbusulfanmccune`) are auto-installed
  by tests via
  [`PKPDsim::install_default_literature_model()`](https://insightrx.github.io/PKPDsim/reference/install_default_literature_model.html)
  if absent
