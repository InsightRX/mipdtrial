# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## Commands

```r
# Load package during development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
devtools::load_all(); testthat::test_file("tests/testthat/test-collect_tdms.R")

# Run a single test by its description (`desc`, not `filter`)
devtools::load_all(); testthat::test_file("tests/testthat/test-collect_tdms.R", desc = "handles LLOQ correctly")

# Run the test files whose names match a pattern
devtools::test(filter = "collect_tdms")

# Check the package (includes R CMD check)
devtools::check()

# Regenerate documentation from roxygen comments
devtools::document()

# Rebuild README.md (it is generated — never edit README.md by hand)
devtools::build_readme()
```

`NAMESPACE` and everything under `man/` are roxygen2-generated; edit the roxygen
comments above the function and re-run `devtools::document()` instead of editing
those files directly. The same applies to `README.md`, which is knit from
`README.Rmd`.

CI (`.github/workflows/R-CMD-check.yaml`) runs `R CMD check` on ubuntu-latest and
windows-latest against R release, on pushes and PRs to `main` or `master`, and on
`workflow_dispatch`. Vignettes are built during check, so a broken vignette fails
CI.

## Architecture

**mipdtrial** simulates Model-Informed Precision Dosing (MIPD) trials: given a virtual patient population, it iteratively collects drug levels (TDMs), fits a PK model via MAP Bayesian estimation, and adjusts doses to hit a pharmacokinetic target (AUC, trough concentration, etc.).

### Call hierarchy

```
run_trial()                          # user entry point; parallelises over subjects
└─ sim_subject()                     # one subject
   ├─ create_cov_object()            # data row → PKPDsim covariate list
   ├─ initial_regimen$method()       # e.g. model_based_starting_dose()
   └─ sample_and_adjust_by_dose()    # main dose-adjustment loop
      ├─ collect_tdms()              # simulate drug levels + residual error
      ├─ map_adjust_dose() / map_adjust_interval()
      │  ├─ simulate_fit()           # MAP Bayesian estimation via PKPDmap (in R/map_fit.R)
      │  └─ dose_grid_search()       # find optimal dose/interval
      └─ update_regimen()            # apply new dose to PKPDsim regimen
```

### The design system

Everything is configured via a **trial design** object (built by `create_trial_design()`), which holds seven named sub-designs (`sim` and `est` are separate list elements):

| Sub-design | Key function | Controls |
|---|---|---|
| `sim` / `est` | `create_model_design()` | PKPDsim models and parameters for simulation ("truth") and estimation |
| `sampling` | `create_sampling_design()` | When TDMs are collected; adaptive (peak/trough/dose-relative) or fixed times; LLOQ |
| `target` | `create_target_design()` | PK/PD target (AUC, trough, `%T>MIC`, …); can be time-varying across updates |
| `regimen_update` | `create_regimen_update_design()` | Which doses trigger updates; which optimisation function to use and its arguments |
| `initial_regimen` | `create_initial_regimen_design()` | Starting dose method |
| `evaluation` | `create_eval_design()` | Non-target metrics computed post-hoc |

Trial designs can also be loaded from a YAML file via `create_trial_design(file = "spec.yaml")`. The YAML keys mirror the sub-design names and their arguments one-to-one; see `inst/md/*.yaml` for worked examples and the "sample timing (YAML)" vignette.

`create_trial_design()` runs the design through `check_trial_design()`, which
enforces the cross-design invariants — most importantly that
`nrow(design$target$scheme)` is either 1 or equal to
`nrow(design$regimen_update$scheme)`, so a time-varying target has exactly one
entry per regimen update. `check_trial_design()` also resolves a
`dose_optimization_method` given as a string into the actual function via `get()`,
which is why both `"map_adjust_dose"` and `map_adjust_dose` are accepted. Add new
cross-design validation there rather than in the individual `create_*_design()`
functions.

**Important:** optimization functions (e.g. `map_adjust_dose`) are **copied by value** into the design object. If you edit one of those functions you must re-run `create_regimen_update_design()` to pick up the change. In an interactive session `devtools::load_all()` alone is not enough — the stale closure stays in the existing design object.

### Key data structures

**`tdms` data frame** (output of `collect_tdms`, accumulated in `sample_and_adjust_by_dose`):
- `t`, `obs_type`, `true_y` – simulated truth
- `y` – measured level (truth + residual error; LLOQ-censored if applicable)
- `predictive_ipred` – prediction from the estimation model using current parameter estimates (population prior on dose 1, MAP estimates thereafter); `NA` when `est_design` is not supplied

**`design$est` / `design$sim`** both have: `model` (PKPDsim ODE object), `parameters` (named list), `omega_matrix`, `ruv` (list with `prop`/`add`).

**`run_trial()` return value** (class `mipdtrial_results`): a mixed list — `tdms`,
`dose_updates`, `final_reg`, `gof`, `final_exposure` and `eval_exposure` are
row-bound data frames; `additional_info` (MAP estimates per update) is not a data
frame, so it stays a list with one element per subject.

The shape of that list is not hard-coded: `bind_sim_output()` walks the names of
whatever `sim_subject()` returned for the first subject, row-binding elements that
are data frames and collecting everything else into a list. To add a new
per-subject output, return it from `sim_subject()` and it flows through
automatically — no change to `bind_sim_output()` or `run_trial()` is required.

### Sampling and target time anchoring

`create_sampling_design()` and `create_target_design()` both accept `anchor = "dose"` or `anchor = "day"`, and `when` values such as `"dose"`, `"peak"`, `"trough"` which are resolved to absolute times at runtime by `get_sampling_times_from_scheme()`. This is why all timing-related functions receive the *current* `regimen` object — absolute times can change as doses are updated.

This late resolution is the core design constraint of the package: never
pre-compute and cache sampling or target times at design-construction time, since
an interval or infusion-length change during the trial invalidates them.

### Separate sim/est models

`sim_model` and `est_model` can differ to simulate model misspecification. True patient parameters (`pars_true_i`) must match `sim_model`; estimation uses `design$est` parameters as the population prior. Covariates must be compatible with both models.

### Inter-occasion variability (IOV)

IOV bins come from the model, not the design: call sites obtain them with
`PKPDsim::get_model_iov(model)$bins` and pass them into the `PKPDsim::sim()` /
`PKPDmap` calls. `get_iov_specification()` expands the parameter and omega
specifications accordingly, and is called from `simulate_fit()` and
`generate_iiv()`.
Any new code path that simulates or fits must forward `iov_bins` the same way, or
IOV is silently dropped for that path.

### Parallelism

`run_trial()` parallelises over subjects and takes a `threads` argument (default
`1`, i.e. sequential via `purrr::map()`). When `threads > 1` it sets up
`future::plan(future::multisession, workers = threads)` itself and uses
`furrr::future_map()` — the caller does not set a `future` plan. The cap in
`R/run_trial.R` is off by one relative to its own warning message: it computes
`n_cores <- max(1, parallel::detectCores() - 1)` and then reduces `threads` to
`n_cores - 1`, so the effective maximum is `detectCores() - 2` and a two-core host
ends up with `threads = 0`. Treat that as a bug to fix rather than behaviour to
preserve.

Reproducibility is per-subject, not per-run: the main loop calls
`set.seed(seed + i)` for subject `i`, so results are identical regardless of the
number of threads. Preserve that property when touching the loop.

## Testing conventions

Tests use testthat edition 3 (`Config/testthat/edition: 3`). `tests/testthat/setup.R`
builds a shared `mod_1cmt_iv` one-compartment IV model with an AUC compartment via
`PKPDsim::new_ode_model("pk_1cmt_iv_auc")` — use it for tests that just need a
working model. The same file installs the `pkbusulfanmccune` literature model with
`PKPDsim::install_default_literature_model()` when absent, so the first test run on
a clean machine reaches the network and is slow.

## Dependencies

`Imports` holds exactly three packages:

- **PKPDsim**: ODE-based PK/PD simulation (all models, regimens, covariates)
- **PKPDmap**: MAP Bayesian estimation (underlying estimation engine, wrapped by `simulate_fit()`)
- **yaml**: YAML-based design spec loading

PKPDsim and PKPDmap are pinned to GitHub via `Remotes:` — they track
`InsightRX/PKPDsim` and `InsightRX/PKPDmap` rather than CRAN, so an unexplained
failure after a dependency update usually means an upstream change on `main` of
those repos.

Everything else (cli, dplyr, ggplot2, purrr, furrr, future, progressr, tidyr,
knitr, rmarkdown, testthat) is in `Suggests`, but the code does not treat it that
way: `run_trial()` and other core functions call `cli::`, `dplyr::`, `purrr::`,
`furrr::`, `future::` and `progressr::` unconditionally, and there is no
`requireNamespace()` guard anywhere in `R/`. Those six are de facto hard
requirements declared as optional. Follow the existing unguarded `::` pattern or
move the package to `Imports` — do not guard one call site while the rest stay
unguarded. `parallel` is used in `run_trial()` but declared nowhere; it ships with
R, so it works.

Literature model packages (e.g. `pkbusulfanmccune`) are not declared at all; tests
install them via `PKPDsim::install_default_literature_model()` when absent.
