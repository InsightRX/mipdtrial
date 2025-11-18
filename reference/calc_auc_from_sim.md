# Get AUC from a simulation

Get AUC from a simulation

## Usage

``` r
calc_auc_from_sim(sim_output, auc_comp, extract_time, target_type)
```

## Arguments

- sim_output:

  output of a
  [`PKPDsim::sim`](https://insightrx.github.io/PKPDsim/reference/sim.html)
  call

- auc_comp:

  auc compartment

- extract_time:

  time points for AUC12 or 24 to extract

- target_type:

  type of AUC target in `c("auc24", "auc12", "cum_auc")`.

## Value

`calc_auc_from_sim` returns a numeric vector of AUCs between each
simulated time point. Control time period over which AUC should be
calculated using `target_time` argument to
[`PKPDsim::sim`](https://insightrx.github.io/PKPDsim/reference/sim.html).
