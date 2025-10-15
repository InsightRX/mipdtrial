
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mipdtrial

<!-- badges: start -->
<!-- badges: end -->

The goal of mipdtrial is to make it easy to simulate pharmacokinetic/
pharmacodynamic (PK/PD) endpoints in response to dose adaptation.

Existing tools are cumbersome to use for this purpose. For example,
tools like NONMEM are optimized for model development, and assume fixed
regimens when used for simulation. Other algorithms, like sample
optimization simulations, optimize for information gain and not for
attainment of clinically relevant metrics, such as AUC target
attainment. `mipdtrial` fills in this niche by helping users simulate
PK/PD resulting from dose adaptations informed by past PK/PD readouts.

Here are some example sorts of questions:

- Will fewer patients receive a therapeutic AUC if we change our
  clinical protocol from collecting a peak and a trough sample to
  collecting a single mid-interval sample?
- How might model misspecification impact patient target attainment?
- How does my institution’s nomogram compare to a model-based dose?

## Installation

You can install the development version of mipdtrial from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("InsightRX/mipdtrial")
```

For examples of how to use the package to answer questions about MIPD
and target attainment, check out the vignettes listed under “Articles!”

## Usage

To use the `mipdtrial` package, it is crucial to understand the concept
of “design” that is introduced in the package. The main goal of the
designs is to allow configuration of flexible trials in which the
sampling and regimen updates can depend on prior regimen changes, such
as when the dosing interval is changed, but we still want to sample at
dose 7 regardless of the dosing interval. Or, when infusion lengths are
changed, but we still want to sample a peak sample. When times are
pre-specified and fixed this flexibility is not possible.

The following designs need to be configured for every trial simulation:

- `sampling_design` : determines at what timepoints samples are taken.
- `target_design`: determines at what timepoint the target should be
  measured, and what the target is.
- `regimen_update_design` : determines at what timepoint the dose can be
  updated in response to new information sampled using the
  `sampling_design`, and how to optimize the dosing regimen.

All of these three designs can be “anchored” to a specific dose or day
number. They can also be offset from the dosing time to e.g. sample at
“peak” or “trough” times. Here is an example of how to set up a design
for a simulated MIPD trial:

``` r
## sample at peak (at 1-hour infusion end), and at true trough
## do this at dose #1 and #3
tdm_design <- create_sampling_design(
  when = c("peak", "trough", "peak", "trough"),
  at = c(1, 1, 3, 3),
  anchor = "dose"
)

## Now sample slightly more realistically, half an hour after infusion end,
## and half an hour before true trough. We can use `offset` for this:
tdm_design <- create_sampling_design(
  when = c("peak", "trough", "peak", "trough"),
  offset = c(0.5, -0.5, 0.5, -0.5), 
  at = c(1, 1, 3, 3),
  anchor = "dose"
)

## If you know the sampling times and dosing intervals are not going 
## to change during the trial, you could also specify this design simply
## using fixed times as: (assuming 12-hour intervals)
tdm_design <- create_sampling_design(
    time = c(1.5, 11.5, 25.5, 35.5)
)

## For targets, we follow broadly the same concept. To target an AUC4 of 
## 400-600 at day 6, we can write:
target_design <- create_target_design(
  targettype = "auc24", 
  targetmin = 400,
  targetmax = 600,
  at = 6,
  anchor = "day"
)

## And for regimen update designs, it works similar as well. The following code
## implements dose updates at dose #3 and #5, using a MAP-based optimization.
dose_update_design <- create_regimen_update_design(
  at = c(3, 5),
  anchor = "dose",
  update_type = "dose",
  dose_optimization_method = map_adjust_dose
)
```

The vignettes show various additional examples of how to set up the
trial simulations using designs. If you find an example of an MIPD trial
design that cannot be captured yet using these functions, please let us
know.

## Roadmap

The `mipdtrial` package is currently under development, and there will
likely be changes to core functionality in the upcoming months. Features
that are on our short-term roadmap:

- Add more optimization functions
- Add more initial dosing functions
- Add further functionality to generate more realistic trial scenarios

## Contributing

We welcome input from the community:

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/InsightRX/mipdtrial/issues) on the GitHub
  page. Please include a reproducible example of the unexpected
  behavior.

- Please [open a pull
  request](https://github.com/InsightRX/mipdtrial/pulls) if you have a
  fix or updates that would improve the package. If you’re not sure if
  your proposed changes are useful or within scope of the package, feel
  free to contact one of the authors of this package.

## Disclaimer

The functionality in this R package is provided “as is”. While its
authors adhere to software development best practices, the software may
still contain unintended errors.

InsightRX Inc. and the authors of this package can not be held liable
for any damages resulting from any use of this software. By the use of
this software package, the user waives all warranties, expressed or
implied, including any warranties to the accuracy, quality or
suitability of InsightRX for any particular purpose, either medical or
non-medical.

------------------------------------------------------------------------

<div align="right">

©
<img src="man/figures/insightrx_logo_color.png" alt="InsightRX logo" width="120" />

</div>
