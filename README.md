
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
