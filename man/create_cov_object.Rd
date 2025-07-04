% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_cov_object.R
\name{create_cov_object}
\alias{create_cov_object}
\title{Create a list of PKPDsim covariates for modeling}
\usage{
create_cov_object(dat_i, mapping, implementation = NULL, time_column = NULL)
}
\arguments{
\item{dat_i}{a rectangular data set, containing only rows for an individual}

\item{mapping}{named vector or list indicating which columns to extract, and what
the covariate names are expected in the model. Names should be model
covariates and values should be data frame column names.}

\item{implementation}{named vector indicating covariate implementation, one
of \code{c("interpolate", "locf")}. If NULL, assumes "interpolate" for all
covariates. See \code{PKPDsim::new_covariate} for details. Names should be model
covariates and values should be implementation method.}

\item{time_column}{if covariates are time-varying, supply a column for time.
Otherwise, initial value only is taken for all covariates.}
}
\value{
Returns a named list of PKPDsim covariates.
}
\description{
Data sets typically come in rectangular formats while for MIPD trials we
model each individual separately. PKPDsim expects covariates to be provided
as a list of PKPDsim covariates. This function helps convert rectangular
(wide) data to model-ready covariates.
}
\details{
The data is assumed to already be numeric (e.g., categorical covariates like
smoking status have already been mapped to 1 or 0).

The function returns NULL if required columns are missing or NA.
}
\examples{
dat <- data.frame(ID = 1, weight = 70, CRCL = 4)
mapping <- c(WT = "weight", CRCL = "CRCL")
create_cov_object(dat, mapping)
}
