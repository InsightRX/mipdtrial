% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_dose_update_numbers_from_scheme.R
\name{get_dose_update_numbers_from_design}
\alias{get_dose_update_numbers_from_design}
\title{Get dose number to update dose/interval at from the regime update scheme
and a provided regimen.}
\usage{
get_dose_update_numbers_from_design(design, regimen)
}
\arguments{
\item{design}{regimen update design created using
\code{create_regimen_update_design()}}

\item{regimen}{a \code{PKPDsim} regimen object}
}
\value{
vector of dose numbers to update at
}
\description{
Get dose number to update dose/interval at from the regime update scheme
and a provided regimen.
}
