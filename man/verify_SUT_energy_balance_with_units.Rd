% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/energy_balance.R
\name{verify_SUT_energy_balance_with_units}
\alias{verify_SUT_energy_balance_with_units}
\title{Confirm that an SUT-style data frame conserves energy.}
\usage{
verify_SUT_energy_balance_with_units(
  .sutmats = NULL,
  R = "R",
  U = "U",
  V = "V",
  Y = "Y",
  S_units = "S_units",
  tol = 1e-06,
  SUT_prod_energy_balance = ".SUT_prod_energy_balance",
  SUT_ind_energy_balance = ".SUT_ind_energy_balance"
)
}
\arguments{
\item{.sutmats}{an SUT-style data frame containing columns
\code{R} (optionally), \code{U}, \code{V}, \code{Y}, and \code{S_units}.}

\item{R}{resource (\code{R}) matrix or name of the column in \code{.sutmats} that contains same. Default is "R".}

\item{U}{use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "U".}

\item{V}{make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "V".}

\item{Y}{final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "Y".}

\item{S_units}{\code{S_units} matrix or name of the column in \code{.sutmats} that contains same. Default is "S_units".}

\item{tol}{the maximum amount by which energy can be out of balance. Default is \code{1e-6}.}

\item{SUT_prod_energy_balance}{the name for booleans telling if product energy is in balance. Default is ".SUT_prod_energy_balance".}

\item{SUT_ind_energy_balance}{the name for booleans telling if product energy is in balance. Default is ".SUT_inds_energy_balance".}
}
\value{
\code{.sutmats} with additional columns.
}
\description{
If energy is in balance for every row, \code{.sutmats} is returned with two additional columns, and
execution returns to the caller.
If energy balance is not observed for one or more rows,
a warning is emitted, and
columns named \code{SUT_prod_energy_blance} and \code{SUT_ind_energy_blance} are added to \code{.sutmats}.
\code{FALSE} indicates energy is not in balance.
}
\details{
This function should be called
for its side-effect of testing whether energy is in balance in \code{.sutmats}.

Both product and industry energy balance are verified.
Units (as supplied by the \code{S_units} matrix) are respected.
}
\examples{
library(tidyr)
verify_SUT_energy_balance_with_units(UKEnergy2000mats \%>\%
                                       tidyr::spread(key = matrix.name, value = matrix),
                                       tol = 1e-3)
}
