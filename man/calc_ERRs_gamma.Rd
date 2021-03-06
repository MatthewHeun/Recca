% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/errs.R
\name{calc_ERRs_gamma}
\alias{calc_ERRs_gamma}
\title{Calculate energy return ratios for the gamma system boundary.}
\usage{
calc_ERRs_gamma(
  .sutmats,
  U = "U",
  r_EIOU = "r_EIOU",
  V = "V",
  g = "g",
  S_units = "S_units",
  ger_gamma = "ger_gamma",
  ner_gamma = "ner_gamma",
  r_gamma = "r_gamma"
)
}
\arguments{
\item{.sutmats}{a data frame containing columns for \code{U} and \code{r_EIOU} matrices and a \code{g} vector.}

\item{U}{a string for the name of a column of \code{U} matrices in \code{.sutmats}. (Default is "\code{U}".)}

\item{r_EIOU}{a string for the name of a column of \code{r_EIOU} matrices in \code{.sutmats}. (Default is "\code{r_EIOU}".)}

\item{V}{a string for the name of a column of \code{V} matrices in \code{.sutmats}. (Default is "\code{V}".)}

\item{g}{a string for the name of a column of \code{g} vector in \code{.sutmats}. (Default is "\code{g}".)}

\item{S_units}{a string for the name of a column of \code{S_units} matrices in \code{.sutmats}. (Default is "\code{S_units}".)}

\item{ger_gamma}{the name of the gross energy ratio column in output. (Default is "\code{ger_gamma}",}

\item{ner_gamma}{the name of the net energy ratio column in output. (Default is "\code{ner_gamma}",)}

\item{r_gamma}{the name of the ratio of energy ratios in output. (Default is "\code{r_gamma}",}
}
\value{
\code{.sutmats} with additional columns "\code{ner_gamma}", "\code{ger_gamma}", and "\code{r_gamma}".
}
\description{
Calculates energy return ratios for the gamma system boundary.
Calculations are performed as shown in Equations 8, 9, and 10 in
Heun, Owen, and Brockway. 2018.
A physical supply-use table framework for energy analysis on the energy conversion chain.
Applied Energy, vol 226, pp. 1134-1162.
}
\details{
The energy return ratios for a given industry are calculated
iff the units for inputs and outputs for that industry are unit-homogeneous.
If units for inputs or outputs are heterogeneous for an industry, \code{NA} is the result.
}
\examples{
library(tidyr)
library(Recca)
UKEnergy2000mats \%>\%
  spread(key = "matrix.name", value = "matrix") \%>\%
  calc_io_mats() \%>\%
  calc_ERRs_gamma()
}
