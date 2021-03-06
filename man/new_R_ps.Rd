% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reconstruct.R
\name{new_R_ps}
\alias{new_R_ps}
\title{Assess the effect of new levels of resources}
\usage{
new_R_ps(
  .sutmats = NULL,
  R_prime = "R_prime",
  U = "U",
  V = "V",
  Y = "Y",
  S_units = "S_units",
  q = "q",
  C = "C",
  eta_i = "eta_i",
  maxiter = 100,
  convergence_tol = 0,
  e_bal_tol = 1e-06,
  U_prime = "U_prime",
  V_prime = "V_prime",
  Y_prime = "Y_prime"
)
}
\arguments{
\item{.sutmats}{a data frame of supply-use table matrices with matrices arranged in columns.}

\item{R_prime}{a new resource matrix or name of a column in \code{.sutmats} containing same. Default is "\code{R_prime}".}

\item{U}{use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".}

\item{V}{make (\code{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "\code{V}".}

\item{Y}{final demand (\code{Y}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{Y}".}

\item{S_units}{\code{S_units} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{S_units}".}

\item{q}{\code{q} vector or name of the column in \code{.sutmats} that contains same. Default is "\code{q}".}

\item{C}{a \code{C} matrix or name of a column in \code{.sutmats} containing same. Default is "\code{C}".}

\item{eta_i}{an \code{eta_i} vector or name of a column in \code{.sutmats} containing same. Default is "\code{eta_i}".}

\item{maxiter}{the maximum allowable number of iterations when calculating the effects of a new \code{R} matrix.
Default is \code{100}.}

\item{convergence_tol}{the maximum allowable change in any one entry of the \code{U}, \code{V}, and \code{Y} matrices
from one iteration to the next. Default is 0,
i.e., when two subsequent iterations produce the same values,
the algorithm has converged.}

\item{e_bal_tol}{the maximum allowable difference in energy ECC energy balance. Default is \code{1e-6}.}

\item{U_prime}{name for the \code{U_prime} matrix on output. Default is "\code{U_prime}".}

\item{V_prime}{name for the \code{V_prime} matrix on output. Default is "\code{V_prime}".}

\item{Y_prime}{name for the \code{Y_prime} matrix on output. Default is "\code{Y_prime}".}
}
\value{
a list or data frame containing \code{U_prime}, \code{V_prime}, and \code{Y_prime} matrices
}
\description{
This function calculates the effect of changing the resources available to an energy conversion chain.
New versions of \code{U}, \code{V}, and \code{Y} matrices are returned
as \code{U_prime}, \code{V_prime}, and \code{Y_prime}.
This function assumes that each industry's inputs are perfectly substitutable (ps).
}
\details{
Inputs \code{U}, \code{V}, \code{Y},
\code{S_units}, \code{q}, and \code{C}
can be
conveniently calculated by the function \code{\link{calc_io_mats}};
\code{eta_i} can be calculated with \code{\link{calc_eta_i}}.

Each industry must be unit-homogeneous on its inputs.
If not, \code{NA} is returned as the result for \code{U_prime}, \code{V_prime}, and \code{Y_prime}.
}
\examples{
library(dplyr)
library(matsbyname)
library(tidyr)
doubleR <- UKEnergy2000mats \%>\%
  spread(key = "matrix.name", value = "matrix") \%>\%
  # Calculate the input-output matrices which are inputs to the new_R function.
  calc_io_mats() \%>\%
  # Calculate the efficiency of every industry in the ECC.
  calc_eta_i() \%>\%
  # Make an R_prime matrix that gives twice the resource inputs to the economy.
  mutate(
    R_prime = hadamardproduct_byname(2, R)
  ) \%>\%
  # Now call the new_R function which will calculate
  # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
  # given R_prime.
  # Each of the *_prime matrices should be 2x their originals,
  # because R_prime is 2x relative to R.
  # Rows with Last.stage == "services" are NA.
  new_R_ps()
doubleR$U_prime[[1]]
doubleR$V_prime[[1]]
doubleR$Y_prime[[1]]
}
