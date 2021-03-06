% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{products_unit_homogeneous}
\alias{products_unit_homogeneous}
\title{Tell whether ECC products are unit-homogenous}
\usage{
products_unit_homogeneous(
  .sutmats = NULL,
  S_units = "S_units",
  keep_details = FALSE,
  products_unit_homogeneous = ".products_unit_homogeneous"
)
}
\arguments{
\item{.sutmats}{a data frame of supply-use table matrices with matrices arranged in columns.}

\item{S_units}{an \code{S_units} matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{S_units}".}

\item{keep_details}{if \code{TRUE}, per-product results are returned;
if \code{FALSE}, per-ECC results are returned.}

\item{products_unit_homogeneous}{name for the boolean that tells whether products in \code{S_units} are unit-homogeneous on output.
Default is "\code{.products_unit_homogeneous}".}
}
\value{
a list or data frame containing \code{TRUE} if products in \code{S_units} are unit-homogeneous, \code{FALSE} otherwise.

importFrom magrittr extract2
}
\description{
Returns \code{TRUE} if products are unit-homogeneous
according to the \code{S_units} matrix and
\code{FALSE} otherwise.
}
\examples{
library(magrittr)
library(tidyr)
UKEnergy2000mats \%>\%
  spread(key = "matrix.name", value = "matrix") \%>\%
  products_unit_homogeneous() \%>\%
  extract2(".products_unit_homogeneous")
}
