% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reconstruct.R
\name{remove_neu}
\alias{remove_neu}
\title{Remove Non-energy use from an energy conversion chain}
\usage{
remove_neu(
  .sutmats,
  neu_product_pattern = RCLabels::make_or_pattern(c("NEU", nonenergy_products)),
  neu_industry_pattern = RCLabels::make_or_pattern("Non-energy use", pattern_type =
    "leading"),
  method = "solve",
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  S_units = Recca::psut_cols$S_units,
  prime_suffix = "_prime",
  R_prime = paste0(Recca::psut_cols$R, prime_suffix),
  U_prime = paste0(Recca::psut_cols$U, prime_suffix),
  U_feed_prime = paste0(Recca::psut_cols$U_feed, prime_suffix),
  U_eiou_prime = paste0(Recca::psut_cols$U_eiou, prime_suffix),
  r_eiou_prime = paste0(Recca::psut_cols$r_eiou, prime_suffix),
  V_prime = paste0(Recca::psut_cols$V, prime_suffix),
  Y_prime = paste0(Recca::psut_cols$Y, prime_suffix),
  S_units_prime = paste0(Recca::psut_cols$S_units, prime_suffix)
)
}
\arguments{
\item{.sutmats}{A data frame or named list of PSUT matrices.}

\item{neu_product_pattern}{A string regex pattern that identifies Non-energy use rows
in the final demand (\strong{Y}) matrix.
Default is \code{RCLabels::make_or_pattern(c("NEU", nonenergy_products))},
meaning that the "NEU" product and several other non-energy uses
will be removed as product rows.}

\item{neu_industry_pattern}{A string regex pattern that identifies Non-energy use columns
in the final demand (\strong{Y}) matrix.
Default is \code{RCLabels::make_or_pattern("Non-energy use", pattern_type = "leading")},
which specifies that the string "Non-energy use"
is the leading part of a column name in the \strong{Y} matrix.
This approach allows all "Non-energy use in X" columns to be removed.}

\item{method}{The method by which upstream swim will be conducted.
Default is "solve" for the usual \code{solve()} in \code{R}.
This argument is passed to \code{matsbyname::invert_byname()}.}

\item{R, U, U_feed, U_eiou, r_eiou, V, Y, S_units}{String names for matrix columns or list items in \code{.psut_mats}.
Alternatively, these arguments can be single matrices.
Default values are strings from \code{Recca::psut_cols}.}

\item{prime_suffix}{The suffix for names of modified matrices.
Default is "_prime".}

\item{R_prime, U_prime, U_feed_prime, U_eiou_prime, r_eiou_prime, V_prime, Y_prime, S_units_prime}{Output names.
Defaults are formed by appending "_prime" to
default names.}
}
\value{
A version of the energy conversion chain with Non-energy use
industries and products removed.
}
\description{
Frequently, Non-energy use should be removed from an energy conversion chain,
in final demand, at the useful stage, at the final stage, and at the primary stage.
This function removes Non-energy use via an "upstream swim" with \code{new_Y()}.
}
\details{
Although this function is \code{remove_neu()},
it can be used to remove any row or column from the final demand matrix
and calculate the remaining ECC
by overriding the default value for the \code{neu_industry_pattern}
or \code{neu_product_pattern} arguments.

Note that if a data frame or a list is passed in \code{.sutmats},
the original matrices will remain as columns, and
new columns will be added with \verb{*_prime} matrices.
Callers may wish to delete the original matrices
in the returned data frame.
}
\examples{
UKEnergy2000mats |>
  tidyr::spread(key = matrix.name, value = matrix) |>
  dplyr::filter(Last.stage != "Services") |>
  # This data frame does not contain "Non-energy use",
  # so remove "Residential" instead.
  Recca::remove_neu(neu_product_pattern = "^NG|^MD",
                    neu_industry_pattern = "^Residential")
}
