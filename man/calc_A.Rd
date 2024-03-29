% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{calc_A}
\alias{calc_A}
\title{Calculate \strong{Z}, \strong{K}, \strong{C}, \strong{D}, \strong{A}, \strong{B}, and \strong{O} matrices}
\usage{
calc_A(
  .sutdata = NULL,
  direction = c("upstream", "demand", "Leontief", "downstream", "supply", "Ghosh"),
  R = "R",
  U = "U",
  U_feed = "U_feed",
  V = "V",
  Y = "Y",
  q = "q",
  f = "f",
  g = "g",
  r = "r",
  h = "h",
  Z = "Z",
  K = "K",
  C = "C",
  D = "D",
  A = "A",
  O = "O",
  Z_s = "Z_s",
  C_s = "C_s",
  D_s = "D_s",
  D_feed_s = "D_feed_s",
  B = "B",
  O_s = "O_s"
)
}
\arguments{
\item{.sutdata}{a data frame of supply-use table matrices with matrices arranged in columns.}

\item{direction}{A string that identifies the directionality of the IO matrices.
See details.
Default is "upstream".}

\item{R}{resources (\strong{R}) matrix or name of the column in \code{.sutmats} that contains same. Default is "R".
\code{R} is an optional argument.
If all of \strong{R} is added to \strong{V}, this argument can be left unspecified.}

\item{U}{Use (\strong{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "U".}

\item{U_feed}{Feed portion of the use matrix (\strong{U_feed}) or name of the column in \code{.sutmats} that contains same. Default is "U_feed".}

\item{V}{Make (\strong{V}) matrix or name of the column in \code{.sutmats}that contains same. Default is "V".}

\item{Y}{Final demand (\strong{Y}) matrix or name of the column in \code{.sutmats}that contains same. Default is "Y".}

\item{q}{A \strong{q} vector or name of the column in \code{.sutmats} that contains same. Default is "q".}

\item{f}{An \strong{f} vector or name of the column in \code{.sutmats} that contains same. Default is "r".}

\item{g}{A \strong{g} vector or name of the column in \code{.sutmats} that contains same. Default is "g".}

\item{r}{An \strong{r} vector or name of the column in \code{.sutmats} that contains same. Default is "r".}

\item{h}{An \strong{h} vector or name of the column in \code{.sutmats} that contains same. Default is "h".}

\item{Z}{The name for the \strong{Z} matrix on output. Default is "Z".
\strong{Z} is calculated by \code{U * g_hat_inv}.}

\item{K}{The name for the \strong{K} matrix on output. Default is "K".
\strong{K} is calculated by \code{U * f_hat_inv}.}

\item{C}{The name for the \strong{C} matrix on output. Default is "C".
\strong{C} is calculated by \code{transpose(V) * g_hat_inv}.}

\item{D}{The name for the \strong{D} matrix on output. Default is "D".
\strong{D} is calculated by \code{V * q_hat_inv}.}

\item{A}{The name for the \strong{A} matrix on output. Default is "A".
\strong{A} is calculated by \code{Z * D}.}

\item{O}{The name for the \strong{O} matrix on output. Default is "O".
\strong{O} is calculated by \code{r_hat_inv * R}.}

\item{Z_s}{The name for the \strong{Z_s} matrix on output. Default is "Z_s".
\strong{Z_s} is calculated by \code{transpose(V) * f_hat_inv}.}

\item{C_s}{The name for the \strong{C_s} matrix on output. Default is "C_s".
\strong{C_s} is calculated by \code{U * f_hat_inv}.}

\item{D_s}{The name for the \strong{D_s} matrix on output. Default is "D_s".
\strong{D_s} is calculated by \code{transpose(U) * q_hat_inv}.}

\item{D_feed_s}{The name for the \strong{D_feed_s} matrix on output. Default is "D_feed_s".
\strong{D_s} is calculated by \code{transpose(U_feed) * q_hat_inv}.}

\item{B}{The name for the \strong{B} matrix on output. Default is "B".
\strong{B} is calculated by \code{Z_s * D_s}.}

\item{O_s}{The name for the \strong{O_s} matrix on output. Default is "O_s".
\strong{O} is calculated by \code{q_hat_inv * Y}.}
}
\value{
A list or data frame containing
\strong{Z}, \strong{K}, \strong{C}, \strong{D}, \strong{A}, and \strong{O} matrices or
\strong{Z_s}, \strong{C_s}, \strong{D_s}, \strong{D_feed_s}, \strong{B}, and \strong{O_s} matrices,
depending on the value of \code{direction}.
}
\description{
These matrices define the IO structure of an energy conversion chain.
}
\details{
Input-output matrices can be calculated for either
an upstream swim (demand-sided as Leontief) or
a downstream swim (supply-sided as Ghosh).
The \code{direction} argument defines the direction.
Different IO matrices are calculated based on direction.
The default is "upstream", meaning that an upstream swim is desired.
Note that "upstream", "demand", and "Leontief" are synonyms.
"downstream", "supply", and "Ghosh" are synonyms.

For \code{direction = "upstream"},
\strong{Z}, \strong{K}, \strong{C}, \strong{D}, \strong{A}, and \strong{O}
matrices are calculated.
For \code{direction = "downstream"},
\strong{Z_s}, \strong{C_s}, \strong{D_s}, \strong{D_feed_s}, \strong{B}, and \strong{O_s}
matrices are calculated.
}
