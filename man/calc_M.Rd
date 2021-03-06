% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embodied.R
\name{calc_M}
\alias{calc_M}
\title{Calculate embodied energy matrices}
\usage{
calc_M(
  .YqGHEdata = NULL,
  Y = "Y",
  q = "q",
  G = "G",
  E = "E",
  tol = 1e-04,
  M_p = "M_p",
  M_s = "M_s"
)
}
\arguments{
\item{.YqGHEdata}{a data frame containing columns with \code{q} vectors
and \code{Y}, \code{G}, \code{H}, and \code{E} matrices.
\code{.YqGEdata} will likely have been obtained from the \code{\link{calc_GH}} and \code{\link{calc_E}} functions.}

\item{Y}{final demand (\code{Y}) matrix or name of the column in \code{.YqHGEdata} containing same. Default is "\code{Y}".}

\item{q}{\code{q} column vector or name of the column in \code{.YqHGEdata} containing same. Default is "\code{q}".
\code{q} is calculated by \code{U*i + y}.}

\item{G}{\code{G} matrix or name of the column in \code{.YqHGEdata} containing same. Default is "\code{G}".
\code{G} is calculated by \code{L_ixp * y_hat}.}

\item{E}{\code{E} matrix or name of the column in \code{.YqHGEdata} containing same. Default is "\code{E}".
\code{E} is calculated by \code{W * g_hat_inv}.}

\item{tol}{the allowable energy balance error.}

\item{M_p}{the name for matrices of embodied energy in products on output. Default is "\code{M_p}".
These matrices contain embodied products in rows and embodying products in columns.}

\item{M_s}{the name for matrices of embodied energy consumed by final demand sectors. Default is "\code{M_s}".
These matrices contain embodied products in rows and consuming final demand sectors in columns.}
}
\value{
a list or data frame of embodied energy matrices
}
\description{
\code{Q} is calculated by \code{e_hat * G},
but the e_hat column contains lists of matrices,
so the \code{Q} column will also contain lists of matrices.
In each list, there is one Q matrix for each Product in the Energy Conversion Chain.
}
