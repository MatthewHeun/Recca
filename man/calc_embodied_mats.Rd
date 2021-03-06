% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embodied.R
\name{calc_embodied_mats}
\alias{calc_embodied_mats}
\title{Calculate various embodied energy matrices}
\usage{
calc_embodied_mats(
  .iomats = NULL,
  Y = "Y",
  q = "q",
  L_ixp = "L_ixp",
  g = "g",
  W = "W",
  U_EIOU = "U_EIOU",
  G = "G",
  H = "H",
  E = "E",
  M_p = "M_p",
  M_s = "M_s",
  F_footprint_p = "F_footprint_p",
  F_effects_p = "F_effects_p",
  F_footprint_s = "F_footprint_s",
  F_effects_s = "F_effects_s"
)
}
\arguments{
\item{.iomats}{a data frame containing matrices that describe the Input-Output structure
(using the supply-use table format) of an Energy Conversion Chain.
\code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.}

\item{Y}{final demand (\code{Y}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{Y}".}

\item{q}{final demand (\code{q}) vector or name of the column in \code{.iodata} containing same. Default is "\code{q}".}

\item{L_ixp}{industry-by-product Leontief (\code{L_ixp}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{L_ixp}"}

\item{g}{name of the \code{g} vector on output. Default is "\code{g}".}

\item{W}{name of the \code{W} matrix on output. Default is "\code{W}".}

\item{U_EIOU}{name of the \code{U_EIOU} matrices on output. Default is "\code{U_EIOU}".}

\item{G}{name of the \code{G} matrix on output.
\code{G} is calculated by \code{L_ixp * y_hat}. Default is "\code{G}".}

\item{H}{name of the \code{H} matrix on output.
\code{H} is calculated by \code{L_ixp * Y}. Default is "\code{H}".}

\item{E}{name of \code{E} matrix on output.
\code{E} is calculated by \code{W * g_hat_inv}. Default is "\code{E}".}

\item{M_p}{name of the \code{M_p} matrix on output.
\code{M_p} is formed from column sums of positive entries in the various Qx matrices.
Default is "\code{M_p}".}

\item{M_s}{name of the \code{M_s} matrix on output.
\code{M_s} is constructed by \code{M_p * q_hat_inv * Y}. Default is "\code{M_s}".}

\item{F_footprint_p}{name of the \code{F_footprint_p} matrix on output.
\code{F_footprint_p} is calculated by \code{M_p * (M_p^T * i)_hat_inv}. Default is "\code{F_footprint_p}".}

\item{F_effects_p}{name of the \code{F_effects_p} matrix on output.
\code{F_effects_p} is calculated by \code{(M_p * i)_hat_inv * M_p}. Default is "\code{F_effects_p}".}

\item{F_footprint_s}{name of the \code{F_footprint_s} matrix on output.
\code{F_footprint_s} is calculated by \code{M_s * (M_s^T *i)_hat_inv}. Default is "\code{F_footprint_s}".}

\item{F_effects_s}{name of the \code{F_effects_s} matrix on output.
\code{F_effects_s} is calculated by \code{(M_s * i)_hat_inv * M_s}. Default is "\code{F_effects_s}".}
}
\value{
a list or data frame containing embodied energy matrices
}
\description{
Calculate various embodied energy matrices
}
