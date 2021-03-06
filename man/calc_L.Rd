% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/io.R
\name{calc_L}
\alias{calc_L}
\title{Calculates total requirements matrices (\code{L_pxp} and \code{L_ixp})}
\usage{
calc_L(.sutdata = NULL, D = "D", A = "A", L_pxp = "L_pxp", L_ixp = "L_ixp")
}
\arguments{
\item{.sutdata}{a data frame of supply-use table matrices with matrices arranged in columns.}

\item{D}{\code{D} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{D}".}

\item{A}{\code{A} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{A}".}

\item{L_pxp}{name for \code{L_pxp} matrix on output. Default is "\code{L_pxp}".
\code{L_pxp} is calculated by \code{(I - Z*D)^-1}.}

\item{L_ixp}{name for \code{L_ixp} matrix on output. Default is "\code{L_ixp}".
\code{L_ixp} is calculated by \code{D * L_pxp}.}
}
\value{
a list or data frame containing \code{L_pxp} and \code{L_ixp} matrices
}
\description{
Calculates total requirements matrices (\code{L_pxp} and \code{L_ixp})
}
