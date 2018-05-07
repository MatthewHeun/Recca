% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/embodied.R
\name{calc_embodied_mats}
\alias{calc_embodied_mats}
\title{Calculate various embodied energy matrices}
\usage{
calc_embodied_mats(.iodata, Y_colname = "Y", y_colname = "y",
  q_colname = "q", L_ixp_colname = "L_ixp", g_colname = "g",
  W_colname = "W", U_EIOU_colname = "U_EIOU", keep_cols = NULL,
  G_colname = "G", H_colname = "H", E_colname = "E",
  M_p_colname = "M_p", M_s_colname = "M_s",
  F_footprint_p_colname = "F_footprint_p",
  F_effects_p_colname = "F_effects_p",
  F_footprint_s_colname = "F_footprint_s",
  F_effects_s_colname = "F_effects_s", e_colname = ".evectors",
  e_hat_colname = ".ehatmatrices", Q_colname = ".Q",
  Qpos_colname = ".Qposmatrices", Qposcolsums_colname = ".Qposcolsums")
}
\arguments{
\item{.iodata}{a data frame containing matrices that describe the Input-Output structure
(using the supply-use table format) of an Energy Conversion Chain.
\code{.iodata} will likely have been obtained from the \code{calc_io_mats} function.}

\item{Y_colname}{the name of the column in \code{.iodata} containing final demand (\strong{Y}) matrices.}

\item{y_colname}{the name of the column in \code{.iodata} containing final demand (\strong{y}) vectors.}

\item{q_colname}{the name of the column in \code{.iodata} containing final demand (\strong{q}) vectors.}

\item{L_ixp_colname}{the name of the column in \code{.iodata} containing Industry-by-Product
Leontief (\code{L_ixp}) matrices.}

\item{keep_cols}{a vector of names of columns of \code{.iodata} (if any) to return with the output}

\item{G_colname}{the name of the output column containing \code{G} matrices.
\code{G} is calculated by \code{L_ixp * y_hat}.}

\item{H_colname}{the name of the output column containing \code{H} matrices.
\code{H} is calculated by \code{L_ixp * Y}.}

\item{E_colname}{the name of the output column containing \code{E} matrices.
\code{E} is calculated by \code{W * g_hat_inv}.}

\item{F_footprint_p_colname}{the name of the output column containing \strong{F_footprint_p} matrices.
\strong{F}\code{_footprint_p} is calculated by \strong{M_p} (\strong{M_p}^T\strong{i})_hat_inv.}

\item{F_effects_p_colname}{the name of the output column containing \strong{F_effects_p} matrices.
\strong{F}\code{_effects_p} is calculated by \strong{M_p i}_hat_inv \strong{M_p}.}

\item{F_footprint_s_colname}{the name of the output column containing \strong{F_footprint_s} matrices.
\strong{F}\code{_footprint_s} is calculated by \strong{M_s} (\strong{M_s}^T\strong{i})_hat_inv.}

\item{F_effects_s_colname}{the name of the output column containing \strong{F_effects_s} matrices.
\strong{F}\code{_effects_p} is calculated by \strong{M_s i}_hat_inv \strong{M_s}.}

\item{e_colname}{the name of the column containing \code{e} vectors.
Each \code{e} vector is a row of the \code{E} matrix.
Thus, column \code{e_colname} contains lists of \code{e} vectors,
one \code{e} vector for each row in the corresponding \code{E} matrix.
\code{e_colname} is a column of intermediate results that is not included in the output.}

\item{e_hat_colname}{the name of the column containing \code{e_hat} matrices.
Each \code{e} vector is converted to an \code{e_hat} matrix.
Thus the \code{e_hat_colname} column contains lists of \code{e_hat} matrices.
\code{e_hat_colname} is a column of intermediate results that is not included in the output.}

\item{Q_colname}{the name of the output column containing lists of \code{Q} matrices.
\code{Q} is calculated by \code{e_hat * G},
but the e_hat column contains lists of matrices,
so the \code{Q} column will also contain lists of matrices.
In each list, there is one Q matrix for each Product in the Energy Conversion Chain.}

\item{Qpos_colname}{the name of the column containing \code{Qpos} matrices.
Embodied energy entries in \code{Q} are positive.
To simplify the process of finding embodied energy,
set all negative entries in each \code{Q} matrix to zero.}

\item{Qposcolsums_colname}{the name of the column containing column sums of \code{Qpos} matrices.}

\item{M_colname}{the name of the output column containing matrices of embodied energy.
These matrices give the energy of type shown in rows embodied in the Products shown in columns.
\code{M} is formed from column sums of positive entries in the various Qx matrices}
}
\value{
a data frame containing columns specified in \code{keep_cols},
\code{G_colname}, \code{H_colname}, \code{E_colname}, and \code{Q_colname}.
}
\description{
Calculate various embodied energy matrices
}