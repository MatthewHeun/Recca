# This file contains functions that calculate direct efficiencies for an ECC.


#' Calculate industry efficiencies
#'
#' Calculates industry efficiencies for all energy conversion industries in the ECC.
#' Calculations are performed as shown in Equation 11 in
#' Heun, Owen, and Brockway. 2018.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Applied Energy, vol 226, pp. 1134-1162.
#'
#' The efficiency for a given industry is calculated iff the units for inputs and outputs for that industry are unit-homogeneous.
#' If units for inputs and outputs are heterogeneous for an industry, \code{NA} is the result.
#'
#' Note that these efficiencies (\code{eta}) are different from
#' final demand sector and product efficiencies (\code{eta_s} and \code{eta_p}, respectively).
#' Both final demand sector and product efficiencies
#' (\code{eta_s} and \code{eta_p}) are based on embodied energy, whereas
#' industry efficiencies (\code{eta}) is based on direct inputs consumed and outputs produced
#' by the energy conversion industry.
#'
#' To calculate energy conversion final demand sector and product efficiencies, use the
#' \code{\link{calc_embodied_etas}} function.
#'
#' @param .sutmats a data frame containing columns for \code{U}, \code{V}, and \code{S_units} matrices
#' @param U a string for the name of a column of \code{U} matrices in \code{.sutmats}. (Default is "\code{U}".)
#' @param V a string for the name of a column of \code{V} matrices in \code{.sutmats}. (Default is "\code{V}".)
#' @param S_units a string for the name of a column of \code{S_units} matrices in \code{.sutmats}. (Default is "\code{S_units}".)
#' @param eta_i the name of the industry efficiency column in output. Default is "\code{eta_i}".
#'
#' @return \code{.sutmats} with an additional column "\code{eta_i}"
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   calc_eta_i()
calc_eta_i <- function(.sutmats,
                       # Input
                       U = "U", V = "V", S_units = "S_units",
                       # Outputs
                       eta_i = "eta_i"){
  eta_func <- function(U_mat, V_mat, S_units_mat){
    f_vec <- matsbyname::colsums_byname(U_mat) %>% matsbyname::transpose_byname()
    g_vec <- matsbyname::rowsums_byname(V_mat)
    eta_vec <- matsbyname::quotient_byname(g_vec, f_vec)
    # Set the name of the efficiency column to the value of the eta_i argument.
    dimnames(eta_vec) <- list(dimnames(eta_vec)[[1]], eta_i)

    # Ensure that places where there are inhomogeneous units are replaced by NA.
    result_var <- "result"
    units_OK <- flows_unit_homogeneous(U = U_mat, V = V_mat, S_units = S_units_mat,
                                       flows_unit_homogeneous = result_var, keep_details = TRUE)[[result_var]]
    # Make sure that units_OK and eta have same rows by completing the rows (industries) relative to one another
    completed <- matsbyname::complete_and_sort(units_OK, eta_vec, margin = 1)
    # The complete_and_sort function converts the TRUE/FALSE values in units_OK to 1/0.
    # Convert back to TRUE and FALSE.
    units_OK <- completed$a == 1
    eta_vec <- completed$b
    # Now set eta to NA if the industry is unit-heterogeneous in the first column of the respective vectors.
    eta_vec[which(!units_OK[ , 1]), 1] <- NA_real_
    # Return the eta value(s) with the correct name
    list(eta_vec) %>% magrittr::set_names(eta_i)
  }

  matsindf::matsindf_apply(.sutmats, FUN = eta_func, U_mat = U, V_mat = V, S_units_mat = S_units)
}


#' Calculate aggregate efficiencies
#'
#' Calculates aggregate primary-to-final demand (gross and net) efficiencies
#' across the energy conversion chain.
#' If final demand is at the final stage, calculates primary-to-final (gross and net) efficiencies.
#' If final demand is at the useful stage, calculates primary-to-useful (gross and net) efficiencies.
#' If final demand is at the services stage, calculates primary-to-services (gross and net) efficiencies.
#'
#' @param p_aggregates A data frame of primary aggregates, probably calculated by
#'                     `primary_aggregates()`.
#' @param finaldemand_aggregates A data frame of final demand aggregates, probably calculated by
#'                      `finaldemand_aggregates()`.
#' @param aggregate_primary_colname The name of the column in `p_aggregates` that contains primary energy or exergy aggregates.
#'                                  Default is `Recca::aggregate_cols$aggregate_primary`.
#' @param net_aggregate_demand_colname The name of the column in `finaldemand_aggregates`
#'                                     that contains net final demand aggregates.
#'                                     Default is `Recca::aggregate_cols$net_aggregate_demand`.
#' @param gross_aggregate_demand_colname The name of the column in `finaldemand_aggregates`
#'                                       that contains gross final demand aggregates.
#'                                       Default is `Recca::aggregate_cols$gross_aggregate_demand`.
#'
#' @return A data frame of aggregate efficiencies.
#'
#' @export
#'
#' @examples
calc_eta_pfu <- function(p_aggregates,
                         finaldemand_aggregates,
                         aggregate_primary_colname = Recca::aggregate_cols$aggregate_primary,
                         net_aggregate_demand_colname = Recca::aggregate_cols$net_aggregate_demand,
                         gross_aggregate_demand_colname = Recca::aggregate_cols$gross_aggregate_demand) {
  # Find metadata columns
  # Remove matrix columns
  # Remove p_industries and fd_sectors columns
  # Add Primary and Last.stage suffixes to column names
  # Join data frames
  # Calculate efficiencies
  # Return
}
