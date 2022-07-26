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
                       # Inputs
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


#' Calculate aggregate (total) efficiencies
#'
#' Calculates aggregate (total) primary-to-final demand (gross and net) efficiencies
#' across the energy conversion chain.
#'
#' `.aggregate_df` is probably formed by joining the results from
#' `primary_aggregates()` and `finaldemand_aggregates()`.
#' See examples.
#'
#' @param .aggregate_df A data frame or list containing columns
#'                      `aggregate_primary_colname`,
#'                      `net_aggregate_demand_colname`,
#'                      `gross_aggregate_demand_colname`,
#'                      probably formed by joining the results from
#' @param aggregate_primary_colname The name of the column in `p_aggregates` that contains primary energy or exergy aggregates.
#'                                  Default is `Recca::aggregate_cols$aggregate_primary`.
#' @param gross_aggregate_demand_colname The name of the column in `finaldemand_aggregates`
#'                                       that contains gross final demand aggregates.
#'                                       Default is `Recca::aggregate_cols$gross_aggregate_demand`.
#' @param net_aggregate_demand_colname The name of the column in `finaldemand_aggregates`
#'                                     that contains net final demand aggregates.
#'                                     Default is `Recca::aggregate_cols$net_aggregate_demand`.
#' @param eta_pfd_gross The name of the output column containing efficiencies
#'                      of converting primary energy into gross final demand energy.
#'                      Default is `Recca::efficiency_cols$eta_pfd_gross`.
#' @param eta_pfd_net The name of the output column containing efficiencies
#'                    of converting primary energy into net final demand energy.
#'                    Default is `Recca::efficiency_cols$eta_pfd_net`.
#'
#' @return A data frame of aggregate efficiencies.
#'
#' @export
#'
#' @examples
#' wide <- primary_total_aggregates_sut <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
#' # Define primary industries
#' p_industries <- c("Resources - Crude", "Resources - NG")
#' primary_total_aggregates <- wide %>%
#'   Recca::primary_aggregates(p_industries = p_industries, by = "Total") %>%
#'   # Don't need the matrices
#'   dplyr::select(IEATools::iea_cols$country,
#'                 IEATools::iea_cols$year,
#'                 IEATools::iea_cols$energy_type,
#'                 IEATools::iea_cols$last_stage,
#'                 Recca::aggregate_cols$aggregate_primary)
#' # Define final demand sectors
#' fd_sectors <- c("Residential", "Transport", "Oil fields")
#' finaldemand_total_aggregates <- wide %>%
#'   Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Total") %>%
#'   # Don't need the matrices
#'   dplyr::select(IEATools::iea_cols$country,
#'                 IEATools::iea_cols$year,
#'                 IEATools::iea_cols$energy_type,
#'                 IEATools::iea_cols$last_stage,
#'                 Recca::aggregate_cols$gross_aggregate_demand,
#'                 Recca::aggregate_cols$net_aggregate_demand)
#' dplyr::full_join(primary_total_aggregates,
#'                  finaldemand_total_aggregates,
#'                  by = c(IEATools::iea_cols$country,
#'                         IEATools::iea_cols$year,
#'                         IEATools::iea_cols$energy_type,
#'                         IEATools::iea_cols$last_stage)) %>%
#'   calc_eta_pfd()
calc_eta_pfd <- function(.aggregate_df = NULL,
                         # Inputs
                         aggregate_primary_colname = Recca::aggregate_cols$aggregate_primary,
                         gross_aggregate_demand_colname = Recca::aggregate_cols$gross_aggregate_demand,
                         net_aggregate_demand_colname = Recca::aggregate_cols$net_aggregate_demand,
                         # Outputs
                         eta_pfd_gross = Recca::efficiency_cols$eta_pfd_gross,
                         eta_pfd_net = Recca::efficiency_cols$eta_pfd_net) {
  eta_pfd_func <- function(primary, gross_fd, net_fd) {
    eta_pfd_gross_val <- gross_fd / primary
    eta_pfd_net_val <- net_fd / primary
    list(eta_pfd_gross_val, eta_pfd_net_val) %>%
      magrittr::set_names(c(eta_pfd_gross, eta_pfd_net))
  }
  matsindf::matsindf_apply(.aggregate_df,
                           FUN = eta_pfd_func,
                           primary = aggregate_primary_colname,
                           gross_fd = gross_aggregate_demand_colname,
                           net_fd = net_aggregate_demand_colname)
}
