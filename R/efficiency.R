# This file contains functions that calculate direct efficiencies for an ECC.


#' Calculate industry efficiencies
#'
#' Calculates industry efficiencies for all energy conversion industries in the ECC.
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

    result_var <- "result"
    units_OK <- flows_unit_homogeneous(U = U_mat, V = V_mat, S_units = S_units_mat,
                                       flows_unit_homogeneous = result_var, keep_details = TRUE)[[result_var]]

    f_vec <- matsbyname::colsums_byname(U_mat) %>% matsbyname::transpose_byname()
    g_vec <- matsbyname::rowsums_byname(V_mat)
    eta_vec <- matsbyname::quotient_byname(g_vec, f_vec)
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

  matsindf_apply(.sutmats, FUN = eta_func, U_mat = U, V_mat = V, S_units_mat = S_units)
}
