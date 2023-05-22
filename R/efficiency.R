#' Calculate industry efficiencies
#'
#' Calculates industry efficiencies for all energy conversion industries in the ECC.
#' Calculations are performed as shown in Equation 11 in
#' Heun, Owen, and Brockway. 2018.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Applied Energy, vol 226, pp. 1134-1162.
#'
#' The efficiency for a given industry is calculated iff the units for inputs and outputs for that industry are unit-homogeneous.
#' If units for inputs and outputs are heterogeneous for an industry, `NA` is the result.
#'
#' Note that these efficiencies (`eta`) are different from
#' final demand sector and product efficiencies (`eta_s` and `eta_p`, respectively).
#' Both final demand sector and product efficiencies
#' (`eta_s` and `eta_p`) are based on embodied energy, whereas
#' industry efficiencies (`eta`) is based on direct inputs consumed and outputs produced
#' by the energy conversion industry.
#'
#' To calculate energy conversion final demand sector and product efficiencies, use the
#' `calc_embodied_etas()` function.
#'
#' @param .sutmats A data frame containing columns for **U**, **V**, and **S_units** matrices.
#' @param U A string for the name of a column of **U** matrices in `.sutmats`. Default is `Recca::psut_cols$U`.
#' @param V A string for the name of a column of **V** matrices in `.sutmats`. Default is `Recca::psut_cols$V`.
#' @param S_units A string for the name of a column of **S_units** matrices in `.sutmats`. Default is `Recca::psut_cols$S_units`.)
#' @param eta_i The name of the industry efficiency column in output. Default is `Recca::psut_cols$S_units`.
#'
#' @return `.sutmats` with an additional column `eta_i`
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
                       U = Recca::psut_cols$U,
                       V = Recca::psut_cols$V,
                       S_units = Recca::psut_cols$S_units,
                       # Outputs
                       eta_i = Recca::efficiency_cols$eta_i){
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
#' `.aggregate_df` is probably formed by joining the results from
#' `primary_aggregates()` and `finaldemand_aggregates()`
#' or by calling `footprint_aggregates()`.
#' See examples.
#'
#' @param .aggregate_df A data frame or list containing columns
#'                      `aggregate_primary_colname`,
#'                      `net_aggregate_demand_colname`,
#'                      `gross_aggregate_demand_colname`,
#'                      probably the result of calling `footprint_aggregates()`.
#' @param efficiency_name_suffix The suffix for efficiency names.
#'                               Default is `Recca::efficiency_cols$efficiency_name_suffix`.
#' @param aggregate_primary_colname The name of the column in `p_aggregates` that contains primary energy or exergy aggregates.
#'                                  Default is `Recca::aggregate_cols$aggregate_primary`.
#' @param gross_aggregate_demand_colname The name of the column in `finaldemand_aggregates`
#'                                       that contains gross final demand aggregates.
#'                                       Default is `Recca::aggregate_cols$gross_aggregate_demand`.
#' @param net_aggregate_demand_colname The name of the column in `finaldemand_aggregates`
#'                                     that contains net final demand aggregates.
#'                                     Default is `Recca::aggregate_cols$net_aggregate_demand`.
#' @param energy_type The name of the energy type column. Default is `Recca::psut_cols$energy_type`.
#' @param last_stage The name of the last stage column. Default is `Recca::psut_cols$last_stage`.
#' @param eta_pfd_gross The name of the output column containing efficiencies
#'                      of converting primary energy into gross final demand energy.
#'                      Default is `Recca::efficiency_cols$eta_pfd_gross`.
#' @param eta_pfd_net The name of the output column containing efficiencies
#'                    of converting primary energy into net final demand energy.
#'                    Default is `Recca::efficiency_cols$eta_pfd_net`.
#' @param eta_pfd_gross_colname The name of the output column containing names of gross efficiency parameters.
#'                              Default is `paste0(eta_pfd_gross, efficiency_name_suffix)`.
#' @param eta_pfd_net_colname The name of the output column containing names of net efficiency parameters.
#'                            Default is `paste0(eta_pfd_net, efficiency_name_suffix)`.
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
                         efficiency_name_suffix = Recca::efficiency_cols$efficiency_name_suffix,
                         # Inputs
                         aggregate_primary_colname = Recca::aggregate_cols$aggregate_primary,
                         gross_aggregate_demand_colname = Recca::aggregate_cols$gross_aggregate_demand,
                         net_aggregate_demand_colname = Recca::aggregate_cols$net_aggregate_demand,
                         energy_type = Recca::psut_cols$energy_type,
                         last_stage = Recca::psut_cols$last_stage,
                         # Outputs
                         eta_pfd_gross = Recca::efficiency_cols$eta_pfd_gross,
                         eta_pfd_net = Recca::efficiency_cols$eta_pfd_net,
                         eta_pfd_gross_colname = paste0(eta_pfd_gross, efficiency_name_suffix),
                         eta_pfd_net_colname = paste0(eta_pfd_net, efficiency_name_suffix)) {

  eta_pfd_func <- function(primary_val, gross_fd_val, net_fd_val, energy_type_val, last_stage_val) {
    eta_pfd_gross_val <- gross_fd_val / primary_val
    eta_pfd_net_val <- net_fd_val / primary_val
    # eta_pfd_gross_name <- paste0("eta_", energy_type_val, "_p", substr(last_stage_val, 1, 1) %>% tolower(), "_gross")
    # eta_pfd_net_name <- gsub(pattern = "_gross", replacement = "_net", x = eta_pfd_gross_name)

    # c(eta_pfd_gross_val, eta_pfd_net_val, eta_pfd_gross_name, eta_pfd_net_name) %>%
    #   magrittr::set_names(c(eta_pfd_gross, eta_pfd_net, eta_pfd_gross_colname, eta_pfd_net_colname))
    c(eta_pfd_gross_val, eta_pfd_net_val) %>%
      magrittr::set_names(c(eta_pfd_gross, eta_pfd_net))
  }
  out <- matsindf::matsindf_apply(.aggregate_df,
                                  FUN = eta_pfd_func,
                                  primary_val = aggregate_primary_colname,
                                  gross_fd_val = gross_aggregate_demand_colname,
                                  net_fd_val = net_aggregate_demand_colname,
                                  energy_type_val = energy_type,
                                  last_stage_val = last_stage)
  if (is.data.frame(out)) {
    out <- out %>%
      dplyr::mutate(
        "{eta_pfd_gross}" := as.numeric(unlist(.data[[eta_pfd_gross]])),
        "{eta_pfd_net}" := as.numeric(unlist(.data[[eta_pfd_net]]))
      )
  }
  return(out)
}


#' Calculate final-to-useful efficiencies
#'
#' Final-to-useful efficiencies can be calculated from
#' allocations (`C_Y` and `C_eiou`),
#' efficiencies (`eta_i`), and
#' (for exergy) exergy-to-energy ratios (`phi`).
#' This function performs those calculations.
#'
#' The matrix formula for calculating energy efficiencies
#' is **eta_fu_E** `=` **C** `*` **eta_i**.
#' The matrix formula for calculating exergy efficiencies
#' from allocations and machine energy efficiencies is
#' **eta_fu_X** `=` (**phi_u_hat_inv** `*` (**C_Y** `*` **eta_fu_hat**)) `*` **phi_u**.
#'
#' @param .c_mats_eta_phi_vecs A data frame containing allocation matrices (`C_Y` and `C_eiou`),
#'                             vectors of machine efficiencies (`eta_i`), and
#'                             exergy-to-energy ratio vectors (`phi`).
#'                             Default is `NULL`, in which case individual matrices or vectors
#'                             can be passed to `C_Y`, `C_eiou`, `eta_i`, and `phi`.
#' @param C_Y The name of the column in `.c_mats_eta_phi_vecs` containing allocation
#'            matrices for final demand (the `Y` in `C_Y`).
#'            Or a single **C_Y** matrix.
#'            Or a list of **C_Y** matrices.
#'            Default is `Recca::alloc_cols$C_Y`.
#' @param C_eiou The name of the column in `.c_mats_eta_phi_vecs` containing allocation
#'               matrices for energy industry own use (the `eiou` in `C_eiou`).
#'               Or a single **C_EIOU** matrix.
#'               Or a list of **C_EIOU** matrices.
#'               Default is `Recca::alloc_cols$C_eiou`.
#' @param eta_i The name of the column in `.c_mats_eta_phi_vecs` containing machine efficiencies.
#'              Or a single **eta_i** vector.
#'              Default is `Recca::efficiency_cols$eta_i`.
#' @param phi The name of the column in `.c_mats_eta_phi_vecs` containing exergy-to-energy ratios.
#'            Or a single **phi** vector.
#'            Default is `Recca::psut_cols$phi`.
#' @param energy_type,energy,exergy See `Recca::energy_types`.
#' @param eta_fu The base name of the output columns.
#'               Default is `Recca::efficiency_cols$eta_fu`.
#' @param eta_fu_e The name of the energy efficiency output column.
#'                 Default is `paste0(eta_fu, "_", energy)`.
#' @param eta_fu_x The name of the exergy efficiency output column.
#'                 Default is `paste0(eta_fu, "_", exergy)`.
#'
#' @return A data frame or list containing final-to-useful efficiencies.
#'
#' @export
#'
#' @examples
calc_eta_fu <- function(.c_mats_eta_phi_vecs = NULL,
                        C_Y = Recca::alloc_cols$C_Y,
                        C_eiou = Recca::alloc_cols$C_eiou,
                        eta_i = Recca::efficiency_cols$eta_i,
                        phi = Recca::psut_cols$phi,
                        energy_type = Recca::energy_types$energy_type,
                        eta_fu = Recca::efficiency_cols$eta_fu,
                        energy = Recca::energy_types$e,
                        exergy = Recca::energy_types$x,
                        eta_fu_e = paste0(eta_fu, "_", energy),
                        eta_fu_x = paste0(eta_fu, "_", exergy)) {

  eta_func <- function(C_Y_mat, C_eiou_mat, eta_i_vec, phi_vec) {

  }

  matsindf::matsindf_apply(.c_mats_eta_phi_vecs, FUN = eta_func, C_Y_mat = C_Y, C_eiou_mat = C_eiou,
                           eta_i_vec = eta_i, phi_vec = phi)
}




