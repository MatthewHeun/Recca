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
#'   tidyr::spread(key = "matrix.name", value = "matrix") %>%
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


#' Calculate final-to-useful efficiencies for final demand and EIOU when last stage is final
#'
#' Final-to-useful efficiencies for energy carriers and sectors
#' in final demand and energy industry own use
#' can be calculated from
#' allocations (`C_Y` and `C_eiou`),
#' machine efficiencies (`eta_i`), and
#' (for exergetic efficiencies) exergy-to-energy ratios (`phi`).
#' This function performs those calculations.
#' By default, the output contains matrices in the same
#' structure as **Y** and **U_EIOU** when last stage is final.
#'
#' The matrix formula for calculating energy efficiencies
#' is **eta_fu_E** `=` **C** `*` **eta_i**,
#' where **C** is one of **C_Y** or **C_EIOU**.
#' The matrix formula for calculating exergy efficiencies
#' from allocations and machine energy efficiencies is
#' **eta_fu_X** `=` (**phi_u_hat_inv** `*` (**C** `*` **eta_fu_hat**)) `*` **phi_u**.
#'
#' The **C_Y** matrix is assumed to have rows named
#' with prefixes of final energy carriers and
#' suffixes of the final demand sector
#' into which the final energy carrier flows.
#' The **C_EIOU** matrix is similar, except that
#' its rows are named with suffixes of the
#' energy industry sector into which the final energy carrier flows.
#' The columns of the **C_Y** and **C_EIOU** matrices
#' are named with prefixes of machine names and
#' suffixes of useful energy product made by the machine.
#' See examples.
#'
#' The **eta_i** vector of machine efficiencies
#' has rows named with
#' prefixes same as **C_Y** and **C_EIOU** columns.
#' The name of the **eta_i** column is not used.
#' See examples.
#'
#' The **phi** vector of exergy-to-energy ratios
#' has rows named with energy carriers
#' that correspond to the prefixes of
#' **C_Y** and **C_EIOU** rows and the suffixes of
#' **C_Y** and **C_EIOU** columns.
#' See examples.
#'
#' This function uses [matsbyname::vec_from_store_byname()]
#' to construct the `eta_i` and `phi` vectors before multiplying, thereby
#' eliminating unnecessary growth of the output matrices.
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
#'              Or a list of **eta_i** vectors.
#'              Default is `Recca::efficiency_cols$eta_i`.
#' @param phi The name of the column in `.c_mats_eta_phi_vecs` containing exergy-to-energy ratios.
#'            Or a single **phi** vector.
#'            Default is `Recca::psut_cols$phi`.
#' @param matricize A boolean that tells whether to return matrices of the same
#'                  structure as **Y** and **U_EIOU** when last stage is final.
#'                  Default is `TRUE`.
#'                  `FALSE` returns a column vector with rows same as
#'                  **C_Y** and **C_EIOU**.
#' @param energy_type,energy,exergy See `Recca::energy_types`.
#' @param eta_fu The base name of the output columns.
#'               Default is `Recca::efficiency_cols$eta_fu`.
#' @param eta_fu_Y_e The name of the energy efficiency output column
#'                   for energy flowing into final demand (**Y**).
#'                   Default is `paste0(eta_fu, "_Y_", energy)`.
#' @param eta_fu_Y_x The name of the exergy efficiency output column
#'                   for energy flowing into final demand (**Y**).
#'                   Default is `paste0(eta_fu, "_Y_", exergy)`.
#' @param eta_fu_eiou_e The name of the energy efficiency output column
#'                      for energy flowing into the energy industry (**U_EIOU**).
#'                      Default is `paste0(eta_fu, "_EIOU_", energy)`.
#' @param eta_fu_eiou_x The name of the exergy efficiency output column
#'                      for energy flowing into the energy industry (**U_EIOU**).
#'                      Default is `paste0(eta_fu, "_EIOU_", energy)`.
#' @param notation The notation for the row and column labels of the matrices and vectors.
#'                 Default is `RCLabels::arrow_notation`.
#'
#' @return A data frame or list containing final-to-useful efficiencies.
#'
#' @export
#'
#' @examples
#' C_Y <- matrix(c(0.7, 0.3, 0,   0,   0,
#'                 0,   0,   0.2, 0.5, 0.3), byrow = TRUE, nrow = 2, ncol = 5,
#'               dimnames = list(c("Electricity -> Non-ferrous metals",
#'                                 "PSB -> Residential"),
#'                               c("Electric arc furnaces -> HTH.600.C",
#'                                 "Electric lights -> L",
#'                                 "Wood stoves -> LTH.20.C",
#'                                 "Wood stoves -> LTH.50.C",
#'                                 "Wood stoves -> MTH.100.C")))
#' C_Y
#' eta_i <- matrix(c(0.9, 0.2, 0.4, 0.4, 0.3), nrow = 5, ncol = 1,
#'                   dimnames = list(c("Electric arc furnaces -> HTH.600.C",
#'                                     "Electric lights -> L",
#'                                     "Wood stoves -> LTH.20.C",
#'                                     "Wood stoves -> LTH.50.C",
#'                                     "Wood stoves -> MTH.100.C"),
#'                                   "eta_i"))
#' eta_i
#' phi <- matrix(c(1, 1.1, 1 - 298.15/(600+273.15), 0.95,
#'                 1 - (20 + 273.15)/298.15,
#'                 1 - 298.15/(50+273.15),
#'                 1 - 298.15/(100+273.15)),
#'               nrow = 7, ncol = 1,
#'               dimnames = list(c("Electricity", "PSB", "HTH.600.C", "L",
#'                                 "LTH.20.C", "LTH.50.C", "MTH.100.C"),
#'                               "phi"))
#' phi
#' res <- calc_eta_fu_Y_eiou(C_Y = C_Y, C_eiou = C_Y, eta_i = eta_i, phi = phi)
#' res$eta_fu_Y_E
#' res$eta_fu_EIOU_E # Same because C_Y and C_EIOU are same
#' res$eta_fu_Y_X
#' res$eta_fu_EIOU_X # Same because C_Y and C_EIOU are same
#' res2 <- calc_eta_fu_Y_eiou(C_Y = C_Y, C_eiou = C_Y, eta_i = eta_i, phi = phi,
#'                            matricize = FALSE)
#' res2$eta_fu_Y_E
#' res2$eta_fu_Y_X
calc_eta_fu_Y_eiou <- function(.c_mats_eta_phi_vecs = NULL,
                               C_Y = Recca::alloc_cols$C_Y,
                               C_eiou = Recca::alloc_cols$C_eiou,
                               eta_i = Recca::efficiency_cols$eta_i,
                               phi = Recca::psut_cols$phi,
                               matricize = TRUE,
                               energy_type = Recca::energy_types$energy_type,
                               eta_fu = Recca::efficiency_cols$eta_fu,
                               energy = Recca::energy_types$e,
                               exergy = Recca::energy_types$x,
                               eta_fu_Y_e = paste0(eta_fu, "_Y_", energy),
                               eta_fu_Y_x = paste0(eta_fu, "_Y_", exergy),
                               eta_fu_eiou_e = paste0(eta_fu, "_EIOU_", energy),
                               eta_fu_eiou_x = paste0(eta_fu, "_EIOU_", exergy),
                               notation = RCLabels::arrow_notation) {

  eta_func <- function(C_Y_mat, C_eiou_mat, eta_i_vec, phi_vec) {
    # At this point, all incoming matrices and vectors will be single matrices or vectors

    # Calculate some preliminary information, namely C_Y * eta_i_hat.
    # This is used for both energy and exergy calculations.

    # Trim eta_i_vec to include only those machines included in C_Y_mat or C_EIOU_mat
    eta_i_vec_trimmed_hat_Y_E <- matsbyname::trim_rows_cols(a = eta_i_vec, mat = matsbyname::transpose_byname(C_Y_mat),
                                                            margin = 1, notation = notation) |>
      matsbyname::hatize_byname(keep = "rownames")
    eta_i_vec_trimmed_hat_eiou_E <- matsbyname::trim_rows_cols(a = eta_i_vec, mat = matsbyname::transpose_byname(C_eiou_mat),
                                                               margin = 1, notation = notation) |>
      matsbyname::hatize_byname(keep = "rownames")

    # Post-multiply C_Y and C_EIOU by hatized eta vectors.
    CYetaihat <- matsbyname::matrixproduct_byname(C_Y_mat, eta_i_vec_trimmed_hat_Y_E)
    CEIOUetaihat <- matsbyname::matrixproduct_byname(C_eiou_mat, eta_i_vec_trimmed_hat_eiou_E)


    # eta_fu for final demand (Y) and energy (E)

    # Do the multiplication required to get eta_fu values
    eta_fu_Y_E_vec <- matsbyname::rowsums_byname(CYetaihat) |>
      matsbyname::setcolnames_byname(eta_fu_Y_e) |>
      matsbyname::setcoltype(eta_fu_Y_e)
    eta_fu_eiou_E_vec <- matsbyname::rowsums_byname(CEIOUetaihat) |>
      matsbyname::setcolnames_byname(eta_fu_eiou_e) |>
      matsbyname::setcoltype(eta_fu_eiou_e)

    # eta_fu for final demand (Y) and energy (X)

    # Build phi_hat_inv for the denominator with only rows that match prefixes of rows of the
    # CYetaihat and CEIOUetaihat matrices.
    # And change the coltype to enable multiplication.
    phi_hat_inv_Y_denom <- matsbyname::vec_from_store_byname(a = CYetaihat, v = phi_vec, notation = RCLabels::arrow_notation, a_piece = "pref") |>
      matsbyname::hatinv_byname(keep = "rownames") |>
      matsbyname::setcoltype(matsbyname::rowtype(CYetaihat))
    phi_hat_inv_EIOU_denom <- matsbyname::vec_from_store_byname(a = CEIOUetaihat, v = phi_vec, notation = RCLabels::arrow_notation, a_piece = "pref") |>
      matsbyname::hatinv_byname(keep = "rownames") |>
      matsbyname::setcoltype(matsbyname::rowtype(CEIOUetaihat))


    # Build phi vectors for the numerator by creating the vector from the suffixes of the columns of the
    # CYetaihat and CEIOUetaihat matrices.
    phi_Y_num <- matsbyname::vec_from_store_byname(a = CYetaihat, v = phi_vec, margin = 2, notation = RCLabels::arrow_notation, a_piece = "suff") |>
      matsbyname::setrowtype(matsbyname::coltype(CYetaihat))
    phi_EIOU_num <- matsbyname::vec_from_store_byname(a = CEIOUetaihat, v = phi_vec, margin = 2, notation = RCLabels::arrow_notation, a_piece = "suff") |>
      matsbyname::setrowtype(matsbyname::coltype(CEIOUetaihat))

    # Now do the exergy calculations
    eta_fu_Y_X_vec <- matsbyname::matrixproduct_byname(phi_hat_inv_Y_denom, CYetaihat) |>
      matsbyname::matrixproduct_byname(phi_Y_num) |>
      matsbyname::setcolnames_byname(eta_fu_Y_x) |>
      matsbyname::setrowtype(matsbyname::rowtype(CYetaihat)) |> matsbyname::setcoltype(eta_fu_Y_x)
    eta_fu_EIOU_X_vec <- matsbyname::matrixproduct_byname(phi_hat_inv_EIOU_denom, CYetaihat) |>
      matsbyname::matrixproduct_byname(phi_EIOU_num) |>
      matsbyname::setcolnames_byname(eta_fu_eiou_x) |>
      matsbyname::setrowtype(matsbyname::rowtype(CEIOUetaihat)) |> matsbyname::setcoltype(eta_fu_eiou_x)

    # Build an output list
    out <- list(eta_fu_Y_E_vec, eta_fu_eiou_E_vec, eta_fu_Y_X_vec, eta_fu_EIOU_X_vec)
    if (matricize) {
      out <- lapply(out, matsbyname::matricize_byname, notation = notation)
    }
    out |>
      magrittr::set_names(c(eta_fu_Y_e, eta_fu_eiou_e, eta_fu_Y_x, eta_fu_eiou_x))
  }
  matsindf::matsindf_apply(.c_mats_eta_phi_vecs, FUN = eta_func, C_Y_mat = C_Y, C_eiou_mat = C_eiou,
                           eta_i_vec = eta_i, phi_vec = phi)
}


