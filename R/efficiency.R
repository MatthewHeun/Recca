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
#' @param U A string for the name of a column of **U** matrices in `.sutmats`. (Default is "U".)
#' @param V A string for the name of a column of **V** matrices in `.sutmats`. (Default is "V".)
#' @param S_units A string for the name of a column of **S_units** matrices in `.sutmats`. (Default is "S_units".)
#' @param eta_i The name of the industry efficiency column in output. Default is "eta_i".
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
    eta_pfd_gross_name <- paste0("eta_", energy_type_val, "_p", substr(last_stage_val, 1, 1) %>% tolower(), "_gross")
    eta_pfd_net_name <- gsub(pattern = "_gross", replacement = "_net", x = eta_pfd_gross_name)

    c(eta_pfd_gross_val, eta_pfd_net_val, eta_pfd_gross_name, eta_pfd_net_name) %>%
      magrittr::set_names(c(eta_pfd_gross, eta_pfd_net, eta_pfd_gross_colname, eta_pfd_net_colname))
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


#' Pivot, clean, and complete an efficiency data frame
#'
#' Efficiency data frames (created by `calc_eta_pfd()`)
#' include (by default) columns of both
#' primary->final demand efficiencies and efficiency names.
#' The efficiency names are preferred to be columns to themselves.
#' The efficiency data frames can contain residual PSUT matrices,
#' which are cumbersome to carry around.
#' Furthermore, the efficiency data frames
#' contain only primary-to-final demand efficiencies,
#' not any intermediate efficiencies.
#' E.g., the data frame would contain
#' primary-to-final and primary-to-useful efficiencies
#' but not final-to-useful efficiencies.
#' This function cleans up these problems by
#' deleting columns of PSUT matrices (thereby cleaning the data frame),
#' pivoting to put all efficiencies in columns (thereby pivoting the data frame), and
#' calculating missing efficiencies (thereby completing the data frame).
#'
#' This function knows about the following stages
#' in energy conversion chains: Primary, Final, Useful, Services, and Well-being.
#'
#' The cleaning step eliminates all columns that contain exclusively matrices.
#' `matsindf::matrix_cols()` identifies the matrix columns.
#'
#' @param .eta_df A data frame of efficiencies, likely created by `calc_eta_pfd()`.
#' @param abbreviate_stage_names A boolean that tells whether stage names
#'                               are abbreviated to first letter.
#'                               E.g., "Primary" --> "p", "Final" --> "f", "Useful" --> "u", "Services" --> "s".
#'                               Default is `TRUE`.
#'
#' @return A cleaned version of `.eta_df`.
#'
#' @export
#'
#' @examples
pivot_clean_complete_eta_pfd <- function(.eta_df,
                                         abbreviate_stage_names = TRUE,
                                         efficiency_name_suffix = Recca::efficiency_cols$efficiency_name_suffix,
                                         primary = "Primary",
                                         final = "Final",
                                         useful = "Useful",
                                         services = "Services",
                                         wellbeing = "Wellbeing",
                                         # Columns in .eta_df
                                         # country = Recca::psut_cols$country,
                                         # year = Recca::psut_cols$year,
                                         # method = Recca::psut_cols$method,
                                         energy_type = Recca::psut_cols$energy_type,
                                         last_stage = Recca::psut_cols$last_stage,
                                         # product_sector = Recca::aggregate_cols$product_sector,
                                         eta_pfd_gross = Recca::efficiency_cols$eta_pfd_gross,
                                         eta_pfd_net = Recca::efficiency_cols$eta_pfd_net,
                                         eta_pfd_gross_colname = paste0(eta_pfd_gross, efficiency_name_suffix),
                                         eta_pfd_net_colname = paste0(eta_pfd_net, efficiency_name_suffix),
                                         gross_net = Recca::efficiency_cols$gross_net,
                                         gross = Recca::efficiency_cols$gross,
                                         net = Recca::efficiency_cols$net,
                                         eta_pf = Recca::efficiency_cols$eta_pf,
                                         eta_fu = Recca::efficiency_cols$eta_fu,
                                         eta_pu = Recca::efficiency_cols$eta_pu,
                                         eta_ps = Recca::efficiency_cols$eta_ps,
                                         eta_us = Recca::efficiency_cols$eta_us,
                                         eta_pw = Recca::efficiency_cols$eta_pw,
                                         eta_sw = Recca::efficiency_cols$eta_sw,
                                         aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                                         gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand,
                                         net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                                         # Internal columns
                                         .eta = ".eta",
                                         .eta_type = ".eta_type",
                                         .eta_name = ".eta_name",
                                         .eta_stages = ".eta_stages") {
  if (abbreviate_stage_names) {
    primary <- "p"
    final <- "f"
    useful <- "u"
    services <- "s"
    wellbeing <- "w"
  }

  # Clean
  matcols <- matsindf::matrix_cols(.eta_df)
  cleaned <- .eta_df %>%
    dplyr::select(-dplyr::any_of(matcols))

  # Pivot
  wider <- cleaned %>%
    tidyr::pivot_longer(cols = c(eta_pfd_gross, eta_pfd_net), names_to = .eta_type, values_to = .eta) %>%
    dplyr::mutate(
      "{gross_net}" := dplyr::case_when(
        .data[[.eta_type]] == eta_pfd_gross ~ gross,
        .data[[.eta_type]] == eta_pfd_net ~ net,
        TRUE ~ NA_character_
      ),
      "{.eta_stages}" := ifelse(rep(abbreviate_stage_names, times = nrow(.)),
                                # TRUE
                                paste0("p", tolower(substr(.data[[last_stage]], 1, 1))),
                                # FALSE
                                paste0("Primary->", .data[[last_stage]])),
      "{.eta_name}" := paste0("eta_", .data[[.eta_stages]]),
      # Delete several columns we no longer need.
      "{last_stage}" := NULL,
      "{aggregate_primary}" := NULL,
      "{gross_aggregate_demand}" := NULL,
      "{net_aggregate_demand}" := NULL,
      "{eta_pfd_gross_colname}" := NULL,
      "{eta_pfd_net_colname}" := NULL,
      "{.eta_type}" := NULL,
      "{.eta_stages}" := NULL,
    ) %>%
    tidyr::pivot_wider(names_from = .eta_name, values_from = .eta, values_fill = NA_real_)

  # Complete
  completed <- wider
  # Check for existence of columns before calculating.
  # Need to set the names of the efficiencies somewhere, probably as constants.
  if ((eta_pf %in% names(wider)) & (eta_pu %in% names(wider))) {
    # Calculate eta_fu
    completed <- completed %>%
      dplyr::mutate(
        "{eta_fu}" := .data[[eta_pu]] / .data[[eta_pf]]
      ) %>%
      dplyr::relocate(.data[[eta_fu]], .after = .data[[eta_pf]]) %>%
      dplyr::relocate(.data[[eta_pu]], .after = .data[[eta_fu]])
  }
  if (eta_ps %in% names(wider)) {
    # Calculate eta_us
    completed <- completed %>%
      dplyr::mutate(
        "{eta_us}" := .data[[eta_ps]] / .data[[eta_pu]]
      ) %>%
      dplyr::relocate(.data[[eta_us]], .after = .data[[eta_fu]]) %>%
      dplyr::relocate(.data[[eta_pu]], .after = .data[[eta_us]]) %>%
      dplyr::relocate(.data[[eta_ps]], .after = .data[[eta_pu]])
  }
  if (eta_pw %in% names(wider)) {
    # Calculate eta_sw
    completed <- completed %>%
      dplyr::mutate(
        "{eta_sw}" := .data[[eta_pw]] / .data[[eta_ps]]
      ) %>%
      dplyr::relocate(.data[[eta_sw]], .after = .data[[eta_us]]) %>%
      dplyr::relocate(.data[[eta_pu]], .after = .data[[eta_sw]]) %>%
      dplyr::relocate(.data[[eta_ps]], .after = .data[[eta_pu]]) %>%
      dplyr::relocate(.data[[eta_pw]], .after = .data[[eta_ps]])

  }

  return(completed)
}





