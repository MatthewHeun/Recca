#' Extend an ECC in PSUT format from energy to exergy
#'
#' An energy conversion chain can be represented in energy or exergy quantifications of energy.
#' This function moves from an energy quantification to an exergy quantification,
#' given the matrices for the energy quantification and
#' phi (exergy-to-energy ratio) vectors.
#'
#' Internally, this function uses `matsindf::apply`, so
#' the ECC matrices can be provided
#' as individual matrices
#' or in a data frame
#' (in which case the arguments should
#' given the string names of columns in the `.sutmats` data frame).
#'
#'
#' @param .sutmats An optional data frame of energy conversino chain matrices.
#' @param clean_up_df When `.sutmats` is a data frame, tells whether to `tidyr::pivot_longer()` the result,
#'                    remove no-longer-needed input column `phi`, and
#'                    fill the `energy_type` column with "X" for the exergy versions of the ECC matrices.
#'                    Default is `TRUE`.
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,phi Names of columns in `.sutmats` or single matriecs. See `Recca::psut_cols`.
#' @param .exergy_suffix The string suffix to be appended to exergy versions of columns.
#' @param R_name,U_name,U_feed_name,U_eiou_name,r_eiou_name,V_name,Y_name Names of output matrices
#'
#' @return A data frame or list of matrices that represent the exergy version of the ECC.
#'
#' @export
#'
#' @examples
extend_to_exergy <- function(.sutmats = NULL,
                             clean_up_df = TRUE,
                             # Input matrices
                             R = Recca::psut_cols$R,
                             U = Recca::psut_cols$U,
                             U_feed = Recca::psut_cols$U_feed,
                             U_eiou = Recca::psut_cols$U_eiou,
                             r_eiou = Recca::psut_cols$r_eiou,
                             V = Recca::psut_cols$V,
                             Y = Recca::psut_cols$Y,
                             phi = Recca::psut_cols$phi,
                             .exergy_suffix = "_exergy",
                             # Column names
                             R_name = Recca::psut_cols$R,
                             U_name = Recca::psut_cols$U,
                             U_feed_name = Recca::psut_cols$U_feed,
                             U_eiou_name = Recca::psut_cols$U_eiou,
                             r_eiou_name = Recca::psut_cols$r_eiou,
                             V_name = Recca::psut_cols$V,
                             Y_name = Recca::psut_cols$Y,
                             phi_name = Recca::psut_cols$phi,
                             energy_type = IEATools::iea_cols$energy_type,
                             energy = Recca::energy_types$e,
                             exergy = Recca::energy_types$x,
                             s_units = Recca::psut_cols$s_units) {

  # Exergy names
  R_X_name <- paste0(R_name, .exergy_suffix)
  U_X_name <- paste0(U_name, .exergy_suffix)
  U_feed_X_name <- paste0(U_feed_name, .exergy_suffix)
  U_eiou_X_name <- paste0(U_eiou_name, .exergy_suffix)
  V_X_name <- paste0(V_name, .exergy_suffix)
  Y_X_name <- paste0(Y_name, .exergy_suffix)
  r_eiou_X_name <- paste0(r_eiou_name, .exergy_suffix)

  # Check that all columns in .sutmats contain "E" for Energy.type, if the column exists.
  if (is.data.frame(.sutmats)) {
    if (energy_type %in% names(.sutmats)) {
      # Check that all energy types are "E".
      bad_rows <- .sutmats %>%
        dplyr::filter(.data[[energy_type]] != energy) %>%
        dplyr::mutate(
          # Remove matrix columns to leave only metadata columns
          # in preparation for creating an error message.
          "{R_name}" := NULL,
          "{U_name}" := NULL,
          "{U_feed_name}" := NULL,
          "{U_eiou_name}" := NULL,
          "{r_eiou_name}" := NULL,
          "{V_name}" := NULL,
          "{Y_name}" := NULL,
          "{phi_name}" := NULL,
          "{s_units}" := NULL
        )
      if (nrow(bad_rows) > 0) {
        err_msg <- paste0("In Recca::extend_to_exergy(), non-energy rows were found: ",
                          matsindf::df_to_msg(bad_rows))
        stop(err_msg)
      }
    }
  }

  extend_func <- function(R_mat, U_mat, U_feed_mat, U_eiou_mat, V_mat, Y_mat, phi_vec) {
    # When we get here, we should have single matrices
    # For each of these multiplications, we first trim phi_vec to contain only
    # the energy products needed for converting to exergy.
    # Doing this avoids expanding the R, U, V, and Y matrices to all energy products,
    # thereby reducing computational complexity and memory consumption.

    # R_X = R_E * phi_hat
    R_X_mat <- matsbyname::matrixproduct_byname(R_mat, phi_vec %>%
                                                  matsbyname::transpose_byname() %>%
                                                  matsbyname::trim_rows_cols(mat = R_mat, margin = 2) %>%
                                                  matsbyname::hatize_byname())
    # U_X = phi_hat * U_E
    U_X_mat <- matsbyname::matrixproduct_byname(phi_vec %>%
                                                  matsbyname::trim_rows_cols(mat = U_mat, margin = 1) %>%
                                                  matsbyname::hatize_byname(),
                                                U_mat)

    # U_feed_X = phi_hat * U_feed_E
    U_feed_X_mat <- matsbyname::matrixproduct_byname(phi_vec %>%
                                                       matsbyname::trim_rows_cols(mat = U_feed_mat, margin = 1) %>%
                                                       matsbyname::hatize_byname(),
                                                     U_feed_mat)

    # U_eiou_X = phi_hat * U_eiou_E
    U_eiou_X_mat <- matsbyname::matrixproduct_byname(phi_vec %>%
                                                       matsbyname::trim_rows_cols(mat = U_eiou_mat, margin = 1) %>%
                                                       matsbyname::hatize_byname(),
                                                     U_eiou_mat)


    # V_X = V_E * phi_hat
    V_X_mat <- matsbyname::matrixproduct_byname(V_mat, phi_vec %>%
                                                  matsbyname::transpose_byname() %>%
                                                  matsbyname::trim_rows_cols(mat = V_mat, margin = 2) %>%
                                                  matsbyname::hatize_byname())


    # Y_X = phi_hat * Y_E
    Y_X_mat <- matsbyname::matrixproduct_byname(phi_vec %>%
                                                  matsbyname::trim_rows_cols(mat = Y_mat, margin = 1) %>%
                                                  matsbyname::hatize_byname(),
                                                Y_mat)

    # r_eiou_X = U_eiou_X / U_eiou_X
    r_eiou_X_mat <- matsbyname::quotient_byname(U_eiou_X_mat, U_X_mat) %>%
      matsbyname::replaceNaN_byname()

    # Create the list of items to return
    list(R_X_mat,
         U_X_mat,
         U_feed_X_mat,
         U_eiou_X_mat,
         V_X_mat,
         Y_X_mat,
         r_eiou_X_mat) %>%
      magrittr::set_names(c(R_X_name,
                            U_X_name,
                            U_feed_X_name,
                            U_eiou_X_name,
                            V_X_name,
                            Y_X_name,
                            r_eiou_X_name))
  }

  out <- matsindf::matsindf_apply(.sutmats, FUN = extend_func,
                                  R_mat = R,
                                  U_mat = U,
                                  U_feed_mat = U_feed,
                                  U_eiou_mat = U_eiou,
                                  V_mat = V,
                                  Y_mat = Y,
                                  phi_vec = phi)

  if (is.data.frame(out) & clean_up_df) {
    cols_to_keep <- out %>%
      matsindf::everything_except(R_name, U_name, U_feed_name, U_eiou_name,
                                  V_name, Y_name, r_eiou_name, phi_name,
                                  .symbols = FALSE)
    # We'll need to strip suffixes off column names.
    exergy_df <- out %>%
      dplyr::select(cols_to_keep) %>%
      # Change the Energy.type column to Useful
      dplyr::mutate(
        "{energy_type}" := exergy
      ) %>%
      # Strip sep_useful from end of any column names.
      # Hint obtained from https://stackoverflow.com/questions/45960269/removing-suffix-from-column-names-using-rename-all
      dplyr::rename_with(~ gsub(paste0(.exergy_suffix, "$"), "", .x))
    # Bind the energy and exergy data frames together.
    out <- dplyr::bind_rows(.sutmats, exergy_df) %>%
      dplyr::mutate(
        # Eliminate the phi column that is still present in the energy rows
        "{phi_name}" := NULL
      )
  }

  return(out)

}
