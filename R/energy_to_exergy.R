#' Extend an ECC in PSUT format from energy to exergy
#'
#' An energy conversion chain can be represented in energy or exergy quantifications of energy.
#' This function moves from an energy quantification to an exergy quantification,
#' given the matrices for the energy quantification and
#' phi (exergy-to-energy ratio) vectors.
#'
#' Internally, this function uses `matsindf::apply()`, so
#' the ECC matrices can be provided
#' as individual matrices,
#' in a named list, or
#' or in a data frame
#' (in which case the arguments should
#' given the string names of columns in the `.sutmats` data frame, the default).
#'
#' The vector `phi` is considered to be a store of values
#' to be applied to each type of energy carrier.
#' To determine which entry in the `phi` vector  is matched against which energy carrier,
#' `mat_piece` and `phi_piece` are consulted.
#' `mat_piece` and `phi_piece` can be any of
#' "all", "pref", "suff", "noun", or one of many prepositions given in `suffixes`
#'
#'
#' @param .sutmats An optional data frame of energy conversion chain matrices.
#' @param clean_up_df When `.sutmats` is a data frame, tells whether to `tidyr::pivot_longer()` the result,
#'                    remove no-longer-needed input column `phi`, and
#'                    fill the `energy_type` column with "X" for the exergy versions of the ECC matrices.
#'                    Default is `TRUE`.
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,phi Names of columns in `.sutmats` or single matrices. See `Recca::psut_cols`.
#' @param .exergy_suffix The string suffix to be appended to exergy versions of ECC matrices.
#' @param mat_piece The piece of matrix row and column names for `R`, `U`, `U_feed`, `U_EIOU`, `V`, and `Y` matrices
#'                  which are to be matched against names in the `phi` vector.
#'                  Default is "all", meaning that entire names are to be matched.
#' @param phi_piece The piece of names in the `phi` vector against which
#'                  row and column names for matrices `R`, `U`, `U_feed`, `U_EIOU`, `V`, and `Y` matrices
#'                  is to be matched.
#'                  Default is "all", meaning that entire names are to be matched.
#' @param notation The nomenclature for the row and column labels. Default is `RCLabels::bracket_notation`.
#' @param prepositions The prepositions to be used row and column notation.
#'                     Default is `RCLabels::prepositions_list`.
#' @param R_name,U_name,U_feed_name,U_eiou_name,r_eiou_name,V_name,Y_name,phi_name,energy_type,S_units Names of output matrices
#' @param energy,exergy See `Recca::energy_types`.
#'
#' @return A data frame or list of matrices that represents the exergy version of the ECC.
#'
#' @export
#'
#' @examples
#' sutmats <- UKEnergy2000mats %>%
#'   # Put in wide-by-matrix format.
#'   tidyr::spread(key = matrix.name, value = matrix) %>%
#'   # Eliminate services ECCs.
#'   dplyr::filter(LastStage %in% c("Final", "Useful")) %>%
#'   dplyr::mutate(
#'     phi = RCLabels::make_list(Recca::phi_vec, n = nrow(.), lenx = 1)
#'   )
#' extend_to_exergy(sutmats)
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
                             mat_piece = "all",
                             phi_piece = "all",
                             notation = RCLabels::bracket_notation,
                             prepositions = RCLabels::prepositions_list,
                             # Column names
                             R_name = Recca::psut_cols$R,
                             U_name = Recca::psut_cols$U,
                             U_feed_name = Recca::psut_cols$U_feed,
                             U_eiou_name = Recca::psut_cols$U_eiou,
                             r_eiou_name = Recca::psut_cols$r_eiou,
                             V_name = Recca::psut_cols$V,
                             Y_name = Recca::psut_cols$Y,
                             phi_name = Recca::psut_cols$phi,
                             energy_type = Recca::psut_cols$energy_type,
                             S_units = Recca::psut_cols$S_units,
                             energy = Recca::energy_types$e,
                             exergy = Recca::energy_types$x) {

  # Exergy names
  R_X_name <- paste0(R_name, .exergy_suffix)
  U_X_name <- paste0(U_name, .exergy_suffix)
  U_feed_X_name <- paste0(U_feed_name, .exergy_suffix)
  U_eiou_X_name <- paste0(U_eiou_name, .exergy_suffix)
  V_X_name <- paste0(V_name, .exergy_suffix)
  Y_X_name <- paste0(Y_name, .exergy_suffix)
  r_eiou_X_name <- paste0(r_eiou_name, .exergy_suffix)

  # Check that all columns in .sutmats contain "E" for EnergyType, if the column exists.
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
          "{S_units}" := NULL
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
    R_X_mat <- matsbyname::matrixproduct_byname(R_mat,
                                                matsbyname::vec_from_store_byname(a = R_mat,
                                                                                  v = matsbyname::transpose_byname(phi_vec),
                                                                                  a_piece = mat_piece, v_piece = phi_piece,
                                                                                  notation = notation, prepositions = prepositions,
                                                                                  margin = 2) %>%
                                                  matsbyname::hatize_byname(keep = "rownames"))

    # U_X = phi_hat * U_E
    U_X_mat <- matsbyname::matrixproduct_byname(matsbyname::vec_from_store_byname(a = U_mat,
                                                                                  v = phi_vec,
                                                                                  a_piece = mat_piece, v_piece = phi_piece,
                                                                                  notation = notation, prepositions = prepositions,
                                                                                  margin = 1) %>%
                                                  matsbyname::hatize_byname(keep = "rownames"),
                                                U_mat)

    # U_feed_X = phi_hat * U_feed_E
    U_feed_X_mat <- matsbyname::matrixproduct_byname(matsbyname::vec_from_store_byname(a = U_feed_mat,
                                                                                       v = phi_vec,
                                                                                       a_piece = mat_piece, v_piece = phi_piece,
                                                                                       notation = notation, prepositions = prepositions,
                                                                                       margin = 1) %>%
                                                       matsbyname::hatize_byname(keep = "rownames"),
                                                     U_feed_mat)

    # U_eiou_X = phi_hat * U_eiou_E
    U_eiou_X_mat <- matsbyname::matrixproduct_byname(matsbyname::vec_from_store_byname(a = U_eiou_mat,
                                                                                       v = phi_vec,
                                                                                       a_piece = mat_piece, v_piece = phi_piece,
                                                                                       notation = notation, prepositions = prepositions,
                                                                                       margin = 1) %>%
                                                       matsbyname::hatize_byname(keep = "rownames"),
                                                     U_eiou_mat)

    # V_X = V_E * phi_hat
    V_X_mat <- matsbyname::matrixproduct_byname(V_mat,
                                                matsbyname::vec_from_store_byname(a = V_mat,
                                                                                  v = matsbyname::transpose_byname(phi_vec),
                                                                                  a_piece = mat_piece, v_piece = phi_piece,
                                                                                  notation = notation, prepositions = prepositions,
                                                                                  margin = 2) %>%
                                                  matsbyname::hatize_byname(keep = "rownames"))


    # Y_X = phi_hat * Y_E
    Y_X_mat <- matsbyname::matrixproduct_byname(matsbyname::vec_from_store_byname(a = Y_mat,
                                                                                  v = phi_vec,
                                                                                  a_piece = mat_piece, v_piece = phi_piece,
                                                                                  notation = notation, prepositions = prepositions,
                                                                                  margin = 1) %>%
                                                  matsbyname::hatize_byname(keep = "rownames"),
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
      dplyr::select(dplyr::any_of(cols_to_keep)) %>%
      # Change the EnergyType column to Exergy
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


#' Extend the final-to-useful details matrices from energy to exergy
#'
#' The details matrices contain (in row and column names)
#' details about the move from the final energy stage to
#' the useful energy stage.
#' Four pieces of information are provided in row and column names:
#' - final energy product,
#' - final demand sector,
#' - useful energy product, and
#' - final-to-useful machine.
#'
#' Two details matrices are available:
#' - `Y_fu_details` and
#' - `U_EIOU_fu_details`.
#'
#' The two matrices correspond to the two ways in which final energy
#' is converted into useful energy:
#' in final demand (`Y_fu_details`) and
#' in energy industry own use (`U_EIOU_fu_details`).
#'
#'
#' The format for the row and column names for both details matrices is identical:
#' - row names
#'     - `RCLabels::arrow_notation`
#'     - prefix: final energy product
#'     - suffix: final demand sector
#'     - example: "Aviation gasoline -> Domestic aviation"
#' - column names
#'     - `RCLabels::from_notation`
#'     - noun: useful energy product
#'     - object of from: final-to-useful machine
#'     - example: "HPL \[from Electric pumps\]"
#'
#' The row and column types match
#' the row and column names.
#' - row types
#'     - `RCLabels::arrow_notation`
#'     - prefix: "Product"
#'     - suffix: "Industry"
#'     - specifically: "Product -> Industry"
#' - column types
#'     - `RCLabels::from_notation`
#'     - noun: "Product"
#'     - object of from: "Industry"
#'     - specifically: "Product \[from Industry\]"
#'
#' The energy stage of the entries in the details matrices are indicated
#' by the entry in the `EnergyType` column,
#' typically "Useful".
#'
#' If either of the energy details matrices are `NULL`,
#' the exergy matrix returned from this function is also `NULL`.
#'
#' @param .fu_details_mats A data frame containing details matrices.
#' @param Y_fu_details The name of the column of `fu_details_mats` containing details matrices or a details matrix.
#' @param U_eiou_fu_details The name of the column of `fu_details_mats` containing details matrices or a details matrix.
#' @param phi The name of the column of `fu_details_mats` containing phi vectors or a phi vector.
#' @param clean_up_df When `.fu_details_mats` is a data frame, tells whether to `tidyr::pivot_longer()` the result,
#'                    remove no-longer-needed input column `phi`, and
#'                    fill the `energy_type` column with "X" for the exergy versions of the ECC matrices.
#'                    Default is `TRUE`.
#' @param .exergy_suffix The string suffix to be appended to exergy versions of ECC matrices.
#' @param mat_piece The piece of details matrix column names
#'                  which are to be matched against names in the `phi` vector.
#'                  Default is "noun", meaning that the part before " \[from XYZ\]" will be matched.
#' @param phi_piece The piece of names in the `phi` vector against which
#'                  column names for the details matrices
#'                  are to be matched.
#'                  Default is "all", meaning that entire names are to be matched.
#' @param energy_type The name of the EnergyType column in `.fu_details_mats`.
#'                    Default is `Recca::psut_cols$energy_type`.
#' @param mat_col_notation The notation for the column labels of the details matrices.
#'                         Default is `RCLabels::from_notation`.
#' @param mat_colname_preposition The prepositions to be used for details matrix column notation.
#'                                Default is `RCLabels::prepositions_list[[which(RCLabels::prepositions_list == "from")]]`.
#' @param Y_fu_details_colname The name of the column in `.fu_details_mats` containing `Y_fu_details` matrices.
#' @param U_eiou_fu_details_colname The name of the column in `.fu_details_mats` containing `U_EIOU_fu_details` matrices.
#' @param phi_colname The name of the column in `.fu_details_mats` containing `phi` vectors.
#' @param energy,exergy String representing energy and exergy in the `energy_type` column.
#'                      Defaults are `Recca::energy_types$e` and `Recca::energy_types$x`, respectively.
#' @param industry_type,product_type Industry and product row and column types.
#'                                   Defaults are `IEATools::row_col_types$industry` and `IEATools::row_col_types$product`, respectively.
#'
#' @return A version of `.fu_details_mats` containing details matrices in exergy terms.
#'
#' @export
#'
#' @examples
#' details_mat <- Matrix::sparseMatrix(
#'   i = c(1, 2, 3),
#'   j = c(1, 3, 2),
#'   x = c(10, 20, 100),
#'   dimnames = list(c("Electricity -> Households",
#'                     "Electricity -> Industry",
#'                     "Natural gas -> Households"),
#'                   c("Light [from Electric lamps]",
#'                     "MTH.100.C [from Furnaces]",
#'                     "KE [from Fans]"))) |>
#'   matsbyname::setrowtype("Product -> Industry") |>
#'   matsbyname::setcoltype("Product [from Industry]")
#' phi_vec <- Matrix::sparseMatrix(
#'   i = c(1, 2, 3, 4),
#'   j = c(1, 1, 1, 1),
#'   x = c(1.0, 1-(25+273.15)/(100+273.15), 0.96, 1-(25+273.15)/(1000+273.15)),
#'   dimnames = list(c("KE", "MTH.100.C", "Light", "HTH.1000.C"),
#'                   "phi")) |>
#'   matsbyname::setrowtype("Product") |>
#'   matsbyname::setcoltype("phi")
#' extend_fu_details_to_exergy(Y_fu_details = details_mat,
#'                             U_eiou_fu_details = details_mat,
#'                             phi = phi_vec)
extend_fu_details_to_exergy <- function(.fu_details_mats = NULL,
                                         Y_fu_details = Recca::psut_cols$Y_fu_details,
                                         U_eiou_fu_details = Recca::psut_cols$U_eiou_fu_details,
                                         clean_up_df = TRUE,
                                         phi = Recca::psut_cols$phi,
                                         .exergy_suffix = "_exergy",
                                         mat_piece = "noun",
                                         phi_piece = "all",
                                         energy_type = Recca::psut_cols$energy_type,
                                         mat_col_notation = RCLabels::from_notation,
                                         mat_colname_preposition = RCLabels::prepositions_list[[which(RCLabels::prepositions_list == "from")]],
                                         # Column names
                                         Y_fu_details_colname = Recca::psut_cols$Y_fu_details,
                                         U_eiou_fu_details_colname = Recca::psut_cols$U_eiou_fu_details,
                                         phi_colname = Recca::psut_cols$phi,
                                         energy = Recca::energy_types$e,
                                         exergy = Recca::energy_types$x,
                                         industry_type = IEATools::row_col_types$industry,
                                         product_type = IEATools::row_col_types$product) {

  # Check if .fu_details_mats is a data frame and we don't have the required columns.
  # This is probably a country without Y and without U_eiou (there are some).
  # In this case, return NULL.
  if (is.data.frame(.fu_details_mats)) {
    if (! (Y_fu_details_colname %in% colnames(.fu_details_mats)) &
        ! (U_eiou_fu_details_colname %in% colnames(.fu_details_mats))) {
      return(NULL)
    }
  }

  Y_fu_details_X_name <- paste0(Y_fu_details_colname, .exergy_suffix)
  U_EIOU_fu_details_X_name <- paste0(U_eiou_fu_details_colname, .exergy_suffix)

  # Check that all columns in .fu_details_mats contain "E" for EnergyType, if the column exists.
  if (is.data.frame(.fu_details_mats)) {
    if (energy_type %in% names(.fu_details_mats)) {
      # Check that all energy types are "E".
      bad_rows <- .fu_details_mats %>%
        dplyr::filter(.data[[energy_type]] != energy) %>%
        dplyr::mutate(
          # Remove matrix columns to leave only metadata columns
          # in preparation for creating an error message.
          "{Y_fu_details_colname}" := NULL,
          "{U_eiou_fu_details_colname}" := NULL,
        )
      if (nrow(bad_rows) > 0) {
        err_msg <- paste0("In Recca::extend_fu_details_to_exergy(), non-energy rows were found: ",
                          matsindf::df_to_msg(bad_rows))
        stop(err_msg)
      }
    }
  }

  extend_func <- function(Y_fu_details_mat, U_eiou_fu_details_mat, phi_vec) {

    # When we get here, we should have single matrices
    # For each of these matrices, ensure that rowtypes and coltypes match.
    # The biggest problem is likely to be
    # Y_fu_details_mat and U_eiou_fu_details_mat.
    # They are likely to have
    # rowtype: Product -> Industry
    # coltype: Product [from Industry]
    # However, phi_vec is likely to have
    # rowtype: Product.
    # If phi_vec has rowtype of Product and both details mats have
    # rowtype of Product [from Industry],
    # change phi_vec to have
    # rowtype Product [from Industry]
    # so that the multiplication will work.

    rtp <- matsbyname::rowtype(phi_vec) # Probably "Product"
    ctY <- matsbyname::coltype(Y_fu_details_mat) # Probably "Product [from Industry]"
    ctU <- matsbyname::coltype(U_eiou_fu_details_mat) # Probably "Product [from Industry]"
    expected_coltype_YU <- RCLabels::paste_pref_suff(pref = product_type,
                                                     suff = industry_type,
                                                     notation = mat_col_notation)

    # For each of these multiplications, we first trim phi_vec to contain only
    # the energy products needed for converting to exergy.
    # Doing this avoids expanding the Y_fu_details and U_eiou_details matrices to all energy products,
    # thereby reducing computational complexity and memory consumption.

    # Y_fu_details * phi_hat
    if (is.null(Y_fu_details_mat)) {
      Y_fu_details_X_mat <- NULL
    } else {
      phi_vec_Y <- phi_vec
      if (!is.null(ctY) & !is.null(rtp)) {
        if (ctY == expected_coltype_YU & rtp == product_type) {
          # In this situation, we want to set rowtype
          # and coltype of phi_vec_Y to expected_coltype
          # so that multiplication can occur and so that the
          # resulting matrix product maintains original coltype
          # of "Product [from Industry]".
          phi_vec_Y <- matsbyname::setrowtype(phi_vec_Y, expected_coltype_YU)
        }
      }
      Y_fu_details_X_mat <- matsbyname::matrixproduct_byname(Y_fu_details_mat,
                                                             matsbyname::vec_from_store_byname(a = Y_fu_details_mat,
                                                                                               v = phi_vec_Y,
                                                                                               a_piece = mat_piece, v_piece = phi_piece,
                                                                                               notation = mat_col_notation,
                                                                                               prepositions = mat_colname_preposition,
                                                                                               margin = 2) |>
                                                               matsbyname::hatize_byname(keep = "rownames"))
    }

    # U_eiou_fu_details * phi_hat
    if (is.null(U_eiou_fu_details_mat)) {
      U_EIOU_fu_details_X_mat <- NULL
    } else {
      phi_vec_U <- phi_vec
      if (!is.null(ctU) & !is.null(rtp)) {
        if (ctU == expected_coltype_YU & rtp == product_type) {
          # In this situation, we want to set rowtype
          # and coltype of phi_vec_Y to expected_coltype
          # so that multiplication can occur and so that the
          # resulting matrix product maintains original coltype
          # of "Product [from Industry]".
          phi_vec_U <- matsbyname::setrowtype(phi_vec_U, expected_coltype_YU)
        }
      }
      U_EIOU_fu_details_X_mat <- matsbyname::matrixproduct_byname(U_eiou_fu_details_mat,
                                                                  matsbyname::vec_from_store_byname(a = U_eiou_fu_details_mat,
                                                                                                    v = phi_vec_U,
                                                                                                    a_piece = mat_piece, v_piece = phi_piece,
                                                                                                    notation = mat_col_notation,
                                                                                                    prepositions = mat_colname_preposition,
                                                                                                    margin = 2) |>
                                                                    matsbyname::hatize_byname(keep = "rownames"))
    }

    # Create the list of items to return
    list(Y_fu_details_X_mat,
         U_EIOU_fu_details_X_mat) |>
      magrittr::set_names(c(Y_fu_details_X_name,
                            U_EIOU_fu_details_X_name))
  }


  out <- matsindf::matsindf_apply(.fu_details_mats, FUN = extend_func,
                                  Y_fu_details_mat = Y_fu_details,
                                  U_eiou_fu_details_mat = U_eiou_fu_details,
                                  phi_vec = phi)

  if (is.data.frame(out) & clean_up_df) {
    cols_to_keep <- out |>
      matsindf::everything_except(Y_fu_details_colname,
                                  U_eiou_fu_details_colname,
                                  phi_colname,
                                  .symbols = FALSE)
    # We'll need to strip suffixes off column names.
    exergy_df <- out |>
      dplyr::select(dplyr::any_of(cols_to_keep)) |>
      # Change the EnergyType column to Exergy
      dplyr::mutate(
        "{energy_type}" := exergy
      ) |>
      # Strip sep_useful from end of any column names.
      # Hint obtained from https://stackoverflow.com/questions/45960269/removing-suffix-from-column-names-using-rename-all
      dplyr::rename_with(~ gsub(paste0(.exergy_suffix, "$"), "", .x))
    # Bind the energy and exergy data frames together.
    out <- dplyr::bind_rows(.fu_details_mats, exergy_df) |>
      dplyr::mutate(
        # Eliminate the phi column that is still present in the energy rows
        "{phi_colname}" := NULL
      )
  }

  return(out)
}
