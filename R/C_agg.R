#' Calculate allocation matrices for aggregate regions
#'
#' In the CL-PFU database, allocation matrices (**C**_Y and **C**_U_EIOU)
#' at the country level are used to calculate useful energy
#' in the final demand (**Y**) and energy industry own use (**U**_EIOU)
#' matrices for each individual country.
#' The countries are subsequently aggregated to regions.
#' Thus, regions do not have allocation matrices.
#' This function calculates allocation matrices
#' for any region from its
#' **Y**_f, **Y**_fu_details,
#' **U**_EIOU_f, **U**_EIOU_fu_details, and
#' **eta**_i matrices.
#' As such, this function can calculate
#' **C**_Y and **C**_EIOU matrices
#' that did not formerly exist
#' for aggregated regions.
#'
#' These calculations are the reverse of the
#' calculation that provided the useful version of the
#' final demand (**Y**_u) and
#' energy industry own use matrices (**U**_EIOU_u).
#' The original calculations were
#'
#' - Y_fu_details = Yf_vec_hat x C_Y x eta_i_hat
#' - U_EIOU_fu_details = U_EIOU_f_vec_hat x C_EIOU x eta_i_hat
#'
#' where "x" indicates matrix multiplication.
#'
#' So the calculations for the C matrices are
#'
#' - C_Y = Yf_vec_hat_inv x Yfu_details x eta_i_hat_inv
#' - C_EIOU = U_EIOU_f_vec_inv x U_EIOU_fu_details x eta_i_hat_inv
#'
#' @param .sutmats An optional data frame of mass or energy
#'                 (conserved quantities)
#'                 conversion chains in wide-by-matrix format.
#' @param Y_f The **Y**_f matrix or the name of a column in `.sutmats`
#'            that contains the same.
#'            This matrix is the final demand matrix when
#'            the last stage is final energy.
#'            Rows should contain names of final energy carriers, and
#'            columns should contain sectors.
#'            Default is "Yf".
#' @param Y_fu_details The **Y**_fu_details matrix or the name of a column in `.sutmats`
#'                     that contains the same.
#'                     This is a "details" matrix that has
#'                     final energy carriers and sectors in rows (with arrow notation) and
#'                     fu machines and useful energy carriers in columns (with from notation).
#'                     Default is "Yfudetails".
#' @param U_eiou_f The **U**_eiou_f matrix or name of the column in `.sutmats`
#'                 that contains the same.
#'                 This matrix is the energy industry own use matrix
#'                 when the last stage is final energy.
#'                 Rows should contain names of final energy carriers, and
#'                 columns should contain EIOU machines.
#'                 Default is "UEIOUf".
#' @param U_eiou_fu_details The **U**_eiou_fu_details matrix or the name of a column in `.sutmats`
#'                          that contains the same.
#'                          This is a "details" matrix that has
#'                          final energy carriers and energy industry own use
#'                          machines in rows (with arrow notation) and
#'                          final-to-useful machines and useful energy carriers
#'                          in columns (with from notation).
#'                          Default is "Ueioufudetails".
#' @param eta_i The **eta**_i vector or the name of a column in `.sutmats`
#'              that contains the same.
#'              This vector gives the energy efficiency of all final-to-useful machines.
#'              Rows should contain machine names and useful energy carriers
#'              (with arrow notation).
#'              Default is "Etai".
#' @param C_Y_colname The name of the output column that contains calculated
#'                    allocation matrices for final demand (**C**_Y).
#'                    Default is "`r Recca::alloc_cols$C_Y`".
#' @param C_eiou_colname The name of the output column that contains calculated
#'                         allocation matrices for energy industry own use
#'                         (**C**_EIOU).
#'                         Default is "`r Recca::alloc_cols$C_eiou`".
#'
#' @returns A data frame with pre-existing metadata and two new columns,
#'          `C_Y_colname` and `C_eiou_colname`.
#'
#' @export
#'
#' @examples
C_agg <- function(.sutmats = NULL,
                  Y_f = paste0(Recca::psut_cols$Y, "f"),
                  Y_fu_details = "Yfudetails",
                  U_eiou_f = "UEIOUf",
                  U_eiou_fu_details = "Ueioufudetails",
                  eta_i = "Etai",
                  # Output column names
                  C_Y_colname = Recca::alloc_cols$C_Y,
                  C_eiou_colname = Recca::alloc_cols$C_eiou) {


  C_func <- function(Y_f_mat, Y_fu_details_mat,
                     U_eiou_f_mat, U_eiou_fu_details_mat,
                     eta_i_vec) {

  }

  matsindf::matisindf_apply(.sutdata, FUN = C_func, Y_f_mat = Y_f, Y_fu_details_mat = Y_fu_details, U_eiou_f_mat = U_eiou_f, U_eiou_fu_details_mat = U_eiou_fu_details, eta_i_vec = eta_i)

}
