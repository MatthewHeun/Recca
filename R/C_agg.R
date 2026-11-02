#' Calculate allocation matrices for aggregate regions
#'
#' In the CL-PFU database, allocation matrices (**C_Y** and **C_EIOU**)
#' at the country level are used to calculate useful energy and exergy
#' for each country.
#' The countries are subsequently aggregated to regions.
#' Thus, regions do not have allocation matrices.
#' This function calculates allocation matrices
#' for any region from its
#' **U** and **Y** matrices.
#' As such, this function can calculate
#' **C_Y** and **C_EIOU**
#' that did not formerly exist
#' for aggregated regions.
#'
#' Internally, this function calls [calc_eta_i()] to
#' calculate industry efficiencies.
#' Further,
#'
#' @param .sutmats An optional data frame of mass or energy
#'                 (conserved quantities)
#'                 conversion chains in wide-by-matrix format.
#' @param U_feed The **U_feed** matrix or name of the column in `.sutmats`
#'               that contains the same.
#'               Default is "`r Recca::psut_cols$U_feed`".
#' @param U_eiou The **U_eiou** matrix or name of the column in `.sutmats`
#'               that contains the same.
#'               Default is "`r Recca::psut_cols$U_eiou`".
#' @param Y Final demand (**Y**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "`r Recca::psut_cols$Y`".
#' @param C_Y_colname
#' @param C_eiou_colname
#'
#' @returns A data frame with metadata and two columns,
#'          `C_Y_colname` and `C_eiou_colname`.
#'
#' @export
#'
#' @examples
C_agg <- function(.sutmats = NULL,
                  U_feed = Recca::psut_cols$U_feed,
                  U_eiou = Recca::psut_cols$U_eiou,
                  Y = Recca::psut_cols$Y,
                  separator = "___",
                  last_stage = Recca::psut_cols$last_stage,
                  final = Recca::all_stages$final,
                  useful = Recca::all_stages$useful,
                  U_feed_final = paste0(U_feed, separator, final),
                  U_feed_useful = paste0(U_feed, separator, useful),
                  U_eiou_final = paste0(U_eiou, separator, final),
                  U_eiou_useful = paste0(U_eiou, separator, useful),
                  Y_final = paste0(Y, separator, final),
                  Y_useful = paste0(Y, separator, useful),
                  # Output column names
                  C_Y_colname = Recca::alloc_cols$C_Y,
                  C_eiou_colname = Recca::alloc_cols$C_eiou) {

  if(!is.null(.sutmats)) {
    if (U_final %notin% colnames(.sutmats) & U_useful %notin% colnames(.sutmats) &
        Y_final %notin% colnames(.sutmats) & Y_useful %notin% colnames(.sutmats)) {
      # Pivot the data frame
      .sutmats <- .sutmats |>
        tidyr::pivot_wider(cols = c(U_feed))
    }
  }

  C_func <- function(U_feed_mat, U_eiou_mat, Y_mat)

}
