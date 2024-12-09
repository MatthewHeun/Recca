#' Extract an \code{S_units} matrix from a tidy data frame
#'
#' The \code{.tidydf} should be grouped as needed.
#'
#' @param .tidydf the data frame from which an \code{S_units} matrix is to be formed
#' @param Product the name of the \code{Product} column in \code{.tidydf}. Default is "\code{Product}".
#' @param Unit the name of the \code{Unit} column in \code{.tidydf}. Default is "\code{Unit}".
#' @param S_units the name of the \code{S_units} column to be added to \code{.tidydf}.
#'                Default is "\code{S_unit}".
#'
#' @return a data frame containing grouping variables and a new \code{S_unit} column
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(matsindf)
#' library(Recca)
#' UKEnergy2000tidy |>
#'   group_by(Country, Year, EnergyType, LastStage) |>
#'   S_units_from_tidy()
S_units_from_tidy <- function(.tidydf,
                              Product = IEATools::iea_cols$product,
                              Unit = IEATools::iea_cols$unit,
                              S_units = Recca::psut_cols$S_units){
  # Establish names
  Unit <- as.name(Unit)
  Product <- as.name(Product)
  val <- ".val"
  rowtype <- ".rowtype"
  coltype <- ".coltype"

  matsindf::verify_cols_missing(.tidydf, c(S_units, val, rowtype, coltype))

  .tidydf |>
    dplyr::select(!!!dplyr::groups(.tidydf), !!Product, !!Unit) |>
    dplyr::do(unique(.data)) |>
    dplyr::mutate(
      !!as.name(val) := 1,
      !!as.name(S_units) := S_units,
      !!as.name(rowtype) := "Product",
      !!as.name(coltype) := "Unit"
    ) |>
    matsindf::collapse_to_matrices(matnames = S_units, matvals = val,
                                   rownames = as.character(Product), colnames = as.character(Unit),
                                   rowtypes = rowtype, coltypes = coltype) |>
    dplyr::rename(
      !!as.name(S_units) := !!as.name(val)
    )
}
