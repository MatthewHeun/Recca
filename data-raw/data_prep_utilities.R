#' Add a column of matrix names to tidy data frame
#'
#' @param .data a data frame with \code{ledger_side_colname} and \code{energy_colname}.
#' @param ledger_side_colname the name of the column in \code{.data} that contains ledger side
#'        (a string). Default is "\code{Ledger.side}".
#' @param energy_colname the name of the column in \code{.data} that contains energy values
#'        (a string). Default is "\code{E.ktoe}".
#' @param supply_side the identifier for items on the supply side of the ledger (a string).
#'        Default is "\code{Supply}".
#' @param consumption_side the identifier for items on the consumption side
#'        of the ledger (a string). Default is "\code{Consumption}".
#' @param matname_colname the name of the output column containing the name of the matrix
#'        in which this row belongs (a string). Default is "\code{UVY}".
#' @param U_name the name for the use matrix (a string). Default is "\code{U}".
#' @param V_name the name for the make matrix (a string). Default is "\code{V}".
#' @param Y_name the name for the final demand matrix (a string). Default is "\code{Y}".
#'
#' @return \code{.data} with an added column, \code{UVY_colname}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'
#' @examples
#' matsindf:::add_UKEnergy2000_matnames(UKEnergy2000)
add_UKEnergy2000_matnames <- function(.data,
                                      # Input columns
                                      ledger_side_colname = "Ledger.side",
                                      energy_colname = "E.ktoe",
                                      # Input identifiers for supply and consumption
                                      supply_side = "Supply",
                                      consumption_side = "Consumption",
                                      # Output column
                                      matname_colname = "matname",
                                      # Ouput identifiers for use, make, and final demand matrices
                                      U_name = "U",
                                      V_name = "V",
                                      Y_name = "Y"){
  .data %>% mutate(
    # Add a column that indicates the matrix in which this entry belongs.
    !!as.name(matname_colname) := case_when(
      # All negative values on the Supply side of the ledger belong in the use (U) matrix.
      (!!as.name(ledger_side_colname)) == supply_side & (!!as.name(energy_colname)) <= 0 ~ U_name,
      # All positive values on the Supply side of the ledger belong in the make (V) matrix.
      (!!as.name(ledger_side_colname)) == supply_side & (!!as.name(energy_colname)) > 0 ~ V_name,
      # All Consumption items belong in the final demand (Y) matrix.
      (!!as.name(ledger_side_colname)) == consumption_side ~ Y_name,
      # Identify any places where our logic is faulty.
      TRUE ~ NA_character_
    )
  )
}

#' Add row, column, row type, and column type metadata
#'
#' @param .data a data frame containing \code{matname_colname}.
#' @param matname_colname the name of the column in \code{.data} that contains names of matrices
#'        (a string).  Default is "\code{matname}".
#' @param U_name the name for use matrices (a string). Default is "\code{U}".
#' @param V_name the name for make matrices (a string). Default is "\code{V}".
#' @param Y_name the name for final demand matrices (a string). Default is "\code{Y}".
#' @param product_colname the name of the column in \code{.data} where Product names
#'        is found (a string). Default is "\code{Product}".
#' @param flow_colname the name of the column in \code{.data} where Flow information is found
#'        (a string).
#'        The Flow column usually contains the industries involved in this flow.
#'        Default is "\code{Flow}".
#' @param industry_type the name that identifies production industries and
#'        and transformation processes (a string). Default is "\code{Industry}".
#' @param product_type the name that identifies energy carriers (a string).
#'        Default is "\code{Product}".
#' @param sector_type the name that identifies final demand sectors (a string).
#'        Default is "\code{Sector}".
#' @param rowname_colname the name of the output column that contains row names for matrices
#'        (a string). Default is "\code{rowname}".
#' @param colname_colname the name of the output column that contains column names for matrices
#'        (a string). Default is "\code{colname}".
#' @param rowtype_colname the name of the output column that contains row types for matrices
#'        (a string). Default is "\code{rowtype}".
#' @param coltype_colname the name of the output column that contains column types for matrices
#'        (a string). Default is "\code{coltype}".
#'
#' @return \code{.data} with additional columns named
#'         \code{rowname_colname}, \code{colname_colname},
#'         \code{rowtype_colname}, and \code{coltype_colname}.
#'
#' @examples
#' library(magrittr)
#' UKEnergy2000 %>%
#'   matsindf:::add_UKEnergy2000_matnames(.) %>%
#'   matsindf:::add_UKEnergy2000_row_col_meta(.)
add_UKEnergy2000_row_col_meta <- function(.data,
                                          # Input column containing matrix names
                                          matname_colname = "matname",
                                          U_name = "U", V_name = "V", Y_name = "Y",
                                          product_colname = "Product", flow_colname = "Flow",
                                          industry_type = "Industry", product_type = "Product",
                                          sector_type = "Sector",
                                          # Output columns
                                          rowname_colname = "rowname", colname_colname = "colname",
                                          rowtype_colname = "rowtype", coltype_colname = "coltype"){
  .data %>%
    mutate(
      !!as.name(rowname_colname) := case_when(
        (!!as.name(matname_colname)) == U_name ~ !!as.name(product_colname),
        (!!as.name(matname_colname)) == V_name ~ !!as.name(flow_colname),
        (!!as.name(matname_colname)) == Y_name ~ !!as.name(product_colname),
        TRUE ~ NA_character_
      ),
      !!as.name(colname_colname) := case_when(
        (!!as.name(matname_colname)) == U_name ~ !!as.name(flow_colname),
        (!!as.name(matname_colname)) == V_name ~ !!as.name(product_colname),
        (!!as.name(matname_colname)) == Y_name ~ !!as.name(flow_colname),
        TRUE ~ NA_character_
      ),
      !!as.name(rowtype_colname) := case_when(
        (!!as.name(matname_colname)) == U_name ~ product_type,
        (!!as.name(matname_colname)) == V_name ~ industry_type,
        (!!as.name(matname_colname)) == Y_name ~ product_type,
        TRUE ~ NA_character_
      ),
      !!as.name(coltype_colname) := case_when(
        (!!as.name(matname_colname)) == U_name ~ industry_type,
        (!!as.name(matname_colname)) == V_name ~ product_type,
        (!!as.name(matname_colname)) == Y_name ~ sector_type,
        TRUE ~ NA_character_
      )
    )
}
