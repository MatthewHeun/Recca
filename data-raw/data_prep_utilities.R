#' Add a column of matrix names to tidy data frame
#'
#' @param .data a data frame with \code{ledger_side_colname} and \code{energy_colname}.
#' @param ledger_side the name of the column in \code{.DF} that contains ledger side
#'        (a string). Default is "\code{Ledger.side}".
#' @param energy_colname the name of the column in \code{.DF} that contains energy and exergy values
#'        (a string). Default is "\code{EX.ktoe}".
#' @param supply_side the identifier for items on the supply side of the ledger (a string).
#'        Default is "\code{Supply}".
#' @param consumption_side the identifier for items on the consumption side
#'        of the ledger (a string). Default is "\code{Consumption}".
#' @param matname the name of the output column containing the name of the matrix
#'        in which this row belongs (a string). Default is "\code{matname}".
#' @param U the name for the use matrix (a string). Default is "\code{U}".
#' @param V the name for the make matrix (a string). Default is "\code{V}".
#' @param Y the name for the final demand matrix (a string). Default is "\code{Y}".
#'
#' @return \code{.data} with an added column, \code{UVY_colname}.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'
#' @examples
#' UKEnergy2000raw %>%
#'   add_UKEnergy2000_matnames()
add_UKEnergy2000_matnames <- function(.DF,
                                      # Input columns
                                      ledger_side = "Ledger.side",
                                      energy = "EX.ktoe",
                                      # Input identifiers for supply and consumption
                                      supply_side = "Supply",
                                      consumption_side = "Consumption",
                                      # Output column
                                      matname = "matname",
                                      # Ouput identifiers for use, make, and final demand matrices
                                      U = "U",
                                      V = "V",
                                      Y = "Y"){
  matname <- as.name(matname)
  ledger_side <- as.name(ledger_side)
  energy <- as.name(energy)

  .DF %>% mutate(
    # Add a column that indicates the matrix in which this entry belongs.
    !!matname := case_when(
      # All negative values on the Supply side of the ledger belong in the use (U) matrix.
      !!ledger_side == supply_side & !!energy <= 0 ~ U,
      # All positive values on the Supply side of the ledger belong in the make (V) matrix.
      !!ledger_side == supply_side & (!!as.name(energy)) > 0 ~ V,
      # All Consumption items belong in the final demand (Y) matrix.
      !!ledger_side == consumption_side ~ Y,
      # Identify any places where our logic is faulty.
      TRUE ~ NA_character_
    )
  )
}

#' Add row, column, row type, and column type metadata
#'
#' @param .DF a data frame containing \code{matname_colname}.
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
#' UKEnergy2000raw %>%
#'   add_UKEnergy2000_matnames() %>%
#'   add_UKEnergy2000_row_col_meta()
add_UKEnergy2000_row_col_meta <- function(.DF,
                                          # Input column containing matrix names
                                          matname = "matname",
                                          U_name = "U", V_name = "V", Y_name = "Y",
                                          product = "Product", flow = "Flow",
                                          industry_type = "Industry", product_type = "Product",
                                          sector_type = "Industry",
                                          # Output columns
                                          rowname = "rowname", colname = "colname",
                                          rowtype = "rowtype", coltype = "coltype"){
  matname <- as.name(matname)
  Product <- as.name(product)
  Flow    <- as.name(flow)
  rowname <- as.name(rowname)
  colname <- as.name(colname)
  rowtype <- as.name(rowtype)
  coltype <- as.name(coltype)

  .DF %>%
    mutate(
      rowname = case_when(
        !!matname == U_name ~ !!Product,
        !!matname == V_name ~ !!Flow,
        !!matname == Y_name ~ !!Product,
        TRUE ~ NA_character_
      ),
      !!colname := case_when(
        !!matname == U_name ~ !!Flow,
        !!matname == V_name ~ !!Product,
        !!matname == Y_name ~ !!Flow,
        TRUE ~ NA_character_
      ),
      !!rowtype := case_when(
        !!matname == U_name ~ product_type,
        !!matname == V_name ~ industry_type,
        !!matname == Y_name ~ product_type,
        TRUE ~ NA_character_
      ),
      !!coltype := case_when(
        !!matname == U_name ~ industry_type,
        !!matname == V_name ~ product_type,
        !!matname == Y_name ~ sector_type,
        TRUE ~ NA_character_
      )
    )
}
