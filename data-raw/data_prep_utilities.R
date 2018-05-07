
#' Extract an \code{S_units} matrix from a tidy data frame
#'
#' The \code{.tidydf} should be grouped as needed.
#'
#' @param .tidydf the data frame from which an \code{S_units} matrix is to be formed
#' @param Product the name of the \code{Product} column in \code{.tidydf}. Default is "\code{Product}".
#' @param Unit the name of the \code{Unit} column in \code{.tidydf}. Default is "\code{Unit}".
#' @param S_units the name of the \code{S_units} column to be added to \code{.tidydf}.
#'        Default is "\code{S_unit}".
#'
#' @return a data frame containing grouping variables and a new \code{S_unit} column
#'
#' @export
#'
#' @examples
#' S_units_from_tidy(UKEnergy2000tidy %>% group_by("Country", "Year", "Energy.type", "Last.stage"))
S_units_from_tidy <- function(.tidydf, Product = "Product", Unit = "Unit", S_units = "S_units"){
  # Establish names
  Unit <- as.name(Unit)
  Product <- as.name(Product)
  val <- ".val"
  rowtype <- ".rowtype"
  coltype <- ".coltype"

  verify_cols_missing(.tidydf, c(S_units, val, rowtype, coltype))

  .tidydf %>%
    select(!!!groups(.), !!Product, !!Unit) %>%
    do(unique(.)) %>%
    mutate(
      !!as.name(val) := 1,
      !!as.name(S_units) := S_units,
      !!as.name(rowtype) := "Product",
      !!as.name(coltype) := "Unit"
    ) %>%
    collapse_to_matrices(matnames = S_units, values = val,
                         rownames = as.character(Product), colnames = as.character(Unit),
                         rowtypes = rowtype, coltypes = coltype) %>%
    rename(
      !!as.name(S_units) := !!as.name(val)
    )
}


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
                                      flow.aggregation.point = "Flow.aggregation.point",
                                      # Input identifiers for supply, consumption, and EIOU
                                      supply_side = "Supply",
                                      consumption_side = "Consumption",
                                      eiou = "Energy industry own use",
                                      # Output column
                                      matname = "matname",
                                      # Ouput identifiers for use, EIOU, make, and final demand matrices
                                      U = "U",
                                      U_EIOU = "U_EIOU",
                                      V = "V",
                                      Y = "Y"){
  matname <- as.name(matname)
  ledger_side <- as.name(ledger_side)
  energy <- as.name(energy)
  flow.aggregation.point <- as.name(flow.aggregation.point)

  .DF %>% mutate(
    # Add a column that indicates the matrix in which this entry belongs.
    !!matname := case_when(
      # Negative values on the supply side of the ledger with Flow == "Energy industry own use"
      # are put into the U_EIOU matrix
      !!ledger_side == supply_side & !!energy <= 0 & !!flow.aggregation.point == eiou ~ U_EIOU,
      # All other negative values on the Supply side of the ledger belong in the use (U) matrix.
      !!ledger_side == supply_side & !!energy <= 0 ~ U,
      # All positive values on the Supply side of the ledger belong in the make (V) matrix.
      !!ledger_side == supply_side & !!energy > 0 ~ V,
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
#' @param U the name for use matrices (a string). Default is "\code{U}".
#' @param U_EIOU the name for the EIOU portino o fhte U matrix (a string). Default is "\code{U_EIOU}".
#' @param V the name for make matrices (a string). Default is "\code{V}".
#' @param Y the name for final demand matrices (a string). Default is "\code{Y}".
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
                                          # Name of the input column containing matrix names
                                          matname = "matname",
                                          # Column names for Product and Flow
                                          product = "Product", flow = "Flow",
                                          # Expected matrix names in the matname column
                                          U = "U", U_EIOU = "U_EIOU",
                                          V = "V", Y = "Y",
                                          # Row and column Type identifiers
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
        !!matname == U | !!matname == U_EIOU ~ !!Product,
        !!matname == V ~ !!Flow,
        !!matname == Y ~ !!Product,
        TRUE ~ NA_character_
      ),
      !!colname := case_when(
        !!matname == U | !!matname == U_EIOU ~ !!Flow,
        !!matname == V ~ !!Product,
        !!matname == Y ~ !!Flow,
        TRUE ~ NA_character_
      ),
      !!rowtype := case_when(
        !!matname == U | !!matname == U_EIOU ~ product_type,
        !!matname == V ~ industry_type,
        !!matname == Y ~ product_type,
        TRUE ~ NA_character_
      ),
      !!coltype := case_when(
        !!matname == U | !!matname == U_EIOU ~ industry_type,
        !!matname == V ~ product_type,
        !!matname == Y ~ sector_type,
        TRUE ~ NA_character_
      )
    )
}




