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
#' @importFrom dplyr groups
#' @importFrom dplyr do
#' @importFrom matsindf collapse_to_matrices
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' S_units_from_tidy(UKEnergy2000tidy %>% group_by("Country", "Year", "Energy.type", "Last.stage"))
S_units_from_tidy <- function(.tidydf, Product = "Product", Unit = "Unit", S_units = "S_units"){
  # Establish names
  Unit <- as.name(Unit)
  Product <- as.name(Product)
  val <- ".val"
  rowtype <- ".rowtype"
  coltype <- ".coltype"

  verify_cols_missing(.tidydf, c(S_units, val, rowtype, coltype))

  select(.tidydf, !!!groups(.tidydf), !!Product, !!Unit) %>%
    do(unique(.data)) %>%
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
#' This function adds a column of matrix names to a tidy data frame
#' wherein each row of \code{.DF} is a single value in an energy conversion chain.
#' The default argument values assume that \code{.DF} uses IEA-style nomenclature
#' and terminology, although \code{.DF} does not necessarily need to contain IEA data.
#'
#' In a reasonable workflow, this function would be followed by a call to
#' \link{add_row_col_meta} and \link[matsindf]{collapse_to_matrices}.
#'
#' @param .DF a data frame with \code{ledger_side}, \code{energy}, \code{flow_aggregation_point},
#'        and \code{flow} columns.
#' @param ledger_side the name of the column in \code{.DF} that contains ledger side
#'        (a string). Default is "\code{Ledger.side}".
#' @param energy the name of the column in \code{.DF} that contains energy and exergy values
#'        (a string). Default is "\code{EX.ktoe}".
#' @param flow_aggregation_point the name of the column in \code{.DF} that contains flow aggregation point information.
#'        Default is "\code{Flow.aggregation.point}".
#' @param flow the name of the column in \code{.DF} that contains flow information.
#'        Default is "\code{Flow}".
#' @param supply_side the identifier for items on the supply side of the ledger (a string).
#'        Default is "\code{Supply}".
#' @param consumption_side the identifier for items on the consumption side
#'        of the ledger (a string). Default is "\code{Consumption}".
#' @param eiou the identifier for items that are energy industry own use.
#'        Default is "\code{"Energy industry own use"}.
#' @param neg_supply_in_fd identifiers for \code{flow} items that, when negative,
#'        are entries in the final demand (\code{Y}) matrix.
#' @param matname the name of the output column containing the name of the matrix
#'        to which a row's value belongs (a string). Default is "\code{matname}".
#' @param U the name for the use matrix (a string). Default is "\code{U}".
#' @param U_EIOU the name for the energy industry own use matrix. Default is "\code{U_EIOU}".
#' @param V the name for the make matrix (a string). Default is "\code{V}".
#' @param Y the name for the final demand matrix (a string). Default is "\code{Y}".
#'
#' @return \code{.DF} with an added column, \code{matname}.
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#'
#' @examples
#' add_matnames_iea(UKEnergy2000tidy)
add_matnames_iea <- function(.DF,
                             # Input columns
                             ledger_side = "Ledger.side",
                             energy = "EX.ktoe",
                             flow_aggregation_point = "Flow.aggregation.point",
                             flow = "Flow",
                             # Input identifiers for supply, consumption, and EIOU
                             supply_side = "Supply",
                             consumption_side = "Consumption",
                             eiou = "Energy industry own use",
                             neg_supply_in_fd = c("Exports",
                                                  "International aviation bunkers",
                                                  "International marine bunkers",
                                                  "Losses",
                                                  "Statistical differences",
                                                  "Stock changes"),
                             # Output column
                             matname = "matname",
                             # Ouput identifiers for use, EIOU, make, and final demand matrices
                             U = "U",
                             U_EIOU = "U_EIOU",
                             V = "V",
                             Y = "Y"){
  ledger_side <- as.name(ledger_side)
  energy <- as.name(energy)
  flow <- as.name(flow)
  flow_aggregation_point <- as.name(flow_aggregation_point)

  verify_cols_missing(.DF, matname)

  .DF %>%
    mutate(
      !!matname := case_when(
        # All Consumption items belong in the final demand (Y) matrix.
        !!ledger_side == consumption_side ~ Y,
        # All positive values on the Supply side of the ledger belong in the make (V) matrix.
        !!ledger_side == supply_side & !!energy > 0 ~ V,
        # Negative values on the supply side of the ledger with Flow == "Energy industry own use"
        # are put into the U_EIOU matrix
        !!ledger_side == supply_side & !!energy <= 0 & !!flow_aggregation_point == eiou ~ U_EIOU,
        # Negative values on the supply side that have Flow %in% supply_in_fd go in the final demand matrix
        # !!ledger_side == supply_side & !!energy <= 0 & !!flow %in% neg_supply_in_fd ~ Y,
        !!ledger_side == supply_side & !!energy <= 0 & starts_with_any_of(!!flow, neg_supply_in_fd) ~ Y,
        # All other negative values on the Supply side of the ledger belong in the use (U) matrix.
        !!ledger_side == supply_side & !!energy <= 0 ~ U,
        # Identify any places where our logic is faulty.
        TRUE ~ NA_character_
      )
    )
}

#' Add row, column, row type, and column type metadata
#'
#' @param .DF a data frame containing \code{matname_colname}.
#' @param matname the name of the column in \code{.DF} that contains names of matrices
#'        (a string).  Default is "\code{matname}".
#' @param U the name for use matrices (a string). Default is "\code{U}".
#' @param U_EIOU the name for the EIOU portino o fhte U matrix (a string). Default is "\code{U_EIOU}".
#' @param V the name for make matrices (a string). Default is "\code{V}".
#' @param Y the name for final demand matrices (a string). Default is "\code{Y}".
#' @param product the name of the column in \code{.DF} where Product names
#'        is found (a string). Default is "\code{Product}".
#' @param flow the name of the column in \code{.DF} where Flow names
#'        is found (a string). Default is "\code{Flow}".
#' @param industry_type the name that identifies production industries and
#'        and transformation processes (a string). Default is "\code{Industry}".
#' @param product_type the name that identifies energy carriers (a string).
#'        Default is "\code{Product}".
#' @param sector_type the name that identifies final demand sectors (a string).
#'        Default is "\code{Sector}".
#' @param rowname the name of the output column that contains row names for matrices
#'        (a string). Default is "\code{rowname}".
#' @param colname the name of the output column that contains column names for matrices
#'        (a string). Default is "\code{colname}".
#' @param rowtype the name of the output column that contains row types for matrices
#'        (a string). Default is "\code{rowtype}".
#' @param coltype the name of the output column that contains column types for matrices
#'        (a string). Default is "\code{coltype}".
#'
#' @return \code{.DF} with additional columns named
#'         \code{rowname}, \code{colname},
#'         \code{rowtype}, and \code{coltype}.
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' UKEnergy2000tidy %>%
#'   add_matnames_iea() %>%
#'   add_row_col_meta()
add_row_col_meta <- function(.DF,
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
  product <- as.name(product)
  flow <- as.name(flow)
  matname <- as.name(matname)
  rowname <- as.name(rowname)
  colname <- as.name(colname)
  rowtype <- as.name(rowtype)
  coltype <- as.name(coltype)

  verify_cols_missing(.DF, c(rowname, colname, rowtype, coltype))

  .DF %>%
    mutate(
      !!rowname := case_when(
        !!matname == U | !!matname == U_EIOU ~ !!product,
        !!matname == V ~ !!flow,
        !!matname == Y ~ !!product,
        TRUE ~ NA_character_
      ),
      !!colname := case_when(
        !!matname == U | !!matname == U_EIOU ~ !!flow,
        !!matname == V ~ !!product,
        !!matname == Y ~ !!flow,
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
