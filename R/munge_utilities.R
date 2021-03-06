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
#' UKEnergy2000tidy %>%
#'   group_by("Country", "Year", "Energy.type", "Last.stage") %>%
#'   S_units_from_tidy()
S_units_from_tidy <- function(.tidydf, Product = "Product", Unit = "Unit", S_units = "S_units"){
  # Establish names
  Unit <- as.name(Unit)
  Product <- as.name(Product)
  val <- ".val"
  rowtype <- ".rowtype"
  coltype <- ".coltype"

  matsindf::verify_cols_missing(.tidydf, c(S_units, val, rowtype, coltype))

  dplyr::select(.tidydf, !!!dplyr::groups(.tidydf), !!Product, !!Unit) %>%
    dplyr::do(unique(.data)) %>%
    dplyr::mutate(
      !!as.name(val) := 1,
      !!as.name(S_units) := S_units,
      !!as.name(rowtype) := "Product",
      !!as.name(coltype) := "Unit"
    ) %>%
    matsindf::collapse_to_matrices(matnames = S_units, matvals = val,
                                   rownames = as.character(Product), colnames = as.character(Unit),
                                   rowtypes = rowtype, coltypes = coltype) %>%
    dplyr::rename(
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
#' This function respects groups when identifying entries in the resource matrix (\code{R}).
#' So be sure to group \code{.DF} before calling this function, if that is warranted.
#'
#' Internally, this function adds a temporary column to \code{.DF} called "\code{.R}".
#' An error will occur if \code{.DF} already has a column named "\code{.R}".
#'
#' @param .DF a data frame with \code{ledger_side}, \code{energy}, \code{flow_aggregation_point},
#'        and \code{flow} columns.
#' @param ledger_side the name of the column in \code{.DF} that contains ledger side
#'        (a string). Default is "\code{Ledger.side}".
#' @param energy the name of the column in \code{.DF} that contains energy and exergy values
#'        (a string). Default is "\code{E.dot}".
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
#' @param use_R tells whether to use a separate resources matrix (\code{R}). Default is "\code{FALSE}", for now.
#' @param matname the name of the output column containing the name of the matrix
#'        to which a row's value belongs (a string). Default is "\code{matname}".
#' @param U_excl_EIOU the name for the use matrix that excludes energy industry own use (a string). Default is "\code{U_excl_EIOU}".
#' @param U_EIOU the name for the energy industry own use matrix. Default is "\code{U_EIOU}".
#' @param R the name for the resource matrix (a string). Default is "\code{R}".
#' @param V the name for the make matrix (a string). Default is "\code{V}".
#' @param Y the name for the final demand matrix (a string). Default is "\code{Y}".
#'
#' @return \code{.DF} with an added column, \code{matname}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' UKEnergy2000tidy %>%
#'   group_by(Country, Year, Energy.type, Last.stage) %>%
#'   add_matnames_iea() %>%
#'   glimpse()
# add_matnames_iea <- function(.DF,
#                              # Input columns
#                              ledger_side = "Ledger.side",
#                              energy = "E.dot",
#                              flow_aggregation_point = "Flow.aggregation.point",
#                              flow = "Flow",
#                              # Input identifiers for supply, consumption, and EIOU
#                              supply_side = "Supply",
#                              consumption_side = "Consumption",
#                              eiou = "Energy industry own use",
#                              neg_supply_in_fd = c("Exports",
#                                                   "International aviation bunkers",
#                                                   "International marine bunkers",
#                                                   "Losses",
#                                                   "Statistical differences",
#                                                   "Stock changes"),
#                              # Mode switch
#                              use_R = FALSE,
#                              # Output column
#                              matname = "matname",
#                              # Ouput identifiers for
#                              # use matrix excluding EIOU (U_excl_EIOU),
#                              # use matrix energy industry own use items (U_EIOU),
#                              # make (V), and
#                              # final demand (Y)
#                              # matrices.
#                              U_excl_EIOU = "U_excl_EIOU", U_EIOU = "U_EIOU",
#                              R = "R", V = "V", Y = "Y"){
#   ledger_side <- as.name(ledger_side)
#   energy <- as.name(energy)
#   flow <- as.name(flow)
#   flow_aggregation_point <- as.name(flow_aggregation_point)
#
#   matsindf::verify_cols_missing(.DF, matname)
#
#   out <- .DF %>%
#     dplyr::mutate(
#       !!matname := dplyr::case_when(
#         # All Consumption items belong in the final demand (Y) matrix.
#         !!ledger_side == consumption_side ~ Y,
#         # All positive values on the Supply side of the ledger belong in the make (V) matrix.
#         !!ledger_side == supply_side & !!energy > 0 ~ V,
#         # Negative values on the supply side of the ledger with Flow == "Energy industry own use"
#         # are put into the U_EIOU matrix
#         !!ledger_side == supply_side & !!energy <= 0 & !!flow_aggregation_point == eiou ~ U_EIOU,
#         # Negative values on the supply side that have Flow %in% supply_in_fd go in the final demand matrix
#         # !!ledger_side == supply_side & !!energy <= 0 & !!flow %in% neg_supply_in_fd ~ Y,
#         !!ledger_side == supply_side & !!energy <= 0 & startsWith_any_of(!!flow, neg_supply_in_fd) ~ Y,
#         # All other negative values on the Supply side of the ledger belong in the use matrix
#         # that excludes EIOU (U_excl_EIOU).
#         !!ledger_side == supply_side & !!energy <= 0 ~ U_excl_EIOU,
#         # Identify any places where our logic is faulty.
#         TRUE ~ NA_character_
#       )
#     )
#   if (use_R) {
#     gvars <- dplyr::group_vars(out)
#     # Need to split R matrix entries from V matrix entries.
#     matname <- as.name(matname)
#     # Find all the rows that identify outputs of an ECC industry.
#     output_rows <- out %>%
#       dplyr::filter(!!matname == V)
#     # Find all the rows that identify inputs to an ECC industry.
#     input_rows <- out %>%
#       dplyr::filter(!!matname == Y | !!energy < 0) %>%
#       dplyr::mutate(
#         !!energy := abs(!!energy)
#       )
#     # Resource industries are those industries (Flows) that have outputs but no inputs.
#     industries_with_outputs <- output_rows %>%
#       dplyr::select(!!!gvars, !!flow)
#     industries_with_inputs <- input_rows %>%
#       dplyr::select(!!!gvars, !!flow) %>%
#       unique()
#     # The next line subtracts (by group!) all industries with inputs from the industries_with_outputs data frame,
#     # leaving only industries who have outputs but no inputs.
#     resource_rows <- dplyr::anti_join(industries_with_outputs, industries_with_inputs, by = c(gvars, as.character(flow))) %>%
#       dplyr::mutate(
#         # The rows in the resource_rows data frame belong in the resources matrix,
#         # so we give them the R matrix name.
#         .R = TRUE
#       )
#     # The following full_join puts a .R column in the out data frame.
#     # The .R column will have TRUE where matname needs to be changed from its current value to R
#     # The .R column will have NA where matname should not be changed.
#     .R_col <- as.name(".R")
#     matsindf::verify_cols_missing(out, .R_col)
#     out <- dplyr::full_join(out, resource_rows, by = c(gvars, as.character(flow))) %>%
#       dplyr::mutate(
#         !!matname := dplyr::case_when(
#           !!.R_col ~ R,
#           TRUE ~ matname
#         )
#       ) %>%
#       dplyr::select(-!!.R_col)
#   }
#   return(out)
# }

#' Add row, column, row type, and column type metadata
#'
#' @param .DF a data frame containing \code{matname_colname}.
#' @param matname the name of the column in \code{.DF} that contains names of matrices
#'        (a string).  Default is "\code{matname}".
#' @param U the name for use matrices (a string). Default is "\code{U}".
#' @param U_EIOU the name for energy industry own use matrices (a string). Default is "\code{U_EIOU}".
#' @param R the name for resource matrices (a string). Default is "\code{R}".
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
#'        Default is "\code{Industry}".
#' @param resource_type the name that identifies resource sectors (a string).
#'        Default is "\code{Industry}".
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
#' UKEnergy2000tidy %>%
#'   add_matnames_iea() %>%
#'   add_row_col_meta()
# add_row_col_meta <- function(.DF,
#                              # Name of the input column containing matrix names
#                              matname = "matname",
#                              # Column names for Product and Flow
#                              product = "Product", flow = "Flow",
#                              # Expected matrix names in the matname column
#                              U = "U", U_EIOU = "U_EIOU",
#                              R = "R", V = "V", Y = "Y",
#                              # Row and column Type identifiers
#                              industry_type = "Industry", product_type = "Product",
#                              sector_type = "Industry", resource_type = "Industry",
#                              # Output columns
#                              rowname = "rowname", colname = "colname",
#                              rowtype = "rowtype", coltype = "coltype"){
#   product <- as.name(product)
#   flow <- as.name(flow)
#   matname <- as.name(matname)
#   rowname <- as.name(rowname)
#   colname <- as.name(colname)
#   rowtype <- as.name(rowtype)
#   coltype <- as.name(coltype)
#
#   matsindf::verify_cols_missing(.DF, c(rowname, colname, rowtype, coltype))
#
#   .DF %>%
#     dplyr::mutate(
#       !!rowname := dplyr::case_when(
#         startsWith(!!matname, U) ~ !!product,
#         !!matname == R ~ !!flow,
#         !!matname == V ~ !!flow,
#         !!matname == Y ~ !!product,
#         TRUE ~ NA_character_
#       ),
#       !!colname := dplyr::case_when(
#         startsWith(!!matname, U) ~ !!flow,
#         !!matname == V ~ !!product,
#         !!matname == R ~ !!product,
#         !!matname == Y ~ !!flow,
#         TRUE ~ NA_character_
#       ),
#       !!rowtype := dplyr::case_when(
#         startsWith(!!matname, U) ~ product_type,
#         !!matname == R ~ resource_type,
#         !!matname == V ~ industry_type,
#         !!matname == Y ~ product_type,
#         TRUE ~ NA_character_
#       ),
#       !!coltype := dplyr::case_when(
#         startsWith(!!matname, U) ~ industry_type,
#         !!matname == R ~ product_type,
#         !!matname == V ~ product_type,
#         !!matname == Y ~ sector_type,
#         TRUE ~ NA_character_
#       )
#     )
# }
