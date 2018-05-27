#
# This file contains utility functions for Recca
#

#' Verify that column names in a data frame are not already present
#'
#' In the \code{Recca} package, many functions add columns to an existing data frame.
#' If the incoming data frame already contains columns with the names of new columns to be added,
#' a name collision could occur, deleting the existing column of data.
#' This function provides a way to quickly check whether \code{newcols} are already present in
#' \code{.DF}.
#'
#' This function terminates execution if a column of \code{.DF} will be overwritten
#' by one of the \code{newcols}.
#'
#' @param .DF the data frame to which \code{newcols} are to be added
#' @param newcols a single string, a single name,
#'                a vector of strings representing the names of new columns to be added to \code{.DF}, or
#'                a vector of names of new columns to be added to \code{.DF}
#'
#' @return \code{NULL}. This function should be called for its side effect of checking the validity
#'         of the names of \code{newcols} to be added to \code{.DF}.
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = c(1,2), b = c(3,4))
#' verify_cols_missing(df, "d") # Silent. There will be no problem adding column "d".
#' newcols <- c("c", "d", "a", "b")
#' \dontrun{verify_cols_missing(df, newcols)}
verify_cols_missing <- function(.DF, newcols){
  if (!is.vector(newcols)) {
    newcols <- c(newcols)
  }
  df_names <- names(.DF)
  if (any(newcols %in% df_names)) {
    violators <- paste0("'", newcols[which(newcols %in% df_names)], "'", collapse = ", ")
    stop(paste0("column(s) ", violators, " is (are) already column names in data frame '",
                deparse(substitute(.DF)), "'"))
  }
  invisible(NULL)
}

#'
#' #' Add UVY, Commodity, Industry, row, and col columns to a tidy data frame
#' #' containing IEA-formatted data.
#' #'
#' #' This function is specific to our data and nomenclature.
#' #' It does not account for sign changes needed when
#' #' switching from the IEA representation to the SUT representation.
#' #' This function merely tags each row of the IEA data to indicate the
#' #' submatix to which it belongs.
#' #' This function assumes that columns with names
#' #' \code{Ledger.side}, \code{Flow}, \code{Flow.aggregation.point},
#' #' \code{EX.ktoe}, and \code{Product} exist.
#' #'
#' #' @param .ieadata a tidy data frame in IEA format containing columns for
#' #' Ledger.side, Flow, and Flow.aggregation.point
#' #' @param Ledger.side the name of the column in \code{.ieadata} containing ledger side information
#' #' @param Flow of the column in \code{.ieadata} containing flow information
#' #' @param Flow.aggregation.point the name of the column in \code{.ieadata} containing flow aggregation point information
#' #' @param Flow the name of the column in \code{.ieadata} containing flow information
#' #' @param E the name of the column in in \code{.ieadata} containing energy information
#' #' @param UVY the name of the column to be added to \code{.ieadata} containing matrix identifiers
#' #' @param U the name of the use matrix (default is "U")
#' #' @param U_EIOU the name of the use matrix (default is "U_EIOU")
#' #' @param V the name of the make matrix (default is "V")
#' #' @param Y the name of the final demand matrix (default is "Y")
#' #'
#' #' @return a modified version of \code{.data} including a UVY column that indicates
#' #' the matrix in which the data point should be placed.
#' #'
#' #' @export
#' #'
#' #' @importFrom dplyr case_when
#' #' @importFrom dplyr mutate
#' #' @importFrom magrittr %>%
#' #'
#' #' @examples
#' #' library(dplyr)
#' #' library(magrittr)
#' #' addUVY(filter(AllIEAData, Country == "HN"))
#' addUVY <- function(.ieadata,
#'                    # Input columns
#'                    Ledger.side = "Ledger.side",
#'                    Flow.aggregation.point = "Flow.aggregation.point",
#'                    Flow = "Flow",
#'                    E = "E.ktoe",
#'                    # Output columns
#'                    UVY = "UVY",
#'                    U = "U", U_EIOU = "U_EIOU", V = "V", Y = "Y"){
#'   .ieadata %>%
#'     mutate(
#'       UVY = case_when(
#'         # Identify MATRICES for each piece of data in an IEA-table style tidy data frame.
#'         # Note that we proceed from most-specific to most-general,
#'         # because the case_when function applies the rule of the first match.
#'
#'         # Start with the Consumption side of the ledger.
#'         .$Ledger.side == "Consumption" ~                                             "Y",
#'
#'         # Work on the Supply side of the ledger.
#'
#'         # Items that belong in the final demand matrix.
#'
#'         # Exports originated in the IEA data.
#'         # They belong in the primary portion of the final demand matrix.
#'         .$Ledger.side == "Supply" & startsWith(.$Flow, "Exports") ~                    "Y",
#'
#'         # Bunkers and Stock changes both originated in the IEA data.
#'         # They have similar characteristics, namely that
#'         # when these numbers are positive, they belong in the primary portion of the supply (V) matrix, but
#'         # when they are negative, they belong in the primary portion of the final demand matrix (Y) with
#'         # sign changed.
#'         .$Ledger.side == "Supply" &
#'           (startsWith(.$Flow, "International aviation bunkers") |
#'              startsWith(.$Flow, "International marine bunkers") |
#'              startsWith(.$Flow, "Stock changes")) &
#'           .$E.ktoe >= 0 ~                                                           "V",
#'         .$Ledger.side == "Supply" &
#'           (startsWith(.$Flow, "International aviation bunkers") |
#'              startsWith(.$Flow, "International marine bunkers") |
#'              startsWith(.$Flow, "Stock changes")) &
#'           .$E.ktoe < 0 ~                                                            "Y",
#'         # Statistical differences originated in the IEA data, and
#'         # they belong in the balancing portion of the Make (V) or final demand (Y) matrix,
#'         # depending on the sign (+ or -).
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Statistical differences") &
#'           .$E.ktoe >= 0 ~                                                           "V",
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Statistical differences") &
#'           .$E.ktoe < 0 ~                                                            "Y",
#'         # Losses originated in the IEA data, and
#'         # they belong in the balancing submatrix of the final demand (Y) matrix.
#'         .$Ledger.side == "Supply" &
#'           .$Flow == "Losses" ~                                                       "Y",
#'
#'         # Items that belong in the Use (U) and Make (V) transaction matrices.
#'
#'         # Production and Imports originated in the IEA data, and
#'         # they belong in the primary portion of the Make matrix (V_p)
#'         # Note that if we re-classify additional "Production" to an industry
#'         # (as we have done for coal),
#'         # we should add other production machines here (in addition to "Coal mines").
#'         .$Ledger.side == "Supply" &
#'           (startsWith(.$Flow, "Production") |
#'              startsWith(.$Flow, "Imports")) ~                                           "V",
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Coal mines") &
#'           .$Flow.aggregation.point != "Energy industry own use" ~                    "V",
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow, "Oil and gas extraction") &
#'           .$Flow.aggregation.point != "Energy industry own use" ~                    "V",
#'
#'         # Like Production, Transfers originated in the IEA data,
#'         # and they are part of primary-->final transformations.
#'         # Thus, Transfers belong in the _pf portion of the Use (U) and Make (V) matrices
#'         # according to their sign.
#'         .$Ledger.side == "Supply" & startsWith(.$Flow, "Transfers") & .$E.ktoe >= 0 ~         "V",
#'         .$Ledger.side == "Supply" & startsWith(.$Flow, "Transfers") & .$E.ktoe <  0 ~         "U",
#'
#'         # At this point, only Energy industry own use and Transformation processes
#'         # should remain on the Supply side of the ledger.
#'
#'         # Work on Energy industry own use.
#'
#'         # Energy industry own use originates from the IEA data,
#'         # where it typically indicates energy consumed by the energy industry
#'         # when converting primary energy to final energy by, for example,
#'         # Oil refineries.
#'         # Thus, we tag these entries with U_EIOU.
#'         .$Ledger.side == "Supply" &
#'           startsWith(.$Flow.aggregation.point, "Energy industry own use") &
#'           .$E.ktoe < 0 ~                                                            "U_EIOU",
#'
#'         # Work on Transformation processes.
#'
#'         # Transformation process entries with EX.ktoe >= 0 belong in the Make (V) matrix.
#'
#'         # If this entry is a Transformation process that makes energy (EX.ktoe >= 0) and
#'         # the product is a useful energy,
#'         # then this entry belongs in the final-->useful Make submatrix (V_fu).
#'         .$Ledger.side == "Supply" &
#'           .$Flow.aggregation.point == "Transformation processes" &
#'           .$E.ktoe >= 0 ~                                                                "V",
#'
#'         # If this entry is a Transformation process that uses energy (EX.ktoe < 0),
#'         # this entry belongs in the primary-->final Use submatrix (U_pf).
#'         .$Ledger.side == "Supply" &
#'           .$Flow.aggregation.point == "Transformation processes" &
#'           .$E.ktoe < 0 ~                                                                 "U",
#'
#'         # Anything that hasn't been specified above will get an "NA".
#'         # This is almost certainly an error.
#'         # So be sure to check for "NA" values in the UVY column.
#'         TRUE ~ "NA"
#'       )
#'     ) %>%
#'
#'     mutate(
#'       # Set TYPES of rows and columns for the matrices.
#'       # Products are the Commodities, and Flows are the Industries
#'       row = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ .$Product,
#'         startsWith(.$UVY, "V")                          ~ .$Flow,
#'         TRUE ~ "NA"
#'       ),
#'       col = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ .$Flow,
#'         startsWith(.$UVY, "V")                          ~ .$Product,
#'         TRUE ~ "NA"
#'       ),
#'       rowtype = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ "Commodity",
#'         startsWith(.$UVY, "V")                          ~ "Industry",
#'         TRUE ~ "NA"
#'       ),
#'       coltype = case_when(
#'         startsWith(.$UVY, "U") | startsWith(.$UVY, "Y") ~ "Industry",
#'         startsWith(.$UVY, "V")                          ~ "Commodity",
#'         TRUE ~ "NA"
#'       )
#'     )
#' }
