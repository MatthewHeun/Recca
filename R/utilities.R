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