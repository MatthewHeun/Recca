#
# This file contains utility functions for Recca
#


#' Tell if any of a vector of strings starts with another string
#'
#' This function returns \code{TRUE} if any of the strings in \code{x}
#' starts with the string in \code{target} and \code{FALSE} otherwise.
#'
#' This function is vectorized. If \code{target} is a vector or list of strings,
#' the return value is the same length as \code{target} and
#' contains the result of applying the same test
#' (do any of \code{x} start with \code{target}?)
#' to each item in \code{target}.
#'
#' @param x a vector or list of strings
#' @param target a string (or a vector or list of strings)
#'
#' @return \code{TRUE} if any of \code{x} starts with \code{target}, \code{FALSE} otherwise.
#'         If \code{target} is a vector or list, the return value is the same length as \code{target}
#'         and contains the result of applying the test to each item in \code{target}.
#'
#' @importFrom Hmisc escapeRegex
#'
#' @export
#'
#' @examples
#' # TRUE, because one of the x string ("bd") starts with "b"
#' any_start_with(x = c("ad", "bd", "cd"), target = "b")
#' # TRUE, because two of the x strings starts with "Production"
#' any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Production")
#' # FALSE, because none of the x strings starts with "Offshore"
#' any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Offshore")
#' # TRUE FALSE, because the x strings start with "Production" but not "Offshore"
#' any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"),
#'                target = c("Production", "Offshore"))
any_start_with <- function(x, target){
  sapply(target, FUN = function(t){
    grepl(paste0("^", Hmisc::escapeRegex(t)), x) %>%
      any()
    }) %>%
    as.logical()
}


#' Tell if a string starts with any of a vector of strings
#'
#' This function returns \code{TRUE} if \code{x}
#' starts with any of the strings in \code{target} and \code{FALSE} otherwise.
#'
#' This function is vectorized. If \code{x} is a vector or list of strings,
#' the return value has the same length as \code{x} and contains the result
#' of applying the test (does \code{x} start with any of \code{target})
#' for each item in \code{x}.
#'
#' @param x a string (or vector or list of strings)
#' @param target a vector or list of strings
#'
#' @return \code{TRUE} if \code{x} starts with any of the strings in \code{target},
#'         \code{FALSE} otherwise.
#'         If \code{x} is a vector or list of strings, the return value is the same length as \code{x}
#'         and contains the result of applying the test to each item in \code{x}.
#'
#' @export
#'
#' @examples
#' starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix"))
#' starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c"))
#' starts_with_any_of(x = "prefix - suffix", target = "suffix")
#' starts_with_any_of(x = c("Production - Crude", "Production - NG",
#'                          "Exports - Oil", "Exports - Crude"),
#'                    target = c("Production", "Imports"))
starts_with_any_of <- function(x, target){
  sapply(x, FUN = function(one_x){
    grepl(paste(paste0("^", Hmisc::escapeRegex(target)), collapse = "|"), one_x)
  }) %>%
    set_names(NULL)
}


#' Resource industries
#'
#' Identifies resource industries.
#'
#' Resource industries are industries that make a product without using any products.
#' Resource industries are identified by interrogating
#' the use (\code{U}) and make (\code{V}) matrices.
#' Resource industries have all zeroes in their column of the use matrix (\code{U})
#' and at least one non-zero value in their row of the make (\code{V}) matrix.
#'
#' Argument and value descriptions are written assuming that \code{.sutdata} is a data frame.
#' Alternatively, \code{.sutdata} can be unspecified, and \code{U} and \code{V} can be matrices.
#' In that case, the return value is a list with a single item: \code{r_industries}
#' which contains a vector of names of resource industries for the \code{U} and \code{V} matrices.
#'
#' @param .sutdata a list or data frame containing use matrix(ces) and make matrix(ces)
#' @param U identifier for the use matrix (a string). Default is "\code{U}".
#' @param V identifier for the make matrix (a string). Default is "\code{V}".
#' @param r_industries name for the resource industries vector (a string). Default is "\code{r_industries}".
#'
#' @return \code{.sutdata} with an additional column (named with the value of the \code{p_industries} argument)
#'         containing the resource industries for each row
#'
#' @importFrom matsbyname sort_rows_cols
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' library(tidyr)
#' resource_industries(UKEnergy2000mats %>% spread(key = matrix.name, value = matrix))
resource_industries <- function(.sutdata = NULL, U = "U", V = "V", r_industries = "r_industries"){
  r_industries_func <- function(U, V){
    completed_cols_U <- complete_rows_cols(a = U, mat = transpose_byname(V), margin = 2) %>% sort_rows_cols()
    zero_cols_U_inds <- completed_cols_U %>%
      colsums_byname() %>%
      compare_byname("==", 0) %>%
      which()
    list(dimnames(completed_cols_U)[[2]][zero_cols_U_inds]) %>% magrittr::set_names(r_industries)
  }
  matsindf_apply(.sutdata, FUN = r_industries_func, U = U, V = V)
}


#' Extract resource matrices (\code{R}) from make plus resource (\code{V_plus_R}) and use (\code{U}) matrices
#'
#' Resource industries are industries that make a product without using any products.
#' Resource industries are identified by interrogating
#' the use (\code{U}) and make (\code{V_plus_R}) matrices.
#' Resource industries have all zeroes in their column of the use matrix (\code{U})
#' and at least one non-zero value in their row of the make (\code{V_plus_R}) matrix.
#'
#' A resource matrix (\code{R}) has industries in rows and products in columns.
#' The elements of of \code{R} indicate extraction of resources from the biosphere.
#' The industries of \code{R} are the reserves of the extracted products.
#'
#' This function uses the \code{\link{resource_industries}} function to
#' identify the resource industries in the \code{V_plus_R} matrix.
#' Thereafter, the function extracts the resource industries from the \code{V_plus_R} matrix
#' to form the \code{R} matrix.
#' Finally, the \code{R} matrix is subtracted from the \code{V_plus_R} matrix
#' and saved as the \code{V} matrix.
#' If there are no resource industries in the \code{V_plus_R} matrix,
#' a warning is emitted,
#' no \code{R} matrix is created, and
#' no changes are made to the \code{V_plus_R} matrix.
#'
#' @param .sutdata a list or data frame containing use matrix(ces) and make matrix(ces)
#' @param U_colname identifier for the use matrix (a string) \code{.sutdata}. Default is "\code{U}".
#' @param V_plus_R_colname identifier for the make matrix in \code{.sutdata} (a string). Default is "\code{V_plus_R}".
#' @param V_colname identifier for the make matrix (a string). Default is "\code{V}".
#' @param R_colname identifier for the resource matrix (a string). Default is "\code{R}".
#'
#' @return a version of the \code{V_plus_R} matrix without resource industries and an \code{R} matrix
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(magrittr)
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   # Rename the V matrix, because it includes the R matrix.
#'   rename(
#'     V_plus_R = V
#'   ) %>%
#'   extract_R()
extract_R <- function(.sutdata = NULL, U_colname = "U", V_plus_R_colname = "V_plus_R",
                      V_colname = "V", R_colname = "R"){
  extract_R_func <- function(U, V_plus_R){
    r_industry_names <- resource_industries(U = U, V = V_plus_R, r_industries = "r_inds") %>% unlist()
    if (length(r_industry_names) == 0) {
      warning("No R created in extract_R")
    } else {
      new_R <- V_plus_R %>% select_rows_byname(retain_pattern = make_pattern(r_industry_names, pattern_type = "exact"))
      new_V <- V_plus_R %>% select_rows_byname(remove_pattern = make_pattern(r_industry_names, pattern_type = "exact"))
    }
    list(new_V, new_R) %>% magrittr::set_names(c(V_colname, R_colname))
  }
  matsindf_apply(.sutdata, FUN = extract_R_func, U = U_colname, V_plus_R = V_plus_R_colname)
}


