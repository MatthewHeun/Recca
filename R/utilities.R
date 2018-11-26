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
#' @param U use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V make (\code{V}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{V}".
#' @param r_industries name for the \code{r_industries} vector on output. Default is "\code{r_industries}".
#'
#' @return a list or data frame with \code{.sutdata} with an additional column (named with the value of the \code{p_industries} argument)
#'         containing the resource industries for each row
#'
#' @importFrom matsbyname sort_rows_cols
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix) %>%
#'   resource_industries()
resource_industries <- function(.sutdata = NULL, U = "U", V = "V", r_industries = "r_industries"){
  r_industries_func <- function(U_mat, V_mat){
    completed_cols_U <- complete_rows_cols(a = U_mat, mat = transpose_byname(V_mat), margin = 2) %>% sort_rows_cols()
    zero_cols_U_inds <- completed_cols_U %>%
      colsums_byname() %>%
      compare_byname("==", 0) %>%
      which()
    list(dimnames(completed_cols_U)[[2]][zero_cols_U_inds]) %>% magrittr::set_names(r_industries)
  }
  matsindf_apply(.sutdata, FUN = r_industries_func, U_mat = U, V_mat = V)
}


#' Separate resource (\code{R}) and make (\code{V}) matrices from make plus resource (\code{R_plus_V}) matrices
#'
#' Resource industries are industries that make a product without using any products.
#' Resource industries are identified by interrogating
#' the use (\code{U}) and make (\code{R_plus_V}) matrices.
#' Resource industries have all zeroes in their column of the use matrix (\code{U})
#' and at least one non-zero value in their row of the make (\code{R_plus_V}) matrix.
#'
#' A resource matrix (\code{R}) has industries in rows and products in columns.
#' The elements of of \code{R} indicate extraction of resources from the biosphere.
#' The industries of \code{R} are the reserves of the extracted products.
#'
#' This function uses the \code{\link{resource_industries}} function to
#' identify the resource industries in the \code{R_plus_V} matrix.
#' Thereafter, the function extracts the resource industries from the \code{R_plus_V} matrix
#' to form the \code{R} matrix.
#' Finally, the \code{R} matrix is subtracted from the \code{R_plus_V} matrix
#' and saved as the \code{V} matrix.
#' If there are no resource industries in the \code{R_plus_V} matrix,
#' a warning is emitted,
#' no \code{R} matrix is created, and
#' no changes are made to the \code{R_plus_V} matrix.
#'
#' @param .sutdata a list or data frame containing use matrix(ces) and make matrix(ces)
#' @param U a use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param R_plus_V an \code{R_plus_V} matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{R_plus_V}".
#' @param R name for resource (\code{R}) matrix on output. Default is "\code{R}".
#' @param V name for make (\code{V}) matrix on output. Default is "\code{V}".
#'
#' @return a list or data frame containing \code{R} and \code{V} matrices
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   # Rename the V matrix, because it includes the R matrix.
#'   rename(
#'     R_plus_V = V
#'   ) %>%
#'   separate_RV()
separate_RV <- function(.sutmats = NULL,
                        # Input names
                        U = "U", R_plus_V = "R_plus_V",
                        # Output names
                        R = "R", V = "V"){
  extract_R_func <- function(U_mat, R_plus_V_mat){
    r_industry_names <- resource_industries(U = U_mat, V = R_plus_V_mat, r_industries = "r_inds") %>% unlist()
    if (length(r_industry_names) == 0) {
      warning("No R created in separate_RV")
    } else {
      new_R_mat <- R_plus_V_mat %>% select_rows_byname(retain_pattern = make_pattern(r_industry_names, pattern_type = "exact"))
      new_V_mat <- R_plus_V_mat %>% select_rows_byname(remove_pattern = make_pattern(r_industry_names, pattern_type = "exact"))
    }
    list(new_R_mat, new_V_mat) %>% magrittr::set_names(c(R, V))
  }
  matsindf_apply(.sutmats, FUN = extract_R_func, U_mat = U, R_plus_V_mat = R_plus_V)
}

#' Combine resource (\code{R}) and make (\code{V}) matrices into a make plus resource (\code{R_plus_V}) matrix
#'
#' @param .sutmats a list or data frame containing use matrix(ces) and make matrix(ces)
#' @param R an \code{R} matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{R}".
#' @param V a make (\code{V}) matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{V}".
#' @param R_plus_V name for \code{R_plus_V} matrix on output. Default is "\code{R_plus_V}".
#'
#' @return a list or data frame containing \code{R_plus_V}
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   # Delete next line when switch to using R everywhere
#'   rename(R_plus_V = V) %>% separate_RV() %>% select(-R_plus_V) %>%
#'   combine_RV()
combine_RV <- function(.sutmats = NULL,
                       # Input names
                       R = "R", V = "V",
                       # Output name
                       R_plus_V = "R_plus_V"){
  combine_RV_func <- function(R_mat, V_mat){
    R_plus_V_mat <- sum_byname(R_mat, V_mat)
    list(R_plus_V_mat) %>% magrittr::set_names(c(R_plus_V))
  }
  matsindf_apply(.sutmats, FUN = combine_RV_func, R_mat = R, V_mat = V)
}


#' Tell whether ECC products are unit-homogenous
#'
#' Returns \code{TRUE} if products are unit-homogeneous
#' according to the \code{S_units} matrix and
#' \code{FALSE} otherwise.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param S_units an \code{S_units} matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param keep_details if \code{TRUE}, per-product results are returned;
#'        if \code{FALSE}, per-ECC results are returned.
#' @param products_unit_homogeneous name for the boolean that tells whether products in \code{S_units} are unit-homogeneous on output.
#'        Default is "\code{.products_unit_homogeneous}".
#'
#' @return a list or data frame containing \code{TRUE} if products in \code{S_units} are unit-homogeneous, \code{FALSE} otherwise.
#'
#' importFrom magrittr extract2
#'
#' @export
#'
#' @examples
#' library(magrittr)
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   products_unit_homogeneous() %>%
#'   extract2(".products_unit_homogeneous")
products_unit_homogeneous <- function(.sutmats = NULL,
                                      # Input names
                                      S_units = "S_units",
                                      keep_details = FALSE,
                                      # Output names
                                      products_unit_homogeneous = ".products_unit_homogeneous"){
  products_unit_homogeneous_func <- function(S_units_mat){
    num_ones <- count_vals_inrows_byname(S_units_mat, "==", 1)
    out <- num_ones == 1
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(products_unit_homogeneous)
  }
  matsindf_apply(.sutmats, FUN = products_unit_homogeneous_func, S_units_mat = S_units)
}


#' Tell whether each industry's inputs are unit-homogeneous
#'
#' Returns \code{TRUE} if each industry's inputs are unit-homogeneous.
#'
#' The \code{U_bar} matrix is queried for the number of non-zero entries in each column.
#' If the number of non-zero entries in each column is exactly 1,
#' industry inputs are unit-homogeneous.
#' Note that \code{U_bar = \link[matsbyname]{matrixproduct_byname}(\link[matsbyname]{transpose_byname}(S_units), U)}.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U a use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param S_units an \code{S_units} matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param keep_details if \code{TRUE}, per-product results are returned;
#'        if \code{FALSE}, per-ECC results are returned. Default is \code{FALSE}.
#' @param ins_unit_homogeneous name of the output boolean that
#'        tells whether each industry's inputs are unit-homogeneous.
#'        Default is "\code{.inputs_unit_homogeneous}".
#'
#' @return a list or data frame containing
#'         \code{TRUE} if inputs to each energy conversion industry are unit-homogeneous,
#'         \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   inputs_unit_homogeneous()
inputs_unit_homogeneous <- function(.sutmats = NULL,
                                    # Input names
                                    U = "U", S_units = "S_units",
                                    keep_details = FALSE,
                                    # Output names
                                    ins_unit_homogeneous = ".inputs_unit_homogeneous"){
  inputs_unit_homogeneous_func <- function(U_mat, S_units_mat){
    U_bar <- transpose_byname(S_units_mat) %>% matrixproduct_byname(U_mat)
    num_non_zero <- count_vals_incols_byname(U_bar, "!=", 0)
    out <- num_non_zero == 1
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(ins_unit_homogeneous)
  }
  matsindf_apply(.sutmats, FUN = inputs_unit_homogeneous_func, U_mat = U, S_units_mat = S_units)
}


#' Tell whether industry outputs are unit-homogeneous
#'
#' Returns \code{TRUE} if each industry's output are unit-homogeneous.
#'
#' The \code{V_bar} matrix is queried for the number of non-zero entries in each row.
#' If the number of non-zero entries in each row is exactly 1,
#' industry outputs are unit-homogeneous.
#' Note that \code{V_bar = \link[matsbyname]{matrixproduct_byname}(V, S_units)}.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param V a make (\code{V}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{V}".
#' @param S_units an \code{S_units} matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param keep_details if \code{TRUE}, per-industry results are returned;
#'        if \code{FALSE}, per-ECC results are returned.
#' @param outs_unit_homogeneous the name of the output column
#'        that tells whether each industry's outputs are unit-homogeneous.
#'        Default is "\code{.outputs_unit_homogeneous}".
#'
#' @return a list or data frame containing
#'         \code{TRUE} if outputs from each energy conversion industry are unit-homogeneous,
#'         \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   outputs_unit_homogeneous()
outputs_unit_homogeneous <- function(.sutmats = NULL,
                                     # Input names
                                     V = "V", S_units = "S_units",
                                     keep_details = FALSE,
                                     # Output names
                                     outs_unit_homogeneous = ".outputs_unit_homogeneous"){

  outputs_unit_homogeneous_func <- function(V_mat, S_units_mat){
    V_bar <- matrixproduct_byname(V_mat, S_units_mat)
    num_non_zero <- count_vals_inrows_byname(V_bar, "!=", 0)
    out <- num_non_zero == 1
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(outs_unit_homogeneous)
  }
  matsindf_apply(.sutmats, FUN = outputs_unit_homogeneous_func, V_mat = V, S_units_mat = S_units)
}


#' Tell whether industry flows (inputs and outputs) are unit-homogeneous
#'
#' Returns \code{TRUE} if each industry's flows (all inputs and outputs) are unit-homogeneous.
#'
#' The \code{V_bar} matrix is queried for the number of non-zero entries in each row.
#' If the number of non-zero entries in each row is exactly 1,
#' industry outputs are unit-homogeneous.
#' Note that \code{V_bar = \link[matsbyname]{matrixproduct_byname}(V, S_units)}.
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U a use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V a make (\code{V}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{V}".
#' @param S_units an \code{S_units} matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param keep_details if \code{TRUE}, per-industry results are returned;
#'        if \code{FALSE}, per-ECC results are returned.
#' @param flows_unit_homogeneous the name of the output column
#'        that tells whether each industry's outputs are unit-homogeneous.
#'        Default is "\code{.flows_unit_homogeneous}".
#'
#' @return \code{.sutdata} with additional column "\code{flows_unit_homogeneous}" containing
#'         \code{TRUE} if each industry's flows are unit-homogeneous,
#'         \code{FALSE} if each industry's flows are unit-heterogeneous.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   flows_unit_homogeneous()
flows_unit_homogeneous <- function(.sutdata = NULL,
                                   # Input names
                                   U = "U", V = "V", S_units = "S_units",
                                   keep_details = FALSE,
                                   # Output names
                                   flows_unit_homogeneous = ".flows_unit_homogeneous"){

  flows_unit_homogeneous_func <- function(U_mat, V_mat, S_units_mat){
    U_bar <- matrixproduct_byname(transpose_byname(S_units_mat), U_mat)
    V_bar <- matrixproduct_byname(V_mat, S_units_mat)
    # Add V_bar and U_bar_T to obtain a matrix with industries in rows and units in columns.
    sums_by_unit <- sum_byname(V_bar, transpose_byname(U_bar))
    # If rows of sums_by_unit have 1 non-zero row, the inputs and outputs for the industry of that row are unit-homogeneous.
    # If rows of sums_by_unit have more than 1 non-zero row, the inputs and outputs for the industry of that row are unit-inhomogeneous.
    num_non_zero <- count_vals_inrows_byname(sums_by_unit, "!=", 0)
    out <- num_non_zero == 1
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(flows_unit_homogeneous)
  }

  matsindf_apply(.sutdata, FUN = flows_unit_homogeneous_func, U_mat = U, V_mat = V, S_units_mat = S_units)
}
