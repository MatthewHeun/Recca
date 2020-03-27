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
# any_start_with <- function(x, target){
#   sapply(target, FUN = function(t){
#     grepl(paste0("^", Hmisc::escapeRegex(t)), x) %>%
#       any()
#     }) %>%
#     as.logical()
# }
any_start_with <- function(x, target){
  sapply(target, FUN = function(t){
    any(startsWith(x, t))
  }) %>%
    magrittr::set_names(NULL)
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
#' @param prefixes a vector or list of strings
#'
#' @return \code{TRUE} if \code{x} starts with any of the strings in \code{target},
#'         \code{FALSE} otherwise.
#'         If \code{x} is a vector or list of strings, the return value is the same length as \code{x}
#'         and contains the result of applying the test to each item in \code{x}.
#'
#' @export
#'
#' @examples
#' startsWith_any_of(x = "prefix - suffix", prefixes = c("a", "b", "prefix"))
#' startsWith_any_of(x = "prefix - suffix", prefixes = c("a", "b", "c"))
#' startsWith_any_of(x = "prefix - suffix", prefixes = "suffix")
#' startsWith_any_of(x = c("Production - Crude", "Production - NG",
#'                          "Exports - Oil", "Exports - Crude"),
#'                    prefixes = c("Production", "Imports"))
startsWith_any_of <- function(x, prefixes){
  sapply(x, FUN = function(one_x){
    any(startsWith(x = one_x, prefix = prefixes))
  }) %>%
    magrittr::set_names(NULL)
}

#' Resource industries
#'
#' Identifies resource industries.
#'
#' Resource industries are industries that make a product without using any products.
#' If `R` is given, its industries are automatically included in the output.
#' Additional resource industries are identified by interrogating
#' the resources (`R`), use (`U`) and make (`V`) matrices.
#' Resource industries are, by definition, present in the `R` matrix,
#' or they have all zeroes in their column of the use matrix (`U`)
#' and at least one non-zero value in their row of the make (`V`) matrix.
#'
#' Argument and value descriptions are written assuming that `.sutdata` is a data frame.
#' Alternatively, `.sutdata` can be unspecified, and `U` and `V` can be matrices.
#' In that case, the return value is a list with a single item (`r_industries`)
#' which contains a vector of names of resource industries for the `U` and `V` matrices.
#'
#' @param .sutdata a list or data frame containing use matrix(ces) and make matrix(ces)
#' @param R resource (`R`) matrix or name of the column in `.sutmats` that contains same. Default is "R".
#' @param U use (`U`) matrix or name of the column in `.sutmats` that contains same. Default is "U".
#' @param V make (`V`) matrix or name of the column in `.sutmats` that contains same. Default is "V".
#' @param r_industries name for the resource industry vector on output. Default is "r_industries".
#'
#' @return a list or data frame with `.sutdata` with an additional column (named with the value of the `p_industries` argument)
#'         containing the resource industries for each row
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' UKEnergy2000mats %>%
#'   spread(key = matrix.name, value = matrix) %>%
#'   resource_industries()
resource_industries <- function(.sutdata = NULL, R = "R", U = "U", V = "V", r_industries = "r_industries"){
  r_industries_func <- function(R_mat = NULL, U_mat, V_mat){
    r_names_R <- NULL
    if (!is.null(R_mat)) {
      # Any industries in the R matrix are, by definition, resource industries.
      r_names_R <- rownames(R_mat) %>% sort()
    }
    completed_cols_U <- matsbyname::complete_rows_cols(a = U_mat, mat = matsbyname::transpose_byname(V_mat), margin = 2) %>%
      matsbyname::sort_rows_cols()
    # Looking for columns of zeroes in the U matrix.
    # Do so by eliminating all columns with zeroes and
    # comparing against the original column names.
    U_clean_names <- matsbyname::clean_byname(completed_cols_U, margin = 2) %>%
      colnames()
    r_names_V <- setdiff(colnames(completed_cols_U), U_clean_names) %>%
      sort()
    r_names <- c(r_names_R, r_names_V)
    list(r_names) %>% magrittr::set_names(r_industries)
  }
  matsindf::matsindf_apply(.sutdata, FUN = r_industries_func, R_mat = R, U_mat = U, V_mat = V)
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
#' \code{\link{separate_RV}} is the inverse of \code{\link{combine_RV}}.
#'
#' @param .sutmats a list or data frame containing use matrix(ces) and make matrix(ces)
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
  separate_RV_func <- function(U_mat, R_plus_V_mat){
    r_industry_names <- resource_industries(U = U_mat, V = R_plus_V_mat, r_industries = "r_inds") %>% unlist()
    if (length(r_industry_names) == 0) {
      warning("No R created in separate_RV")
    }
    new_R_mat <- R_plus_V_mat %>%
      matsbyname::select_rows_byname(retain_pattern = matsbyname::make_pattern(r_industry_names,
                                                                               pattern_type = "exact"))
    new_V_mat <- R_plus_V_mat %>%
      matsbyname::select_rows_byname(remove_pattern = matsbyname::make_pattern(r_industry_names,
                                                                               pattern_type = "exact"))
    list(new_R_mat, new_V_mat) %>% magrittr::set_names(c(R, V))

  }
  matsindf::matsindf_apply(.sutmats, FUN = separate_RV_func, U_mat = U, R_plus_V_mat = R_plus_V)
}

#' Combine resource (\code{R}) and make (\code{V}) matrices into a make plus resource (\code{R_plus_V}) matrix
#'
#' \code{\link{combine_RV}} is the inverse of \code{\link{separate_RV}}.
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
#'   combine_RV()
combine_RV <- function(.sutmats = NULL,
                       # Input names
                       R = "R", V = "V",
                       # Output name
                       R_plus_V = "R_plus_V"){
  combine_RV_func <- function(R_mat, V_mat){
    R_plus_V_mat <- matsbyname::sum_byname(R_mat, V_mat)
    list(R_plus_V_mat) %>% magrittr::set_names(c(R_plus_V))
  }
  matsindf::matsindf_apply(.sutmats, FUN = combine_RV_func, R_mat = R, V_mat = V)
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
    num_ones <- matsbyname::count_vals_inrows_byname(S_units_mat, "==", 1)
    out <- num_ones == 1
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(products_unit_homogeneous)
  }
  matsindf::matsindf_apply(.sutmats, FUN = products_unit_homogeneous_func, S_units_mat = S_units)
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
    U_bar <- matsbyname::transpose_byname(S_units_mat) %>%
      matsbyname::matrixproduct_byname(U_mat) %>%
      matsbyname::transpose_byname()
    num_non_zero <- matsbyname::count_vals_inrows_byname(U_bar, "!=", 0)
    out <- num_non_zero == 1
    out <- magrittr::set_colnames(out, ins_unit_homogeneous)
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(ins_unit_homogeneous)
  }
  matsindf::matsindf_apply(.sutmats, FUN = inputs_unit_homogeneous_func, U_mat = U, S_units_mat = S_units)
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
    V_bar <- matsbyname::matrixproduct_byname(V_mat, S_units_mat)
    num_non_zero <- matsbyname::count_vals_inrows_byname(V_bar, "!=", 0)
    out <- num_non_zero == 1
    out <- magrittr::set_colnames(out, outs_unit_homogeneous)
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(outs_unit_homogeneous)
  }
  matsindf::matsindf_apply(.sutmats, FUN = outputs_unit_homogeneous_func, V_mat = V, S_units_mat = S_units)
}

#' Tell whether industry inputs are unit-homogeneous and industry outputs are unit-homogeneous
#'
#' Returns \code{TRUE} if each industry's inputs are unit-homogeneous and each industry's outputs are unit homogeneous.
#' When inputs have different units from outputs, (but all inputs are unit-homogeneous and all outputs are unit-homogeneous),
#' \code{TRUE} is returned.
#'
#' This function uses both \link{inputs_unit_homogeneous} and \link{outputs_unit_homogeneous} internally.
#' This function differs from \link{flows_unit_homogeneous}, because \link{flows_unit_homogeneous}
#' requires that all flows are unit-homogeneous before returning \code{TRUE}.
#' This function (\link{inputs_outputs_unit_homogeneous}) will return true when all
#' inputs are unit-homogeneous with different units from outputs (which are also unit-homogeoenous).
#'
#' @seealso \link{inputs_unit_homogeneous}, \link{outputs_unit_homogeneous}, \link{flows_unit_homogeneous}
#'
#' @param .sutmats a data frame of supply-use table matrices with matrices arranged in columns.
#' @param U a use (\code{U}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{U}".
#' @param V a make (\code{V}) matrix or name of the column in \code{.sutmats} that contains same. Default is "\code{V}".
#' @param S_units an \code{S_units} matrix or name of a column in \code{.sutmats} that contains same. Default is "\code{S_units}".
#' @param keep_details if \code{TRUE}, per-industry results are returned;
#'        if \code{FALSE}, per-ECC results are returned.
#' @param ins_outs_unit_homogeneous the name of the output column
#'        that tells whether each industry's inputs and outputs are unit-homogeneous
#'        (though not necessarily in the same units).
#'        Default is "\code{.inputs_outputs_unit_homogeneous}".
#'
#' @return a list or data frame containing
#'         \code{TRUE} if inputs from each energy conversion industry are unit-homogeneous
#'         and outputs from each energy conversion industry are unit-homogeneous,
#'         \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' result <- UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   inputs_outputs_unit_homogeneous()
inputs_outputs_unit_homogeneous <- function(.sutmats = NULL,
                                           # Input names
                                           U = "U", V = "V", S_units = "S_units",
                                           keep_details = FALSE,
                                           # Output names
                                           ins_outs_unit_homogeneous = ".inputs_outputs_unit_homogeneous"){
  inputs_outputs_unit_homogeneous_func <- function(U_mat, V_mat, S_units_mat){
    ins_homo <- inputs_unit_homogeneous(U = U_mat, S_units = S_units_mat,
                                        keep_details = keep_details, ins_unit_homogeneous = ins_outs_unit_homogeneous)[[ins_outs_unit_homogeneous]]
    outs_homo <- outputs_unit_homogeneous(V = V_mat, S_units = S_units_mat,
                                          keep_details = keep_details, outs_unit_homogeneous = ins_outs_unit_homogeneous)[[ins_outs_unit_homogeneous]]
    result <- matsbyname::and_byname(ins_homo, outs_homo)
    list(result) %>% magrittr::set_names(ins_outs_unit_homogeneous)
  }

  matsindf::matsindf_apply(.sutmats, FUN = inputs_outputs_unit_homogeneous_func, U_mat = U, V_mat = V, S_units_mat = S_units)
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
#'        Default is "\code{FALSE}".
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
flows_unit_homogeneous <- function(.sutmats = NULL,
                                   # Input names
                                   U = "U", V = "V", S_units = "S_units",
                                   keep_details = FALSE,
                                   # Output names
                                   flows_unit_homogeneous = ".flows_unit_homogeneous"){

  flows_unit_homogeneous_func <- function(U_mat, V_mat, S_units_mat){
    U_bar <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(S_units_mat), U_mat)
    V_bar <- matsbyname::matrixproduct_byname(V_mat, S_units_mat)
    # Add V_bar and U_bar_T to obtain a matrix with industries in rows and units in columns.
    sums_by_unit <- matsbyname::sum_byname(V_bar, matsbyname::transpose_byname(U_bar))
    # If rows of sums_by_unit have 1 non-zero row, the inputs and outputs for the industry of that row are unit-homogeneous.
    # If rows of sums_by_unit have more than 1 non-zero row, the inputs and outputs for the industry of that row are unit-inhomogeneous.
    num_non_zero <- matsbyname::count_vals_inrows_byname(sums_by_unit, "!=", 0)
    out <- num_non_zero == 1
    out <- magrittr::set_colnames(out, flows_unit_homogeneous)
    if (!keep_details) {
      out <- all(out)
    }
    list(out) %>% magrittr::set_names(flows_unit_homogeneous)
  }

  matsindf::matsindf_apply(.sutmats, FUN = flows_unit_homogeneous_func, U_mat = U, V_mat = V, S_units_mat = S_units)
}


#' Reverse an energy conversion chain
#'
#' Leontief's original input-output analysis involved swimming
#' "upstream" to estimate the economy that would be needed if different final demand were observed.
#' But what if different resources were available?
#' The analysis is the same if resources become final demand (and vice versa)
#' and make becomes use (and vice versa).
#' That is, the analysis is the same if you're dealing with a reversed energy conversion chain (ECC).
#' This function performs that reversal.
#'
#' To reverse an ECC, the \code{R}, \code{V}, \code{U}, and \code{Y} matrices
#' need to be transposed and swapped:
#' \code{R} with \code{Y} and
#' \code{V} with \code{U}.
#' This function performs those operations.
#'
#' @param .sutmats the input ECC
#' @param R the \code{R} matrix in the ECC to be reversed. (Default is "\code{R}".)
#' @param V the \code{V} matrix in the ECC to be reversed. (Default is "\code{V}".)
#' @param U the \code{U} matrix in the ECC to be reversed. (Default is "\code{U}".)
#' @param Y the \code{Y} matrix in the ECC to be reversed. (Default is "\code{Y}".)
#' @param suffix the suffix to be added to matrix names. (Default is "_rev".)
#' @param R_rev the name of the \code{R} matrix in the reversed ECC. (Default is "\code{R_rev}".)
#' @param V_rev the name of the \code{V} matrix in the reversed ECC. (Default is "\code{V_rev}".)
#' @param U_rev the name of the \code{U} matrix in the reversed ECC. (Default is "\code{U_rev}".)
#' @param Y_rev the name of the \code{Y} matrix in the reversed ECC. (Default is "\code{Y_rev}".)
#'
#' @return a reversed version of the ECC described by \code{R}, \code{V}, \code{U}, and \code{Y}.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(Recca)
#' library(tidyr)
#' mats <- UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   reverse()
#' mats$R_rev[[1]]
#' mats$U_rev[[1]]
#' mats$V_rev[[1]]
#' mats$Y_rev[[1]]
reverse <- function(.sutmats = NULL,
                    # Input names
                    R = "R", V = "V", U = "U", Y = "Y",
                    # Output names
                    suffix = "_rev",
                    R_rev = paste0(R, suffix), V_rev = paste0(V, suffix), U_rev = paste0(U, suffix), Y_rev = paste0(Y, suffix)){
  reverse_func <- function(R_mat, V_mat, U_mat, Y_mat){
    R_rev_mat <- matsbyname::transpose_byname(Y_mat)
    V_rev_mat <- matsbyname::transpose_byname(U_mat)
    U_rev_mat <- matsbyname::transpose_byname(V_mat)
    Y_rev_mat <- matsbyname::transpose_byname(R_mat)
    list(R_rev_mat, V_rev_mat, U_rev_mat, Y_rev_mat) %>%
      magrittr::set_names(c(R_rev, V_rev, U_rev, Y_rev))
  }

  matsindf::matsindf_apply(.sutmats, FUN = reverse_func, R_mat = R, V_mat = V, U_mat = U, Y_mat = Y)
}


#' Un-escape HTML codes in text
#'
#' Occasionally,
#' we need to un-escape HTML codes in text.
#' If `text` contains HTML codes, they are replaced with `replacements`, which,
#' by default, describe replacements for "`&amp;`", "`&lt;`", and "`&gt;`"
#' ("`&`", "`<`", and "`>`", respectively).
#'
#' HTML codes can arrive in text read from an Excel file by the `openxlsx` package
#' due to a bug documented [here](https://github.com/awalker89/openxlsx/issues/393).
#'
#' @param text a vector (or one-dimensional list) of character strings
#' @param replacements a list of string pairs. Each pair consists of encoded string and unencoded string,
#'                     in that order. Default is
#'                     `list(c("&amp;", "&"), c("&lt;", "<"), c("&gt;", ">"))`
#'
#' @return If `text` is a vector, a vector of un-encoded strings.
#'         If `text` is a list of strings, a list of un-encoded strings of same structure.
#'         If possible, an outgoing list has simplified structure, even to the point of
#'         conversion to vector.
#'
#' @export
#'
#' @examples
#' replace_html_codes(list("a", "&amp;", "&lt;", "&gt;", "bcd"))
#' replace_html_codes(list(c("&amp;", "&amp;"), c("&lt;", "&lt;"), c("&gt;", "&gt;")))
replace_html_codes <- function(text,
                               replacements = list(c("&amp;", "&"),
                                                   c("&lt;", "<"),
                                                   c("&gt;", ">"))) {
  out <- list()
  for (i in 1:length(text)) {
    text_i <- text[[i]]
    if (length(text_i) > 1) {
      # Be recursive here.
      replace_html_codes(text_i, replacements)
    }
    for (r in replacements) {
      text_i <- gsub(pattern = r[[1]], replacement = r[[2]], x = text_i)
    }
    out[[i]] <- text_i
  }
  # Flatten things, if possible
  if (lapply(out, function(strng) {length(strng) == 1}) %>% as.logical() %>% all()) {
    return(unlist(out))
  }
  return(out)
}



