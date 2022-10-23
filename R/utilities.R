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
      matsbyname::select_rows_byname(retain_pattern = RCLabels::make_or_pattern(strings = r_industry_names,
                                                                               pattern_type = "exact"))
    new_V_mat <- R_plus_V_mat %>%
      matsbyname::select_rows_byname(remove_pattern = RCLabels::make_or_pattern(strings = r_industry_names,
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
#' To reverse an ECC, the **R**, **U**, **V**, and **Y** matrices
#' need to be transposed and swapped:
#' **R** with **Y** and
#' **U** with **V**.
#' This function performs those operations.
#'
#' @param .sutmats the input ECC
#' @param R The **R** matrix in the ECC to be reversed. (Default is "R".)
#' @param U The **U** matrix in the ECC to be reversed. (Default is "U".)
#' @param V The **V** matrix in the ECC to be reversed. (Default is "V".)
#' @param Y The **Y** matrix in the ECC to be reversed. (Default is "Y".)
#' @param R_rev The name of the **R** matrix in the reversed ECC. (Default is "R_rev".)
#' @param U_rev The name of the **U** matrix in the reversed ECC. (Default is "U_rev".)
#' @param V_rev The name of the **V** matrix in the reversed ECC. (Default is "V_rev".)
#' @param Y_rev The name of the **Y** matrix in the reversed ECC. (Default is "Y_rev".)
#'
#' @return A reversed version of the ECC described by **R**, **U**, **V**, and **Y**.
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
                    # Input names or matrices
                    R = Recca::psut_cols$R,
                    U = Recca::psut_cols$U,
                    V = Recca::psut_cols$V,
                    Y = Recca::psut_cols$Y,
                    # Output names
                    R_rev = paste0(Recca::psut_cols$R, "_rev"),
                    U_rev = paste0(Recca::psut_cols$U, "_rev"),
                    V_rev = paste0(Recca::psut_cols$V, "_rev"),
                    Y_rev = paste0(Recca::psut_cols$Y, "_rev")){
  reverse_func <- function(R_mat, U_mat, V_mat, Y_mat){
    R_rev_mat <- matsbyname::transpose_byname(Y_mat)
    U_rev_mat <- matsbyname::transpose_byname(V_mat)
    V_rev_mat <- matsbyname::transpose_byname(U_mat)
    Y_rev_mat <- matsbyname::transpose_byname(R_mat)
    list(R_rev_mat, U_rev_mat, V_rev_mat, Y_rev_mat) %>%
      magrittr::set_names(c(R_rev, U_rev, V_rev, Y_rev))
  }

  matsindf::matsindf_apply(.sutmats,
                           FUN = reverse_func,
                           R_mat = R,
                           U_mat = U,
                           V_mat = V,
                           Y_mat = Y)
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


#' Scrape primary industry names from R, V, and Y matrices
#'
#' Primary industry names are needed for aggregation.
#' This function interrogates the row names of R and V and the column names Y matrices
#' for names that start with `p_industries`.
#' The assumption is that many of these row and column names may have
#' compound names of the form "Resources \[of Oil and gas extraction\]".
#' So this function looks for leading strings.
#' If "Resources" is in `p_industries`,
#' "Resources \[of Oil and gas extraction\]" will be among the returned strings.
#'
#' Note all of `R`, `V`, and `Y` need to be specified.
#'
#' @param .sutdata An optional data frame containing columns of PSUT matrices
#' @param p_industry_prefixes The name of a column in `.sutdata` containing
#'                            vectors of prefixes that identify primary industry names, or
#'                            a vector of prefixes that identify primary industry names.
#'                            Default is `Recca::industry_cols$p_industry_prefixes`.
#'                            Hint: `IEATools::tpes_flows` contains a good list of
#'                            primary industry prefixes.
#' @param R The name of the `R` matrix column in `.sutdata` or an `R` matrix.
#' @param V The name of the `V` matrix column in `.sutdata` or a `V` matrix.
#' @param Y The name of the `Y` matrix column in `.sutdata` or a `Y` matrix.
#' @param p_industries_complete The name of the output column containing complete names of primary industries.
#'        Default is `Recca::industry_cols$p_industries_complete`.
#'
#' @return If `.sutdata` is a data frame, a data frame with additional column `p_industries_complete`.
#'         If `.sutdata` is a list of named matrices (`R`, `V`, and `Y`),
#'         A vector or vectors of full names of primary industries in the `R`, `V`, and `Y` matrices,
#'         a list of primary industries.
#'
#' @export
#'
#' @examples
#' Rrows <- c("Resources [of Oil and gas extraction]", "Resources [of Coal mines]")
#' R <- matrix(c(1, 0,
#'               0, 2), nrow = 2, byrow = TRUE,
#'             dimnames = list(Rrows, c("Crude oil", "Brown coal")))
#' Vrows <- c("Imports [of Crude oil]", "Stock changes [of Bituminous coal]")
#' V <- matrix(c(3, 0,
#'               0, 4), nrow = 2, byrow = TRUE,
#'             dimnames = list(Vrows, c("Crude oil", "Bituminous coal")))
#' Ycols <- c("Exports [of Electricity]", "International marine bunkers [of Gas/diesel oil]")
#' Y <- matrix(c(5, 0,
#'               0, 6), nrow = 2, byrow = TRUE,
#'             dimnames = list(c("Electricity", "Gas/diesel oil"), Ycols))
#' p_industry_prefixes <- c("Resources", "Imports", "Exports",
#'                          "Stock changes", "International marine bunkers")
#' # This function works with individual matrices, so long as they are
#' # first wrapped in `list()`.
#' find_p_industry_names(p_industry_prefixes = list(p_industry_prefixes),
#'                       R = list(R), V = list(V), Y = list(Y))
#' # Also works in the context of a data frame.
#' # Use a `tibble`, because it handles matrices better
#' res <- tibble::tibble(R = list(R,R), V = list(V,V), Y = list(Y,Y),
#'                      p_industries = list(p_industry_prefixes, "Resources")) %>%
#'  find_p_industry_names(p_industry_prefixes = "p_industries")
#' res$p_industries_complete[[1]]
#' res$p_industries_complete[[2]]
find_p_industry_names <- function(.sutdata = NULL,
                                  p_industry_prefixes = Recca::industry_cols$p_industry_prefixes,
                                  # Input names
                                  R = Recca::psut_cols$R,
                                  V = Recca::psut_cols$V,
                                  Y = Recca::psut_cols$Y,
                                  # Output column name
                                  p_industries_complete = Recca::industry_cols$p_industries_complete) {

  name_matching_func <- function(p_industry_prefixes_vec, R_mat = NULL, V_mat = NULL, Y_mat = NULL) {
    # Transpose Y matrix so we can operate on rows of all matrices.
    mats <- list(R_mat, V_mat, matsbyname::transpose_byname(Y_mat))

    full_p_industry_names <- lapply(mats, function(m) {
      m %>%
        matsbyname::select_rows_byname(retain_pattern =
                                         RCLabels::make_or_pattern(strings = p_industry_prefixes_vec, pattern_type = "leading")) %>%
        matsbyname::getrownames_byname()
    }) %>%
      # flatten
      unlist()
    # If we don't have a list, make a list.
    if (!is.list(full_p_industry_names)) {
      full_p_industry_names <- list(full_p_industry_names)
    }
    full_p_industry_names %>%
      magrittr::set_names(p_industries_complete)
  }

  matsindf::matsindf_apply(.sutdata,
                           FUN = name_matching_func,
                           p_industry_prefixes_vec = p_industry_prefixes,
                           R_mat = R,
                           V_mat = V,
                           Y_mat = Y)
}


#' Write energy conversion chain matrices in an Excel file
#'
#' It is often helpful to see energy conversion chain (ECC) matrices in Excel format,
#' arranged spatially.
#' This function takes ECC matrices and writes them to an Excel file.
#'
#' If `.psut_data` is a PSUT data frame,
#' each row is written to a different tab in the output file at `path`.
#'
#' @param .psut_data A list or data frame of energy conversion chains.
#' @param path The path of the Excel file to be created.
#' @param overwrite_file A boolean that tells whether you want to overwrite
#'                       the file at `path`, if it already exists.
#' @param pad The number of rows and columns between adjacent matrices in the Excel sheet.
#'            Default is `2`.
#' @param include_io_mats A boolean that tells whether to include input-output matrices
#'                        in the worksheets written by this function.
#'                        Input-output matrices are obtained from `calc_io_mats()`.
#'                        Default is `FALSE`.
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,S_units Names of ECC matrices or actual matrices.
#'                                             See `Recca::psut_cols`.
#' @param .wrote_mats_colname The name of the outgoing column
#'                            that tells whether a worksheet was written successfully.
#'                            Default is "Wrote mats".
#' @param UV_bg_color The color of cells containing U and V matrices.
#'                    Default is a creamy yellow.
#' @param RY_bg_color The color of cells containing R and Y matrices.
#'                    Default is a rust color.
#' @param calculated_bg_color The color of cells containing calculated matrices.
#'                            Default is gray.
#' @param col_widths The widths of columns of matrices.
#'                   Default is `7` to save space.
#'
#' @return An unmodified version of `.psut_data` (if not `NULL`) or a list of
#'         the incoming matrices.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ecc <- UKEnergy2000mats %>%
#'   tidyr::spread(key = "matrix.name", value = "matrix")
#' ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")
#' write_ecc_to_excel(ecc, path = ecc_temp_path, overwrite = TRUE)
#' }
write_ecc_to_excel <- function(.psut_data = NULL,
                               path,
                               overwrite_file = FALSE,
                               pad = 2,
                               include_io_mats = FALSE,
                               R = Recca::psut_cols$R,
                               U = Recca::psut_cols$U,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               r_eiou = Recca::psut_cols$r_eiou,
                               U_eiou = Recca::psut_cols$U_eiou,
                               U_feed = Recca::psut_cols$U_feed,
                               S_units = Recca::psut_cols$S_units,
                               .wrote_mats_colname = "Wrote mats",
                               UV_bg_color = "#FDF2D0",
                               RY_bg_color = "#D3712D",
                               calculated_bg_color = "#D9D9D9",
                               col_widths = 7) {

  # Check if path exists. If so, throw an error.
  if (file.exists(path) & !overwrite_file) {
    stop(paste("File", path, "already exists. Call write_ecc_to_excel(overwrite = TRUE) to overwrite."))
  }
  # Create the workbook
  ecc_wb <- openxlsx::createWorkbook()

  create_one_tab <- function(R_mat, U_mat, V_mat, Y_mat, U_eiou_mat, U_feed_mat, r_eiou_mat, S_units_mat) {

    # Get existing sheet names
    existing_sheets <- openxlsx::sheets(ecc_wb)
    if (length(existing_sheets) == 0) {
      sheet_name <- "1"
    } else {
      sheet_name <- (as.integer(existing_sheets) %>% max()) + 1
    }
    # Add the worksheet to the workbook
    openxlsx::addWorksheet(ecc_wb, sheet_name)

    # Complete matrices relative to one another to make sure we have same number
    # of rows or columns, as appropriate
    # Ensure same columns of U and rows of V
    U_mat_T <- matsbyname::transpose_byname(U_mat)
    completedUV <- matsbyname::complete_and_sort(U_mat_T, V_mat, margin = 1)
    U_mat <- matsbyname::transpose_byname(completedUV[[1]])
    V_mat <- completedUV[[2]]
    # Ensure same columns for R and V
    completedRV <- matsbyname::complete_and_sort(R_mat, V_mat, margin = 2)
    R_mat <- completedRV[[1]]
    V_mat <- completedRV[[2]]
    # Ensure same rows for U and Y
    completedUY <- matsbyname::complete_and_sort(U_mat, Y_mat, margin = 1)
    U_mat <- completedUY[[1]]
    Y_mat <- completedUY[[2]]
    # Ensure same rows and cols for U_EIOU and U
    completedU_eiou <- matsbyname::complete_and_sort(U_eiou_mat, U_mat, margin = c(1, 2))
    U_eiou_mat <- completedU_eiou[[1]]
    # Ensure same rows and cols for U_feed and U
    completedU_feed <- matsbyname::complete_and_sort(U_feed_mat, U_mat, margin = c(1, 2))
    U_feed_mat <- completedU_feed[[1]]
    # Ensure same rows and cols for r_EIOU and U
    completedr_eiou <- matsbyname::complete_and_sort(r_eiou_mat, U_mat, margin = c(1, 2))
    r_eiou_mat <- completedr_eiou[[1]]
    # Ensure same rows for S_units and U
    completedS_units <- matsbyname::complete_and_sort(S_units_mat, U_mat, margin = 1)
    S_units_mat <- completedS_units[[1]]

    # Calculate starting locations for each matrix.
    locations <- calc_mats_locations_excel(R = R_mat,
                                           U = U_mat,
                                           V = V_mat,
                                           Y = Y_mat,
                                           r_eiou = r_eiou_mat,
                                           U_eiou = U_eiou_mat,
                                           U_feed = U_feed_mat,
                                           S_units = S_units_mat,
                                           pad = pad)
    # Write each matrix to the worksheet
    Map(list("R", "U", "V", "Y", "r_eiou", "U_eiou", "U_feed", "S_units"),
        list(R_mat, U_mat, V_mat, Y_mat, r_eiou_mat, U_eiou_mat, U_feed_mat, S_units_mat),
        locations,
        f = function(this_mat_name, this_mat, this_loc) {
          # Find the locations of the matrix origin and matrix extent
          # from this_loc.
          # We'll use this in many places below.
          mat_origin <- this_loc[["origin"]] + c(x = 1, y = 1)  # Offset for the row and column names
          mat_extent <- this_loc[["extent"]] + c(x = 0, y = -1) # Offset for the matrix label
          # Write the data
          openxlsx::writeData(wb = ecc_wb,
                              sheet = sheet_name,
                              x = this_mat,
                              xy = this_loc[["origin"]],
                              array = TRUE, colNames = TRUE, rowNames = TRUE)
          # Set the background color to matrix_bg_color for the numbers in the matrix
          # Define the matrix numbers style
          if (this_mat_name %in% c("R", "Y")) {
            this_bg_color <- RY_bg_color
          } else if (this_mat_name %in% c("U", "V")) {
            this_bg_color <- UV_bg_color
          } else {
            this_bg_color <- calculated_bg_color
          }
          mat_num_style <- openxlsx::createStyle(fgFill = this_bg_color,
                                                 halign = "center",
                                                 valign = "center")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = mat_num_style,
                             rows = mat_origin[["y"]]:mat_extent[["y"]],
                             cols = mat_origin[["x"]]:mat_extent[["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          # Rotate and center column labels
          col_label_style <- openxlsx::createStyle(textRotation = 90,
                                                   halign = "center",
                                                   valign = "bottom")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = col_label_style,
                             rows = this_loc[["origin"]][["y"]],
                             cols = this_loc[["origin"]][["x"]]:this_loc[["extent"]][["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          # Right align row labels
          row_label_style <- openxlsx::createStyle(halign = "right",
                                                   valign = "center")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = row_label_style,
                             rows = this_loc[["origin"]][["y"]]:this_loc[["extent"]][["y"]],
                             cols = this_loc[["origin"]][["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          # Add matrix label
          openxlsx::writeData(wb = ecc_wb,
                              sheet = sheet_name,
                              x = this_mat_name,
                              startRow = this_loc[["extent"]][["y"]],
                              startCol = mat_origin[["x"]])
          # Format matrix label
          mat_name_style <- openxlsx::createStyle(halign = "center",
                                                  textDecoration = "Bold")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = mat_name_style,
                             rows = this_loc[["extent"]][["y"]],
                             cols = mat_origin[["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          openxlsx::mergeCells(wb = ecc_wb,
                               sheet = sheet_name,
                               rows = this_loc[["extent"]][["y"]],
                               cols = mat_origin[["x"]]:mat_extent[["x"]])
          # Set column widths to "auto" to save space.
          openxlsx::setColWidths(wb = ecc_wb,
                                 sheet = sheet_name,
                                 cols = mat_origin[["x"]]:mat_extent[["x"]],
                                 widths = col_widths,
                                 ignoreMergedCells = TRUE)
        })
    list(TRUE) %>%
      magrittr::set_names(.wrote_mats_colname)
  }


  out <- matsindf::matsindf_apply(.psut_data,
                                  FUN = create_one_tab,
                                  R_mat = R,
                                  U_mat = U,
                                  V_mat = V,
                                  Y_mat = Y,
                                  r_eiou_mat = r_eiou,
                                  U_eiou_mat = U_eiou,
                                  U_feed_mat = U_feed,
                                  S_units_mat = S_units)
  # Make sure the directory exists
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  # Write the workbook
  openxlsx::saveWorkbook(ecc_wb, file = path, overwrite = overwrite_file)
}


#' Calculate the origin and extent for each matrix
#'
#' The origin is defined as the upper-left corner of the matrix on the worksheet.
#' The extent is defined as the lower-right corner of the matrix on the worksheet.
#'
#' The outer structure of the return value is matrices,
#' in the order provided in the argument list.
#' The inner structure of the return value is a list of "origin" and "extent,"
#' in that order.
#'
#' This is a helper function, so it is not public.
#'
#' @param R,U,V,Y,r_eiou,U_eiou,U_feed,S_units Matrices to be arranged on an Excel worksheet.
#' @param pad The number of blank rows or columns between matrices.
#'
#' @return A nested list of origins and extents.
calc_mats_locations_excel <- function(R, U, V, Y, r_eiou, U_eiou, U_feed, S_units, pad = 2) {
  # At this point, each argument should be a single matrix.
  # Calculate horizontal sizes for matrices.
  # Each has a +1 due to the column of rownames
  hsizeS_units <- ncol(S_units) + 1
  if (ncol(R) != ncol(V)) {
    stop("R and V should have same number of columns in calc_mats_locations_excel().")
  }
  hsizeVR <- ncol(R) + 1
  hsizeU <- ncol(U) + 1
  hsizeY <- ncol(Y) + 1

  # Calculate vertical sizes for matrices.
  # Each as a +2 due to the row of column names and the label beneath the matrix.
  vsizeS_units <- nrow(S_units) + 2
  if (nrow(U) != nrow(Y)) {
    stop("U and Y should have same number of rows in calc_mats_locations_excel().")
  }
  vsizeUY <- nrow(U) + 2
  vsizeR <- nrow(R) + 2
  vsizeV <- nrow(V) + 2

  # Calculate origin and extent locations for each matrix.
  # The origin is the top left cell of the matrix, including all labels.
  # The extent is the bottom right cell of the matrix, including all labels.
  # x and y are
  # row number (with 1 at the top of the worksheet) and
  # column number (with 1 at the left of the worksheet),
  # respectively.
  originS_units <- c(x = 1, y = 1)
  extentS_units <- originS_units + c(x = hsizeS_units - 1, y = vsizeS_units - 1)

  left_side_U <- hsizeVR + pad + 1

  originU_eiou <- c(x = left_side_U, y = 1)
  extentU_eiou <- originU_eiou + c(x = hsizeU - 1, y = vsizeUY - 1)

  left_side_Y <- extentU_eiou[["x"]] + pad + 1

  originr_eiou <- c(x = left_side_Y, y = 1)
  extentr_eiou <- originr_eiou + c(x = hsizeU - 1, y = vsizeUY - 1)

  originU_feed <- c(x = left_side_U, y = extentU_eiou[["y"]] + pad + 1)
  extentU_feed <- originU_feed + c(x = hsizeU - 1, y = vsizeUY - 1)

  top_row_UY <- extentU_feed[["y"]] + pad + 1

  originU <- c(x = left_side_U, y = top_row_UY)
  extentU <- originU + c(x = hsizeU - 1, y = vsizeUY - 1)

  originY <- c(x = left_side_Y, y = top_row_UY)
  extentY <- originY + c(x = hsizeY - 1, y = vsizeUY - 1)

  originV <- c(x = 1, y = extentU[["y"]] + pad + 1)
  extentV <- originV + c(x = hsizeVR - 1, y = vsizeV - 1)

  originR <- c(x = 1, y = extentV[["y"]] + pad + 1)
  extentR <- originR + c(x = hsizeVR - 1, y = vsizeR - 1)

  list(R = list(origin = originR, extent = extentR),
       U = list(origin = originU, extent = extentU),
       V = list(origin = originV, extent = extentV),
       Y = list(origin = originY, extent = extentY),
       r_eiou = list(origin = originr_eiou, extent = extentr_eiou),
       U_eiou = list(origin = originU_eiou, extent = extentU_eiou),
       U_feed = list(origin = originU_feed, extent = extentU_feed),
       S_units = list(origin = originS_units, extent = extentS_units))
}



