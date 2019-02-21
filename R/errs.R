# This file contains functions to calculate energy return ratios (ERRs) of an ECC.


#' Calculate energy return ratios for the gamma system boundary.
#'
#' Calculates energy return ratios for the gamma system boundary.
#' Calculations are performed as shown in Equations 8, 9, and 10 in
#' Heun, Owen, and Brockway. 2018.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Applied Energy, vol 226, pp. 1134-1162.
#'
#' The energy return ratios for a given industry are calculated
#' iff the units for inputs and outputs for that industry are unit-homogeneous.
#' If units for inputs or outputs are heterogeneous for an industry, \code{NA} is the result.
#'
#' @param .sutmats a data frame containing columns for \code{U} and \code{r_EIOU} matrices and a \code{g} vector.
#' @param U a string for the name of a column of \code{U} matrices in \code{.sutmats}. (Default is "\code{U}".)
#' @param r_EIOU a string for the name of a column of \code{r_EIOU} matrices in \code{.sutmats}. (Default is "\code{r_EIOU}".)
#' @param g a string for the name of a column of \code{g} vector in \code{.sutmats}. (Default is "\code{g}".)
#' @param ner_gamma the name of the net energy ratio column in output. (Default is "\code{ner_gamma}",)
#' @param ger_gamma the name of the gross energy ratio column in output. (Default is "\code{ger_gamma}",
#' @param r_gamma the name of the ratio of energy ratios in output. (Default is "\code{r_gamma}",
#'
#' @return \code{.sutmats} with additional columns "\code{ner_gamma}", "\code{ger_gamma}", and "\code{r_gamma}".
#'
#' @export
#'
#' @examples
calc_err_gamma <- function(.sutmats,
                       # Input
                       U = "U", r_EIOU = "r_EIOU", g = "g", S_units = "S_units",
                       # Outputs
                       ger_gamma = "ger_gamma", ner_gamma = "ner_gamma", r_gamma= "r_gamma"){

  err_func <- function(U_mat, r_EIOU_mat, g_vec){
    # All equation number references are from
    # Heun, Owen, Brockway. 2018.
    # A physical supply-use table framework for energy analysis on the energy conversion chain.
    # Applied Energy, vol 226, pp. 1134-1162.
    # Equation 8 gives the gross energy ratio (ger) for each industry.
    ger <- hadamardproduct_byname(U_mat, r_EIOU_mat) %>% colsums_byname() %>% hatinv_byname() %>% matrixproduct_byname(g_vec)
    dimnames(ger) <- list(dimnames(ger)[[1]], ger_gamma)
    # Equation 9 gives the net energy ratio for each industry.
    ner <- difference_byname(ger, 1)
    dimnames(ner) <- list(dimnames(ner)[[1]], ner_gamma)
    # Equation 10 gives the ratio of ner/ger for each industry.
    r <- quotient_byname(ner %>% setcolnames_byname(r_gamma), ger %>% setcolnames_byname(r_gamma))
    # Return the energy return ratios.
    list(ger, ner, r) %>% magrittr::set_names(c(ger_gamma, ner_gamma, r_gamma))
  }
  matsindf::matsindf_apply(.sutmats, FUN = err_func, U_mat = U, r_EIOU_mat = r_EIOU, g_vec = g)
}
