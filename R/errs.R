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
#' @param V a string for the name of a column of \code{V} matrices in \code{.sutmats}. (Default is "\code{V}".)
#' @param g a string for the name of a column of \code{g} vector in \code{.sutmats}. (Default is "\code{g}".)
#' @param S_units a string for the name of a column of \code{S_units} matrices in \code{.sutmats}. (Default is "\code{S_units}".)
#' @param ner_gamma the name of the net energy ratio column in output. (Default is "\code{ner_gamma}",)
#' @param ger_gamma the name of the gross energy ratio column in output. (Default is "\code{ger_gamma}",
#' @param r_gamma the name of the ratio of energy ratios in output. (Default is "\code{r_gamma}",
#'
#' @return \code{.sutmats} with additional columns "\code{ner_gamma}", "\code{ger_gamma}", and "\code{r_gamma}".
#'
#' @export
#'
#' @examples
#' library(tidyr)
#' library(Recca)
#' UKEnergy2000mats %>%
#'   spread(key = "matrix.name", value = "matrix") %>%
#'   calc_io_mats() %>%
#'   calc_ERRs_gamma()
calc_ERRs_gamma <- function(.sutmats,
                       # Input
                       U = "U", r_EIOU = "r_EIOU", V = "V", g = "g", S_units = "S_units",
                       # Outputs
                       ger_gamma = "ger_gamma", ner_gamma = "ner_gamma", r_gamma= "r_gamma"){

  ERR_func <- function(U_mat, r_EIOU_mat, V_mat, g_vec, S_units_mat){
    # All equation number references are from
    # Heun, Owen, Brockway. 2018.
    # A physical supply-use table framework for energy analysis on the energy conversion chain.
    # Applied Energy, vol 226, pp. 1134-1162.
    # Equation 8 gives the gross energy ratio (ger) for each industry.
    ger <- matsbyname::hadamardproduct_byname(U_mat, r_EIOU_mat) %>%
      matsbyname::colsums_byname() %>%
      matsbyname::hatinv_byname() %>%
      matsbyname::matrixproduct_byname(g_vec)
    ger <- magrittr::set_colnames(ger, ger_gamma)
    # Equation 9 gives the net energy ratio for each industry.
    ner <- matsbyname::difference_byname(ger, 1)
    ner <- magrittr::set_colnames(ner, ner_gamma)
    # Equation 10 gives the ratio of ner/ger for each industry.
    r <- matsbyname::quotient_byname(ner %>% matsbyname::setcolnames_byname(r_gamma),
                                     ger %>% matsbyname::setcolnames_byname(r_gamma))

    # Put NA where the units don't work correctly.
    result_var <- "result"
    units_OK <- inputs_outputs_unit_homogeneous(U = U_mat, V = V_mat, S_units = S_units_mat, ins_outs_unit_homogeneous = result_var, keep_details = TRUE)[[result_var]]
    # Make sure that units_OK and the output vectors have same rows by completing the rows (industries) relative to one another
    ger_completed <- matsbyname::complete_and_sort(units_OK, ger, margin = 1)
    ner_completed <- matsbyname::complete_and_sort(units_OK, ner, margin = 1)
    r_completed <- matsbyname::complete_and_sort(units_OK, r, margin = 1)
    # The complete_and_sort function converts the TRUE/FALSE values in units_OK to 1/0.
    # Convert back to TRUE and FALSE.
    ger_units_OK <- ger_completed$a == 1
    ner_units_OK <- ner_completed$a == 1
    r_units_OK <- r_completed$a == 1
    ger <- ger_completed$b
    ner <- ner_completed$b
    r <- r_completed$b
    # Now set ger, ner, and r entries to NA if the industry is unit-heterogeneous in the first column of the respective vectors.
    ger[which(!ger_units_OK[ , 1]), 1] <- NA_real_
    ner[which(!ner_units_OK[ , 1]), 1] <- NA_real_
    r[which(!r_units_OK[ , 1]), 1] <- NA_real_

    # Return the energy return ratios.
    list(ger, ner, r) %>% magrittr::set_names(c(ger_gamma, ner_gamma, r_gamma))
  }
  matsindf::matsindf_apply(.sutmats, FUN = ERR_func, U_mat = U, r_EIOU_mat = r_EIOU, V_mat = V, g_vec = g, S_units_mat = S_units)
}
