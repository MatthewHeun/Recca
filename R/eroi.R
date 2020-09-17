#
# This file contains functions that help to calculate the EROI of energy carriers
#

#' Calculate embodied EIOU per industry output
#'
#' This function calculates the EIOU per industry output given a `.iomats` data frame input-output matrices for an
#' energy conversion chain. The returned `E_EIOU` matrix contains the by-product EIOU used by unit of each industry output,
#' while the `e_EIOU` vector contains the total EIOU by unit of industry output.
#'
#' The output `E_EIOU` matrix is calculated as `U_EIOU %*% g_hat_inv`.
#'
#' The output `e_EIOU` vector is sum of `E_EIOU` columns: `transpose(i) %*% E_EIOU`.
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure of an Energy Conversion Chain.
#' `.iomats` will likely have been obtained from the `calc_io_mats()` function.
#' @param g name of the `g` vector on output. Default is "`g`".
#' @param U_EIOU name of the `U_EIOU` matrices on output. Default is "`U_EIOU`".
#' @param E_EIOU name for the `E_EIOU` matrix on output. Default is "`E_EIOU`".
#' @param e_EIOU name for the `e_EIOU` vector on output. Default is "`e_EIOU`".
#'
#' @return List or data frame containing the `E_EIOU` matrix and `e_EIOU` vector.
#'
#' @export
#'
#' @examples
#' library(IEATools)
#' UKEnergy2000mats %>%
#'    dplyr::filter(Last.stage == "Final", Energy.type == "E") %>%
#'    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
#'    calc_io_mats() %>%
#'    calc_E_EIOU()
calc_E_EIOU <- function(.iomats = NULL,
                        # Input names
                        g = "g", U_EIOU = "U_EIOU",
                        # Output names
                        E_EIOU = "E_EIOU",
                        e_EIOU = "e_EIOU"){

  E_EIOU_func <- function(g_vec, U_EIOU_mat){
    E_EIOU_mat <- matsbyname::matrixproduct_byname(U_EIOU_mat, g_vec %>% matsbyname::hatinv_byname())
    e_EIOU_vec <- matsbyname::colsums_byname(E_EIOU_mat) %>%
      matsbyname::transpose_byname()
    list(E_EIOU_mat, e_EIOU_vec) %>% magrittr::set_names(c(E_EIOU, e_EIOU))
  }

  matsindf::matsindf_apply(.iomats, FUN = E_EIOU_func, g_vec = g, U_EIOU_mat = U_EIOU)
}


#' Calculate EROIs
#'
#' This function calculates energy return on investment (EROI) values
#' given a data frame of input-output matrices
#' in the physical supply-use table (PSUT) format for an energy conversion chain.
#'
#' The argument `.iomats` should be a wide-by-matrices data frame.
#'
#' Other input columns are named by the matrices they contain.
#'
#' This function adds many additional columns to `.iomats`, each one containing a particular type of EROI.
#' The default column names use the following naming convention:
#' * names of EROIs calculated for products include the string "_p";
#' * names of EROIs calculated for industries include the string "_i";
#' * names of gross EROIs include the string "_g";
#' * names of net EROIs include the string "_n".
#'
#' In addition, calculations are made based on inclusion of either
#' * only EIOU required for feedstock inputs production (in which case "_feed" appears in the name);
#' * both EIOU required for feedstock and EIOU inputs production (no additional string in the name).
#'
#' Output columns include:
#' * `eroi_g_p`: vector of product-level gross EROIs, including both EIOU required for feedstock and EIOU inputs production.
#'   The inverse of `eroi_g_p` is calculated by `transpose(i) %*% e_EIOU_hat %*% L_ixp`.
#' * `eroi_n_p`: vector of product-level net EROIs, including both EIOU required for feedstock and EIOU inputs production.
#'   `eroi_n_p` is calculated by `eroi_g_p - 1`.
#' * `eroi_g_i`: vector of industry-level gross EROIs, including both EIOU required for feedstock and EIOU inputs production.
#'   The inverse of `eroi_i_p` is calculated by `transpose(C) * eroi_g_p_inv`.
#' * `eroi_n_i`: vector of industry-level net EROIs, including both EIOU required for feedstock and EIOU inputs production.
#'   `eroi_n_i` is calculated by `eroi_g_i - 1`.
#' * `eroi_g_p_feed`: vector of product-level gross EROIs, including only EIOU required for feedstock inputs production.
#'   The inverse of `eroi_g_p_feed` is calculated by `transpose(i) %*% e_EIOU_hat %*% L_ixp_feed`.
#' * `eroi_n_p_feed`: vector of product-level net EROIs, including only EIOU required for feedstock inputs production.
#'   `eroi_n_p_feed` is calculated by `eroi_g_p_feed - 1`.
#' * `eroi_g_i_feed`: vector of industry-level gross EROIs, including only EIOU required for feedstock inputs production.
#'   The inverse of `eroi_i_p` is calculated by `transpose(C) %*% eroi_g_p_feed_inv`.
#' * `eroi_n_i_feed`: vector of industry-level net EROIs, including only EIOU required for feedstock inputs production.
#'   `eroi_n_i_feed` is calculated by `eroi_g_i_feed - 1`.
#'
#'
#' Note: All matrix multiplication (`%*%`) is performed "by name" using
#' `matsbyname::matrixproduct_byname()`.
#'
#' @param .iomats A wide-by-matrices data frame containing matrices that describe the Input-Output structure
#' (using the supply-use table format) of an Energy Conversion Chain.
#' `.iomats` will likely have been obtained combining the `calc_io_mats()` and `calc_E_EIOU()` functions.
#' See the example.
#' @param e_EIOU The name of the column containing `e_EIOU` vectors in `.iomats`. Default is "e_EIOU".
#' @param L_ixp The name of the column containing `L_ixp` matrices in `.iomats`. Default is "L_ixp".
#' @param L_ixp_feed The name of the column containing `L_ixp_feed` matrix in `.iomats`. Default is "L_ixp_feed".
#' @param D The name of the column containing `D` matrices in `.iomats`. Default is "D".
#' @param C The name of the column containing `C` matrix in `.iomats`. Default is "C".
#' @param eroi_g_p The name of the output column containing vectors of product-level gross EROIs, including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_g_p".
#' @param eroi_n_p The name of the output column containing product-level net EROIs,
#'                 including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_n_p".
#' @param eroi_g_i The name of the output column containing vectors of industry-level gross EROIs,
#'                 including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_g_i".
#' @param eroi_n_i The name of the output column containing vectors of industry-level net EROIs,
#'                 including both energy use for feedstock and EIOU production.
#'                 Default is "eroi_n_i".
#' @param eroi_g_p_feed The name of the output column containing vectors of product-level gross EROIs,
#'                      including only energy use for feedstock production.
#'                      Default is "eroi_g_p_feed".
#' @param eroi_n_p_feed The name of the output column containing vectors of product-level net EROIs,
#'                      including only energy use for feedstock production.
#'                      Default is "eroi_n_p_feed".
#' @param eroi_g_i_feed The name of the output column containing vectors of industry-level gross EROIs,
#'                      including only energy use for feedstock production.
#'                      Default is "eroi_g_i_feed".
#' @param eroi_n_i_feed The name of the output column containing vectors of industry-level net EROIs,
#'                      including only energy use for feedstock production.
#'                      Default is "eroi_n_i_feed".
#'
#' @return A data frame that includes several additional EROIs vectors in added columns.
#'         See description for details.
#'
#' @export
#'
#' @examples
#' library(IEATools)
#' UKEnergy2000mats %>%
#'   dplyr::filter(Last.stage == "Final", Energy.type == "E") %>%
#'   tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
#'   calc_io_mats() %>%
#'   calc_E_EIOU() %>%
#'   calc_erois()
calc_erois <- function(.iomats = NULL,
                       # Input names
                       e_EIOU = "e_EIOU",
                       L_ixp = "L_ixp",
                       L_ixp_feed = "L_ixp_feed",
                       D = "D",
                       C = "C",
                       # Output names
                       eroi_g_p = "eroi_g_p",
                       ##### Emmanuel: rename all of these variables to our new convention. ****
                       eroi_n_p = "eroi_n_p",
                       eroi_g_i = "eroi_g_i",
                       eroi_n_i = "eroi_n_i",
                       eroi_g_p_feed = "eroi_g_p_feed",
                       eroi_n_p_feed = "eroi_n_p_feed",
                       eroi_g_i_feed = "eroi_g_i_feed",
                       eroi_n_i_feed = "eroi_n_i_feed"
){
  calc_eroi_func <- function(e_EIOU_vec, L_ixp_mat, L_ixp_feed_mat, D_mat, C_mat){
    # First; including both energy use for feedstock and energy use production
    eroi_g_p_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::hatize_byname(e_EIOU_vec), L_ixp_mat) %>%
      matsbyname::colsums_byname() %>%
      matsbyname::transpose_byname()

    eroi_g_p_vec <- 1/eroi_g_p_inv_vec
    eroi_n_p_vec <- eroi_g_p_vec - 1

    # Pushing to industry
    eroi_g_i_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(C_mat), eroi_g_p_inv_vec)

    eroi_g_i_vec <- 1/eroi_g_i_inv_vec
    eroi_n_i_vec <- eroi_g_i_vec - 1

    # Second, including only energy use for feedstock production
    eroi_g_p_feed_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::hatize_byname(e_EIOU_vec), L_ixp_feed_mat) %>%
      matsbyname::colsums_byname() %>%
      matsbyname::transpose_byname()

    eroi_g_p_feed_vec <- 1/eroi_g_p_feed_inv_vec
    eroi_n_p_feed_vec <- eroi_g_p_feed_vec - 1

    # Pushing to industry
    eroi_g_i_feed_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(C_mat), eroi_g_p_feed_inv_vec)

    eroi_g_i_feed_vec <- 1/eroi_g_i_feed_inv_vec
    eroi_n_i_feed_vec <- eroi_g_i_feed_vec - 1

    list(eroi_g_p_vec, eroi_n_p_vec, eroi_g_i_vec, eroi_n_i_vec,
         eroi_g_p_feed_vec, eroi_n_p_feed_vec, eroi_g_i_feed_vec, eroi_n_i_feed_vec) %>%
      magrittr::set_names(c(eroi_g_p, eroi_n_p, eroi_g_i, eroi_n_i,
                            eroi_g_p_feed, eroi_n_p_feed, eroi_g_i_feed, eroi_n_i_feed))

  }

  matsindf::matsindf_apply(.iomats, FUN = calc_eroi_func, e_EIOU_vec = e_EIOU,
                           L_ixp_mat = L_ixp, L_ixp_feed_mat = L_ixp_feed,
                           D_mat = D, C_mat = C)
}


# The summarise_erois() function, to write later, should summarise the erois calculated by the calc_erois() function so that
# it returns the primary and final stage erois for (1) Oil and oil products, (2) Natural gas, and (3) Coal and coal products.
# So some vectors of characters strings should be defined in there listing which products belong to which category.
# But that can only be done once we have the PSUT on which we will eventually work defined.

# Then, two options:
# (1) For primary stage groups, we should sum up the production (in V matrix) by energy carrier as well as by energy carrier group
# say (Coal - primary), then we sum up the energy embodied in each energy carrier of the group weighted by its share in the primary
# production of that group!

# (2) For final stage groups, we should sum up the use (for energy purposes only - so using Y and U_EIOU only) by energy carrier,
# as well as by group of energy carriers (e.g. "Coal and coal products - Final stage"). Then we should sum up the energy embodied in
# each energy carrier of the group weighted by its share in final energy use in that group!

# Actually, we could also do this at (3) the useful stage, but this will have to wait.


# summarise_erois <- function(){
#
# }
