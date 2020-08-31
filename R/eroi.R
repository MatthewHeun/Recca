#
# This file contains functions that help to calculate the EROI of energy carriers
#

#' Calculate various embodied energy matrices
#'
#' #### Change title and add description.
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure
#' (using the supply-use table format) of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the `calc_io_mats()` function.
#' @param g name of the \code{g} vector on output. Default is "\code{g}".
#' @param U_EIOU name of the \code{U_EIOU} matrices on output. Default is "\code{U_EIOU}".
#' @param E_EIOU name for the \code{E_EIOU} matrix on output. Default is "\code{E_EIOU}".
#'        \code{E} is calculated by \code{U_EIOU * g_hat_inv}.
#' @param e_EIOU name for the \code{e_EIOU} vector on output. Default is "\code{e_EIOU}".
#'        \code{e_EIOU} is calculated by \code{transpose(i) * e_EIOU}.
#'
#' @return list or data frame containing the `E_EIOU` matrix and \code{e_EIOU} vector.
#'
#' @export
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
#' This function calculates energy return on investment (EROI)
#' given an data frame input-output matrices for an energy conversion chain.
#' The argument `.iomats` should be wide-by-matrices.
#' See details for types of EROIs that are returned.
#'
#' This function adds many additional columns to `.sutmats`.
#' The default column names use the following naming convention:
#' * names of EROIs calculated for products include the string "_p" and
#' * names of EROIs calculated for industries include the string "_i".
#'
#' Calculations are made also based on inclusion of either
#' * only feedstock inputs ("_feed") or
#' * both feedstocks and EIOU inputs (no additional string in the name).
#'
#' Output columns include:
#' * `g_eroi_p` gives the name for the column in the output data frame
#'   for the vector of product-level gross EROIs.
#'   These EROIs include both energy used in feedstocks and EIOU production.
#'   The inverse of `g_eroi_p` is calculated by `transpose(i) %*% e_EIOU_hat %*% L_ixp`.
#'
#'
#'
#' Note: All matrix multiplication (`%*%`) is performed "by name" using
#' `matsbyname::matrixproduct_byname()`.
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure
#' (using the supply-use table format) of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.
#' @param e_EIOU name for the \code{e_EIOU} vector on output. Default is "\code{e_EIOU}".
#'        \code{e_EIOU} is calculated by \code{i * e_EIOU}.
#' @param L_ixp name for `L_ixp` matrix on output. Default is "L_ixp".
#'        `L_ixp` is calculated by `D * L_pxp`.
#' @param L_ixp_feed name for `L_ixp_feed` matrix on output. Default is "L_ixp_feed".
#'        `L_ixp_feed` is calculated by `D_feed * L_pxp_feed`.
#' @param D name for `D` matrix on output. Default is "D".
#'        `D` is calculated by `V * q_hat_inv`.
#' @param C name for `C` matrix on output. Default is "C".
#'        `C` is calculated by `transpose(V) * g_hat_inv`.
#' @param g_eroi_p The name for the column of gross, product-based EROIs.
#'                 Default is "g_eroi_p".
#' @param n_eroi_p is the vector of product-level net EROIs, including both energy use for feedstock and EIOU production.
#'        `n_eroi_p` is calculated by `g_eroi_p - 1`.
#' @param g_eroi_i is the vector of industry-level gross EROIs, including both energy use for feedstock and EIOU production.
#'        `g_eroi_i_inv` is calculated by `transpose(C) * g_eroi_p_inv`.
#' @param n_eroi_i is the vector of industry-level net EROIs, including both energy use for feedstock and EIOU production.
#'        `n_eroi_i` is calculated by `g_eroi_i - 1`.
#' @param g_eroi_p_feed is the vector of product-level gross EROIs, including only energy use for feedstock production.
#'        `g_eroi_p_feed_inv` is calculated by `transpose(i) * e_EIOU_hat * L_ixp_feed`.
#' @param n_eroi_p_feed is the vector of product-level net EROIs, including only energy use for feedstock production.
#'        `n_eroi_p_feed` is calculated by `g_eroi_p_feed - 1`.
#' @param g_eroi_i_feed is the vector of industry-level gross EROIs, including only energy use for feedstock production.
#'        `g_eroi_i_feed_inv` is calculated by `transpose(C) * g_eroi_p_feed_inv`.
#' @param n_eroi_i_feed is the vector of industry-level net EROIs, including only energy use for feedstock production.
#'        `n_eroi_i_feed` is calculated by `g_eroi_i_feed - 1`.
#'
#' @return A data frame that includes several additional EROIs.
#'         See description for details.
#'
#' @export
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
                       n_eroi_p = "n_eroi_p",
                       g_eroi_i = "g_eroi_i",
                       n_eroi_i = "n_eroi_i",
                       g_eroi_p_feed = "g_eroi_p_feed",
                       n_eroi_p_feed = "n_eroi_p_feed",
                       g_eroi_i_feed = "g_eroi_i_feed",
                       n_eroi_i_feed = "n_eroi_i_feed"
){
  calc_eroi_func <- function(e_EIOU_vec, L_ixp_mat, L_ixp_feed_mat, D_mat, C_mat){
    # First; including both energy use for feedstock and energy use production
    g_eroi_p_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::hatize_byname(e_EIOU_vec), L_ixp_mat) %>%
      matsbyname::colsums_byname() %>%
      matsbyname::transpose_byname()

    g_eroi_p_vec <- 1/g_eroi_p_inv_vec
    n_eroi_p_vec <- g_eroi_p_vec - 1

    # Pushing to industry
    g_eroi_i_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(C_mat), g_eroi_p_inv_vec)

    g_eroi_i_vec <- 1/g_eroi_i_inv_vec
    n_eroi_i_vec <- g_eroi_i_vec - 1

    # Second, including only energy use for feedstock production
    g_eroi_p_feed_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::hatize_byname(e_EIOU_vec), L_ixp_feed_mat) %>%
      matsbyname::colsums_byname() %>%
      matsbyname::transpose_byname()

    g_eroi_p_feed_vec <- 1/g_eroi_p_feed_inv_vec
    n_eroi_p_feed_vec <- g_eroi_p_feed_vec - 1

    # Pushing to industry
    g_eroi_i_feed_inv_vec <- matsbyname::matrixproduct_byname(matsbyname::transpose_byname(C_mat), g_eroi_p_feed_inv_vec)

    g_eroi_i_feed_vec <- 1/g_eroi_i_feed_inv_vec
    n_eroi_i_feed_vec <- g_eroi_i_feed_vec - 1

    list(g_eroi_p_vec, n_eroi_p_vec, g_eroi_i_vec, n_eroi_i_vec,
         g_eroi_p_feed_vec, n_eroi_p_feed_vec, g_eroi_i_feed_vec, n_eroi_i_feed_vec) %>%
      magrittr::set_names(c(eroi_g_p, n_eroi_p, g_eroi_i, n_eroi_i,
                            g_eroi_p_feed, n_eroi_p_feed, g_eroi_i_feed, n_eroi_i_feed))

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
