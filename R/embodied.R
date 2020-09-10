#
# This file contains functions that help to calculate embodied primary energy
#

#' Calculate various embodied energy matrices
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure
#' (using the supply-use table format) of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{Y}".
#' @param q final demand (\code{q}) vector or name of the column in \code{.iodata} containing same. Default is "\code{q}".
#' @param L_ixp industry-by-product Leontief (\code{L_ixp}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{L_ixp}"
#' @param g name of the \code{g} vector on output. Default is "\code{g}".
#' @param W name of the \code{W} matrix on output. Default is "\code{W}".
#' @param U_EIOU name of the \code{U_EIOU} matrices on output. Default is "\code{U_EIOU}".
#' @param G name of the \code{G} matrix on output.
#'        \code{G} is calculated by \code{L_ixp * y_hat}. Default is "\code{G}".
#' @param H name of the \code{H} matrix on output.
#'        \code{H} is calculated by \code{L_ixp * Y}. Default is "\code{H}".
#' @param E name of \code{E} matrix on output.
#'        \code{E} is calculated by \code{W * g_hat_inv}. Default is "\code{E}".
#' @param M_p name of the \code{M_p} matrix on output.
#'        \code{M_p} is formed from column sums of positive entries in the various Qx matrices.
#'        Default is "\code{M_p}".
#' @param M_s name of the \code{M_s} matrix on output.
#'        \code{M_s} is constructed by \code{M_p * q_hat_inv * Y}. Default is "\code{M_s}".
#' @param F_footprint_p name of the \code{F_footprint_p} matrix on output.
#'        \code{F_footprint_p} is calculated by \code{M_p * (M_p^T * i)_hat_inv}. Default is "\code{F_footprint_p}".
#' @param F_effects_p name of the \code{F_effects_p} matrix on output.
#'        \code{F_effects_p} is calculated by \code{(M_p * i)_hat_inv * M_p}. Default is "\code{F_effects_p}".
#' @param F_footprint_s name of the \code{F_footprint_s} matrix on output.
#'        \code{F_footprint_s} is calculated by \code{M_s * (M_s^T *i)_hat_inv}. Default is "\code{F_footprint_s}".
#' @param F_effects_s name of the \code{F_effects_s} matrix on output.
#'        \code{F_effects_s} is calculated by \code{(M_s * i)_hat_inv * M_s}. Default is "\code{F_effects_s}".
#'
#' @return a list or data frame containing embodied energy matrices
#'
#' @export
calc_embodied_mats <- function(.iomats = NULL,
                               # Input names
                               Y = "Y", q = "q",
                               L_ixp = "L_ixp", g = "g", W = "W", U_EIOU = "U_EIOU",
                               # Output names
                               G = "G", H = "H", E = "E",
                               M_p = "M_p", M_s = "M_s",
                               F_footprint_p = "F_footprint_p", F_effects_p = "F_effects_p",
                               F_footprint_s = "F_footprint_s", F_effects_s = "F_effects_s"){
  embodied_func <- function(Y_mat, q_vec, L_ixp_mat, g_vec, W_mat, U_EIOU_mat){
    GH_list <- calc_GH(Y = Y_mat, L_ixp = L_ixp_mat,
                       G = G, H = H)
    G_mat <- GH_list[[G]]
    E_list <- calc_E(g = g_vec, W = W_mat, U_EIOU = U_EIOU_mat,
                     E = E)
    E_mat <- E_list[[E]]
    M_list <- calc_M(Y = Y_mat, q = q_vec, G = G_mat, E = E_mat,
                     M_p = M_p, M_s = M_s)
    M_p_mat <- M_list[[M_p]]
    M_s_mat <- M_list[[M_s]]
    F_list <- calc_F_footprint_effects(M_p = M_p_mat, M_s = M_s_mat,
                                     F_footprint_p = F_footprint_p, F_effects_p = F_effects_p,
                                     F_footprint_s = F_footprint_s, F_effects_s = F_effects_s)
    c(GH_list, E_list, M_list, F_list) %>% magrittr::set_names(c(names(GH_list), names(E_list), names(M_list), names(F_list)))
  }
  matsindf::matsindf_apply(.iomats, FUN = embodied_func, Y_mat = Y, q_vec = q,
                 L_ixp_mat = L_ixp, g_vec = g, W_mat = W, U_EIOU_mat = U_EIOU)
}

#' Calculate the \code{G} and \code{H} matrices for embodied energy calculations
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{Y}".
#' @param L_ixp industry-by-product Leontief (\code{L_ixp}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{L_ixp}".
#' @param G name for the \code{G} matrix on output. Default is "\code{G}".
#'        \code{G} is calculated by \code{L_ixp * y_hat}.
#' @param H name for the \code{H} matrix on output. Default is "\code{H}".
#'        \code{G} is calculated by \code{L_ixp * Y}.
#'
#' @return a list or data frame containing \code{G} and \code{H} matrices.
#'
#' @export
calc_GH <- function(.iomats = NULL,
                    # Input columns
                    Y = "Y", L_ixp = "L_ixp",
                    # Output columns
                    G = "G", H = "H"){
  GH_func <- function(Y_mat, L_ixp_mat){
    y <- matsbyname::rowsums_byname(Y)
    G_mat <- matsbyname::matrixproduct_byname(L_ixp, matsbyname::hatize_byname(y))
    H_mat <- matsbyname::matrixproduct_byname(L_ixp, Y)
    list(G_mat, H_mat) %>% magrittr::set_names(c(G, H))
  }
  matsindf::matsindf_apply(.iomats, FUN = GH_func, Y_mat = Y, L_ixp_mat = L_ixp)
}

#' Calculate the \code{E} matrix for embodied energy calculations
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.
#' @param g final demand (\code{g}) vector or name of the column in \code{.iomats} containing same. Default is "\code{g}".
#' @param W product-by-industry value added (\code{W}) matrix or name of the column in \code{.iomats} containing same. Default is "\code{W}".
#' @param U_EIOU energy industry own use matrix or name of the column in \code{.iomats} containing same. Default is "\code{U_EIOU}".
#' @param E the name for the \code{E} matrix on output. Default is "\code{E}".
#'        \code{E} is calculated by \code{W * g_hat_inv}.
#'
#' @return list or data frame containing \code{E} matrices
#'
#' @export
calc_E <- function(.iomats = NULL,
                   # Input names
                   g = "g", W = "W", U_EIOU = "U_EIOU",
                   # Output name
                   E = "E"){
  E_func <- function(g_vec, W_mat, U_EIOU_mat){
    E_mat <- matsbyname::matrixproduct_byname(matsbyname::sum_byname(W_mat, U_EIOU_mat), g_vec %>% matsbyname::hatinv_byname())
    list(E_mat) %>% magrittr::set_names(E)
  }
  matsindf::matsindf_apply(.iomats, FUN = E_func, g_vec = g, W_mat = W, U_EIOU_mat = U_EIOU)
}


#' Calculate embodied energy matrices
#'
#' \code{Q} is calculated by \code{e_hat * G},
#' but the e_hat column contains lists of matrices,
#' so the \code{Q} column will also contain lists of matrices.
#' In each list, there is one Q matrix for each Product in the Energy Conversion Chain.

#' @param .YqGHEdata a data frame containing columns with \code{q} vectors
#' and \code{Y}, \code{G}, \code{H}, and \code{E} matrices.
#' \code{.YqGEdata} will likely have been obtained from the \code{\link{calc_GH}} and \code{\link{calc_E}} functions.
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.YqHGEdata} containing same. Default is "\code{Y}".
#' @param q \code{q} column vector or name of the column in \code{.YqHGEdata} containing same. Default is "\code{q}".
#'        \code{q} is calculated by \code{U*i + y}.
#' @param G \code{G} matrix or name of the column in \code{.YqHGEdata} containing same. Default is "\code{G}".
#'        \code{G} is calculated by \code{L_ixp * y_hat}.
#' @param E \code{E} matrix or name of the column in \code{.YqHGEdata} containing same. Default is "\code{E}".
#'        \code{E} is calculated by \code{W * g_hat_inv}.
#' @param tol the allowable energy balance error.
#' @param M_p the name for matrices of embodied energy in products on output. Default is "\code{M_p}".
#'        These matrices contain embodied products in rows and embodying products in columns.
#' @param M_s the name for matrices of embodied energy consumed by final demand sectors. Default is "\code{M_s}".
#'        These matrices contain embodied products in rows and consuming final demand sectors in columns.
#'
#' @return a list or data frame of embodied energy matrices
#'
#' @export
calc_M <- function(.YqGHEdata = NULL,
                   # Input names
                   Y = "Y", q = "q", G = "G", E = "E",
                   tol = 1e-4,
                   # Output names
                   M_p = "M_p", M_s = "M_s"){
  M_func <- function(Y_mat, q_vec, G_mat, E_mat){
    # Form one e vector for each row of the E matrix.
    # All vectors for a given row of the data frame are stored in a list
    # in the e_colname column of the data frame.
    e_vecs <- matsbyname::list_of_rows_or_cols(E_mat, margin = 1)
    # Form one e_hat matrix for each e vector in each list.
    # !!e_hat_colname := matsbyname::hatize_byname(!!as.name(e_colname)),
    e_hat_list <- lapply(e_vecs, FUN = matsbyname::hatize_byname)
    # Calculate Q matrices
    G_list <- matsbyname::make_list(G_mat, n = length(e_hat_list), lenx = 1)
    Q_list <- Map(matsbyname::matrixproduct_byname, e_hat_list, G_list)
    # We're looking for embodied energy, which are positive entries in the Q matrices.
    # Set negative entries in the Q matrices to zero
    Qpos_list <- lapply(Q_list,
                   FUN = function(m){
                     # Here, each m is a Q matrix for a specific Product.
                     # Need to apply our function to each element (e) of m
                     apply(X = m, MARGIN = c(1,2), FUN = function(e){
                       ifelse(e < 0, 0, e)
                     }) %>%
                       # At this point, we have another copy of our matrix with
                       # all negative elements set to zero.
                       # Ensure that row and column types of m are preserved.
                       matsbyname::setrowtype(matsbyname::rowtype(m)) %>%
                       matsbyname::setcoltype(matsbyname::coltype(m))
                   })
    # Calculate column sums for each matrix in Qpos_list.
    # These column sums give the amount of energy of the type given by
    # the Q matrix in the Product of its column name.
    Qposcolsums_list <- lapply(Qpos_list, FUN = matsbyname::colsums_byname)
    # rbind the column sums of each Qpos in Qposcolsums_list into a matrix,
    # with row names taken from the name of the Q matrix whose column sums comprise the row.
    M_p_mat <- do.call(rbind, Qposcolsums_list) %>%
      matsbyname::setrownames_byname(names(Qposcolsums_list)) %>%
      matsbyname::setrowtype(matsbyname::rowtype(E_mat)) %>% matsbyname::setcoltype(matsbyname::rowtype(E_mat))
    # Calculate the "per-sector" embodied energy.
    M_s_mat <- matsbyname::matrixproduct_byname(M_p_mat, q_vec %>% matsbyname::hatinv_byname() %>% matsbyname::matrixproduct_byname(Y_mat))
    # Verify energy balance for embodied matrices (M_p)
    # It should be that q - rowsums(M_p) = 0
    err = q_vec %>% matsbyname::setcolnames_byname("err") %>% matsbyname::setcoltype("err") %>%
      matsbyname::difference_byname(matsbyname::rowsums_byname(M_p_mat) %>%
                                      matsbyname::setcolnames_byname("err") %>%
                                      matsbyname::setcoltype("err"))
    M_p_energy_balance_OK = matsbyname::iszero_byname(err, tol = tol)
    stopifnot(M_p_energy_balance_OK)
    # Everything has checked out. Build our list and return.
    list(M_p_mat, M_s_mat) %>% magrittr::set_names(c(M_p, M_s))
  }
  matsindf::matsindf_apply(.YqGHEdata, FUN = M_func, Y_mat = Y, q_vec = q, G_mat = G, E_mat = E)
}

#' Upstream footprint and downstream effects matrices
#'
#' Calculates upstream footprint matrices (\code{F_footprint_p}, \code{F_footprint_s})
#' and downstream effects matrices (\code{F_effects_p}, \code{F_effects_s})
#' given embodied matrices \code{M_p} and \code{M_s}.
#' Column sums of \code{F_footprint} are 1.
#' Row sums of \code{F_effects} are 1.
#'
#' @param .Mmats a data frame containing a column of embodied matrices
#' @param M_p embodied product matrix or name of the column in \code{.Mmats} containing same. Default is "\code{M_p}".
#' @param M_s embodied sector matrix or name of the column in \code{.Mmats} containing same. Default is "\code{M_s}".
#' @param F_footprint_p the name for \code{F_footprint_p} matrices on output. Default is "\code{F_footprint_p}".
#' @param F_effects_p the name for \code{F_effects_p} matrices on output. Default is "\code{F_effects_p}".
#' @param F_footprint_s the name for \code{F_footprint_s} matrices on output. Default is "\code{F_footprint_s}".
#' @param F_effects_s the name for \code{F_effects_s} matrices on output. Default is "\code{F_effects_s}".
#'
#' @return a list or data frame containing \code{F_footprint_p}, \code{F_effects_p},
#' \code{F_footprint_s}, and \code{F_effects_s} matrices
#'
#' @export
calc_F_footprint_effects <- function(.Mmats = NULL,
                                     # Input names
                                     M_p = "M_p",
                                     M_s = "M_s",
                                     # Output names
                                     F_footprint_p = "F_footprint_p",
                                     F_effects_p = "F_effects_p",
                                     F_footprint_s = "F_footprint_s",
                                     F_effects_s = "F_effects_s"){
  F_func <- function(M_p_mat, M_s_mat){
    # Cleaning zero rows and columns before fractionizing to avoid Inf values.
    F_footprint_p_mat <- matsbyname::fractionize_byname(matsbyname::clean_byname(M_p_mat, margin = 2), margin = 2)
    F_effects_p_mat <- matsbyname::fractionize_byname(matsbyname::clean_byname(M_p_mat, margin = 1), margin = 1)
    F_footprint_s_mat <- matsbyname::fractionize_byname(matsbyname::clean_byname(M_s_mat, margin = 2), margin = 2)
    F_effects_s_mat <- matsbyname::fractionize_byname(matsbyname::clean_byname(M_s_mat, margin = 1), margin = 1)

    # Run some tests to make sure everything is working.
    # Start with footprint matrices
    colsums_F_footprint_p <- matsbyname::colsums_byname(F_footprint_p_mat)
    colsums_F_footprint_s <- matsbyname::colsums_byname(F_footprint_s_mat)
    err_F_footprint_p <- matsbyname::difference_byname(colsums_F_footprint_p, 1)
    err_F_footprint_s <- matsbyname::difference_byname(colsums_F_footprint_s, 1)
    F_footprint_p_OK <- matsbyname::iszero_byname(err_F_footprint_p)
    F_footprint_s_OK <- matsbyname::iszero_byname(err_F_footprint_s)
    assertthat::assert_that(F_footprint_p_OK, msg = "F_footprint_p_OK is not true.")
    assertthat::assert_that(F_footprint_s_OK, msg = "F_footprint_s_OK is not true.")
    # Also check effects matrices
    rowsums_F_effects_p <- matsbyname::rowsums_byname(F_effects_p_mat)
    rowsums_F_effects_s <- matsbyname::rowsums_byname(F_effects_s_mat)
    err_F_effects_p <- matsbyname::difference_byname(rowsums_F_effects_p, 1)
    err_F_effects_s <- matsbyname::difference_byname(rowsums_F_effects_s, 1)
    F_effects_p_OK <- matsbyname::iszero_byname(err_F_effects_p)
    F_effects_s_OK <- matsbyname::iszero_byname(err_F_effects_s)
    assertthat::assert_that(F_effects_p_OK, msg = "F_effects_p_OK is not true.")
    assertthat::assert_that(F_effects_s_OK, msg = "F_effects_s_OK is not true.")

    # Everything checked out, so make our outgoing list and return it.
    list(F_footprint_p_mat, F_effects_p_mat, F_footprint_s_mat, F_effects_s_mat) %>%
      magrittr::set_names(c(F_footprint_p, F_effects_p, F_footprint_s, F_effects_s))
  }
  matsindf::matsindf_apply(.Mmats, FUN = F_func, M_p_mat = M_p, M_s_mat = M_s)
}

#' Embodied energy efficiencies
#'
#' Embodied energy efficiencies are based on the total upstream primary energy demand
#' for a product produced by the ECC or
#' for the energy consumed by a final demand sector of the ECC.
#' This function calculates both.
#' \code{eta_s} gives sector-based embodied energy efficiency, and
#' \code{eta_p} gives product-based embodied energy efficiency.
#'
#' Note that these efficiencies (\code{eta_s} and \code{eta_p}) are different from
#' energy conversion industry efficiencies.
#' To calculate energy conversion industry efficiencies, use the
#' \code{\link{calc_eta_i}} function.
#'
#' @param .embodiedmats a data frame containing columns of \code{Y}, \code{G}, and \code{H} matrices
#' @param primary_machine_names a list of strings representing names of Industries whose output is counted in Total Primary Energy Supply (TPES)
#' @param Y final demand \code{Y} matrix or name of a column in \code{.embodiedmats} containing same. Default is "\code{Y}".
#' @param G \code{G} matrix or name of a column in \code{.embodiedmats} containing same. Default is "\code{G}".
#' @param H \code{H} matrix or name of a column in \code{.embodiedmats} containing same. Default is "\code{H}".
#' @param eta_p the name for product-based efficiencies on output. Default is "\code{eta_p}".
#' @param eta_s the name for final-demand-sector-based efficiencies on output. Default is "\code{eta_s}".
#'
#' @return a list or data frame containing embodied energy efficiencies
#'
#' @export
calc_embodied_etas <- function(.embodiedmats = NULL,
                               # Input information
                               primary_machine_names,
                               # Input columns of .embodiedmats
                               Y = "Y", G = "G", H = "H",
                               # Output columns
                               eta_p = "eta_p", eta_s = "eta_s"){
  eta_func <- function(Y_mat, G_mat, H_mat){
    eta_p_vec <- matsbyname::quotient_byname(
      matsbyname::rowsums_byname(Y_mat) %>% matsbyname::transpose_byname(),
      G_mat %>% matsbyname::select_rows_byname(retain_pattern =
                                                 matsbyname::make_pattern(primary_machine_names, pattern_type = "leading")) %>%
        matsbyname::colsums_byname()
    ) %>%
      matsbyname::transpose_byname() # Make it a column vector
    eta_s_vec <- matsbyname::quotient_byname(
      matsbyname::colsums_byname(Y_mat) %>% matsbyname::setrownames_byname("row") %>% matsbyname::setrowtype("row"),
      H_mat %>% matsbyname::select_rows_byname(retain_pattern =
                                                 matsbyname::make_pattern(primary_machine_names, pattern_type = "leading")) %>%
        matsbyname::colsums_byname() %>% matsbyname::setrownames_byname("row") %>% matsbyname::setrowtype("row")
    ) %>%
      matsbyname::transpose_byname() # Make it a column vector
    list(eta_p_vec, eta_s_vec) %>% magrittr::set_names(c(eta_p, eta_s))
  }
  matsindf::matsindf_apply(.embodiedmats, FUN = eta_func, Y_mat = Y, G_mat = G, H_mat = H)
}



#' Calculate various embodied EIOU matrices
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure
#' (using the supply-use table format) of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{Y}".
#' @param q final demand (\code{q}) vector or name of the column in \code{.iodata} containing same. Default is "\code{q}".
#' @param L_ixp industry-by-product Leontief (\code{L_ixp}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{L_ixp}"
#' @param g name of the \code{g} vector on output. Default is "\code{g}".
#' @param W name of the \code{W} matrix on output. Default is "\code{W}".
#' @param U_EIOU name of the \code{U_EIOU} matrices on output. Default is "\code{U_EIOU}".
#' @param G name of the \code{G} matrix on output.
#'        \code{G} is calculated by \code{L_ixp * y_hat}. Default is "\code{G}".
#' @param H name of the \code{H} matrix on output.
#'        \code{H} is calculated by \code{L_ixp * Y}. Default is "\code{H}".
#' @param E name of \code{E} matrix on output.
#'        \code{E} is calculated by \code{W * g_hat_inv}. Default is "\code{E}".
#'
#' @return a list or data frame containing EIOU embodied matrices
#'
#' @export
calc_embodied_EIOU <- function(.iomats = NULL,
                               # Input names
                               e_EIOU = "e_EIOU",
                               Y = "Y", y = "y",
                               L_ixp = "L_ixp", L_ixp_feed = "L_ixp_feed",
                               # Output names
                               #G = "G", H = "H", G_feed = "G_feed", H_feed = "H_feed",
                               Q_EIOU_p = "Q_EIOU_p", Q_EIOU_s = "Q_EIOU_s",
                               Q_EIOU_feed_p = "Q_EIOU_feed_p", Q_EIOU_feed_s = "Q_EIOU_feed_s"){

  embodied_EIOU_func <- function(e_EIOU_vec, y_vec, Y_mat, L_ixp_mat, L_ixp_feed_mat){

    e_EIOU_hat_vec <- matsbyname::hatize_byname(e_EIOU_vec)
    y_hat_vec <- matsbyname::hatize_byname(y_vec)

    #GH_list <- calc_GH(Y = Y_mat, L_ixp = L_ixp_mat)
    # G_mat <- GH_list[[G]]
    # H_mat <- GH_list[[H]]

    Q_EIOU_p_mat <- matsbyname::matrixproduct_byname(e_EIOU_hat_vec,
                                                     matsbyname::matrixproduct_byname(L_ixp_mat, y_hat_vec))

    Q_EIOU_s_mat <- matsbyname::matrixproduct_byname(e_EIOU_hat_vec,
                                                     matsbyname::matrixproduct_byname(L_ixp_mat, Y_mat))

    #GH_feed_list <- calc_GH(Y = Y_mat, L_ixp = L_ixp_feed_mat)
    # G_feed_mat <- GH_feed_list[[G]]
    # H_feed_mat <- GH_feed_list[[H]]

    Q_EIOU_feed_p_mat <- matsbyname::matrixproduct_byname(e_EIOU_hat_vec,
                                                          matsbyname::matrixproduct_byname(L_ixp_feed_mat, y_hat_vec))
    Q_EIOU_feed_s_mat <-matsbyname::matrixproduct_byname(e_EIOU_hat_vec,
                                                          matsbyname::matrixproduct_byname(L_ixp_feed_mat, Y_mat))

    list(Q_EIOU_p_mat, Q_EIOU_s_mat, Q_EIOU_feed_p_mat, Q_EIOU_feed_s_mat) %>%
      magrittr::set_names(c(Q_EIOU_p, Q_EIOU_s, Q_EIOU_feed_p, Q_EIOU_feed_s))

  }
  matsindf::matsindf_apply(.iomats, FUN = embodied_EIOU_func, e_EIOU_vec = e_EIOU, Y_mat = Y, y_vec = y,
                           L_ixp_mat = L_ixp, L_ixp_feed_mat = L_ixp_feed)
}
