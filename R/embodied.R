#
# This file contains functions that help to calculate embodied primary energy
#

#' Calculate various embodied energy matrices
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure
#'                (using the supply-use table format)
#'                of an Energy Conversion Chain.
#'                `.iomats` will likely have been obtained from the `calc_io_mats()` function.
#' @param q Final demand (**q**) vector or name of the column in `.iomats` containing same. Default is "q".
#' @param g A **g** vector or name of the column in `.iomats` containing same. Default is "g".
#' @param r An **r** vector or name of the column in `.iomats` containing same. Default is "r".
#' @param L_ixp Industry-by-product Leontief (**L_ixp**) matrix or name of the column in `.iomats` containing same. Default is "L_ixp".
#' @param A An **A** matrix or name of the column in `.iomats` containing same. Default is "A".
#' @param R A resources (**R**) matrix or name of the column in `.iomats` containing same. Default is "R".
#' @param V A make (**V**) matrix or name of the column in `.iomats` containing same. Default is "V".
#' @param U_feed A feedstock use (**U_feed**) matrix or name of the column in `.iomats` containing same. Default is "U_feed".
#' @param Y A final demand (**Y**) matrix or name of the column in `.iomats` containing same. Default is "Y".
#' @param G The name of the **G** matrix on output. **G** is calculated by `L_ixp %*% y_hat`. Default is "G".
#' @param H The name of the **H** matrix on output. **H** is calculated by `L_ixp %*% Y`. Default is "H".
#' @param E The name of the **E** matrix on output. **E** is calculated by `W %*% g_hat_inv`. Default is "E".
#' @param M_p The name of the **M_p** matrix on output. **M_p** is formed from column sums of positive entries in the various **Q**_x matrices. Default is "M_p".
#' @param M_s The name of the **M_s** matrix on output. **M_s** is calculated by `M_p %*% q_hat_inv %*% Y`. Default is "M_s".
#' @param F_footprint_p The name of the **F_footprint_p** matrix on output. **F_footprint_p** is calculated by `M_p %*% (M_p^T %*% i)_hat_inv`. Default is "F_footprint_p".
#' @param F_effects_p The name of the **F_effects_p** matrix on output. **F_effects_p** is calculated by `(M_p %*% i)_hat_inv %*% M_p`. Default is "F_effects_p".
#' @param F_footprint_s The name of the **F_footprint_s** matrix on output. **F_footprint_s** is calculated by `M_s %*% (M_s^T %*% i)_hat_inv`. Default is "F_footprint_s".
#' @param F_effects_s The name of the **F_effects_s** matrix on output. **F_effects_s** is calculated by `(M_s %*% i)_hat_inv %*% M_s`. Default is "F_effects_s".
#'
#' @return A list or data frame containing embodied energy matrices.
#'
#' @export
calc_embodied_mats <- function(.iomats = NULL,
                               # Input names
                               q = "q", g = "g", r = "r",
                               L_ixp = "L_ixp", A = "A",
                               R = "R", V = "V", U_feed = "U_feed", Y = "Y",
                               # Output names
                               G = "G", H = "H", E = "E",
                               M_p = "M_p", M_s = "M_s",
                               F_footprint_p = "F_footprint_p", F_effects_p = "F_effects_p",
                               F_footprint_s = "F_footprint_s", F_effects_s = "F_effects_s"){
  embodied_func <- function(Y_mat, q_vec, L_ixp_mat, g_vec, r_vec, W_mat, U_EIOU_mat, A_mat, R_mat, V_mat, U_mat, U_feed_mat){
    GH_list <- calc_GH(Y = Y_mat, L_ixp = L_ixp_mat, R = R_mat, A = A_mat, q = q_vec,
                       G = G, H = H)
    G_mat <- GH_list[[G]]
    E_list <- calc_E(R = R_mat, V = V_mat, U_feed = U_feed_mat, g = g_vec, r = r_vec)
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
  matsindf::matsindf_apply(.iomats, FUN = embodied_func, Y_mat = Y, q_vec = q, r_vec = r,
                 L_ixp_mat = L_ixp, g_vec = g, A_mat = A, R_mat = R, V_mat = V, U_feed_mat = U_feed)
}


#' Calculate the \code{G} and \code{H} matrices for embodied energy calculations
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.
#' @param Y final demand (\code{Y}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{Y}".
#' @param R Resources (\code{R}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{R}".
#' @param A The name of the `A` matrix column in the `.iomats` data frame.
#'          Default is "A".
#' @param q The name of the `q` vector in the `.iomats` data frame.
#'          Default is "q".
#' @param L_ixp industry-by-product Leontief (\code{L_ixp}) matrix or name of the column in \code{.iodata} containing same. Default is "\code{L_ixp}".
#' @param G name for the \code{G} matrix on output. Default is "\code{G}".
#'        \code{G} is calculated by \code{G_R + G_V}.
#' @param G_V name for the \code{G_V} matrix on output. Default is "\code{G_V}".
#'        \code{G_V} is calculated by `L_ixp * y_hat`.
#' @param G_R name for the \code{G_R} matrix on output. Default is "\code{G_R}".
#'        \code{G_R} is calculated by `R * q_hat_inv * L_pxp * y_hat`.
#' @param H name for the \code{H} matrix on output. Default is "\code{H}".
#'        \code{H} is calculated by \code{H_V + H_R}.
#' @param H_V name for the \code{H_V} matrix on output. Default is "\code{H_V}".
#'        \code{H_V} is calculated by `L_ixp * Y`.
#' @param H_R name for the \code{H_R} matrix on output. Default is "\code{H_R}".
#'        \code{H_R} is calculated by `R * q_hat_inv * L_pxp * Y`.
#'
#'
#' @return a list or data frame containing \code{G} and \code{H} matrices.
#'
#' @export
calc_GH <- function(.iomats = NULL,
                    # Input columns
                    Y = "Y", L_ixp = "L_ixp", R = "R", A = "A", q = "q",
                    # Output columns
                    G_V = "G_V", G_R = "G_R", G = "G",
                    H_V = "H_V", H_R = "H_R", H = "H"){
  GH_func <- function(Y_mat, L_ixp_mat, R_mat, A_mat, q_vec){

    # Calculating y
    y <- matsbyname::rowsums_byname(Y_mat)

    # Calculating G_V and G_R
    G_V_mat <- matsbyname::matrixproduct_byname(L_ixp, matsbyname::hatize_byname(y, keep = "rownames"))
    G_R_mat <- R_mat %>%
      matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(q_vec, keep = "rownames")) %>%
      matsbyname::matrixproduct_byname(matsbyname::invert_byname(
        matsbyname::Iminus_byname(A_mat))) %>%
      matsbyname::matrixproduct_byname(matsbyname::hatize_byname(y, keep = "rownames"))

    # Calculating H_V and H_R
    H_V_mat <- matsbyname::matrixproduct_byname(L_ixp, Y)
    H_R_mat <- R_mat %>%
      matsbyname::matrixproduct_byname(matsbyname::hatinv_byname(q_vec, keep = "rownames")) %>%
      matsbyname::matrixproduct_byname(matsbyname::invert_byname(
        matsbyname::Iminus_byname(A_mat))) %>%
      matsbyname::matrixproduct_byname(Y_mat)

    # Calculating G and H
    G_mat <- matsbyname::sum_byname(G_V_mat, G_R_mat)
    H_mat <- matsbyname::sum_byname(H_V_mat, H_R_mat)

    # Naming and returning matrices
    list(G_V_mat, G_R_mat, G_mat, H_V_mat, H_R_mat, H_mat) %>% magrittr::set_names(c(G_V, G_R, G, H_V, H_R, H))
  }
  matsindf::matsindf_apply(.iomats, FUN = GH_func, Y_mat = Y, L_ixp_mat = L_ixp, R_mat = R, A_mat = A, q_vec = q)
}

#' Calculate the `E` matrix for embodied energy calculations
#'
#' @param .iomats a data frame containing matrices that describe the Input-Output structure of an Energy Conversion Chain.
#' \code{.iomats} will likely have been obtained from the \code{\link{calc_io_mats}} function.
#' @param R A resources (**R**) matrix or name of column in `.iomats` containing same. Default is "R".
#' @param V A make (**V**) matrix or name of column in `.iomats` containing same. Default is "V".
#' @param U_feed A feedstock use (**U_feed**) matrix or name of column in `.iomats` containing same. Default is "U_feed".
#' @param g A **g** vector or name of column in `.iomats` containing same. Default is "g".
#' @param r An **r** vector or name of column in `.iomats` containing same. Default is "r".
#' @param E The name for the **E** matrix on output. **E** is calculated by `W %*% g_hat_inv`. Default is "E".
#'
#' @return A list or data frame containing `E` matrices.
#'
#' @export
calc_E <- function(.iomats = NULL,
                   # Input names
                   R = "R", V = "V", U_feed = "U_feed", g = "g", r = "r",
                   # Output name
                   E = "E"){
  E_func <- function(R_mat, V_mat, U_feed_mat, g_vec, r_vec){

    R_T_plus_U_T_minus_U <- matsbyname::sum_byname(R_mat, V_mat) %>%
      matsbyname::transpose_byname() %>%
      matsbyname::difference_byname(U_feed_mat)

    r_plus_g <- matsbyname::sum_byname(g_vec, r_vec)

    E_mat <- matsbyname::matrixproduct_byname(R_T_plus_U_T_minus_U, matsbyname::hatinv_byname(r_plus_g, keep = "rownames"))

    list(E_mat) %>% magrittr::set_names(E)
  }
  matsindf::matsindf_apply(.iomats, FUN = E_func, R_mat = R, V_mat = V, U_feed_mat = U_feed, g_vec = g, r_vec = r)
}


#' Calculate embodied energy matrices
#'
#' **M** is calculated by `e_hat * G`,
#' but `e_hat` contains lists of matrices,
#' so the **M** is also contain lists of matrices.
#' In each list, there is one **M** matrix for each Product in the Energy conversion chain.
#'
#' @param .YqGHEdata A data frame containing columns of **q** vectors
#'                   and **Y**, **G**, **H**, and **E** matrices.
#'                   `.YqGEdata` will likely have been obtained from the `calc_GH()` and `calc_E()` functions.
#' @param Y A final demand (**Y**) matrix or name of the column in `.YqHGEdata` containing same. Default is "Y".
#' @param q A **q** column vector or name of the column in `.YqHGEdata` containing same. Default is "q".
#' @param G A **G** matrix or name of the column in `.YqHGEdata` containing same. Default is "G".
#' @param E An **E** matrix or name of the column in `.YqHGEdata` containing same. Default is "E".
#' @param tol The allowable energy balance error.
#' @param M_p The name for matrices of embodied energy in products on output. Default is "M_p".
#'            These matrices contain embodied products in rows and embodying products in columns.
#' @param M_s The name for matrices of embodied energy consumed by final demand sectors. Default is "M_s".
#'            These matrices contain embodied products in rows and consuming final demand sectors in columns.
#'
#' @return A list or data frame of embodied energy matrices.
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
    e_hat_list <- lapply(e_vecs, FUN = matsbyname::hatize_byname, keep = "rownames")
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
    M_s_mat <- matsbyname::matrixproduct_byname(M_p_mat, q_vec %>% matsbyname::hatinv_byname(keep = "rownames") %>% matsbyname::matrixproduct_byname(Y_mat))
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
#' Calculates upstream footprint matrices (**F_footprint_p** and **F_footprint_s**)
#' and downstream effects matrices (**F_effects_p** and **F_effects_s**)
#' given embodied matrices **M_p** and **M_s**.
#' Column sums of **F_footprint** are `1`.
#' Row sums of **F_effects** are `1`.
#'
#' @param .Mmats A data frame containing a column of embodied matrices.
#' @param M_p An embodied product matrix or name of the column in `.Mmats` containing same. Default is "M_p".
#' @param M_s An embodied sector matrix or name of the column in `.Mmats` containing same. Default is "M_s".
#' @param F_footprint_p The name for **F_footprint_p** matrices on output. Default is "F_footprint_p".
#' @param F_effects_p The name for **F_effects_p** matrices on output. Default is "F_effects_p".
#' @param F_footprint_s The name for **F_footprint_s** matrices on output. Default is "F_footprint_s".
#' @param F_effects_s The name for **F_effects_s** matrices on output. Default is "F_effects_s".
#'
#' @return A list or data frame containing **F_footprint_p**, **F_effects_p**,
#'         **F_footprint_s**, and **F_effects_s** matrices.
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
#' **eta_s** gives sector-based embodied energy efficiency, and
#' **eta_p** gives product-based embodied energy efficiency.
#'
#' Note that these efficiencies (**eta_s** and **eta_p**) are different from
#' energy conversion industry efficiencies.
#' To calculate energy conversion industry efficiencies, use the
#' `calc_eta_i()` function.
#'
#' @param .embodiedmats A data frame containing columns of **Y**, **G**, and **H** matrices.
#' @param primary_machine_names A list of strings representing names of Industries whose output is counted in Total Energy Supply (TPES).
#' @param Y A final demand (**Y**) matrix or name of a column in `.embodiedmats` containing same. Default is "Y".
#' @param G A **G**  matrix or name of a column in `.embodiedmats` containing same. Default is "G".
#' @param H An **H** matrix or name of a column in `.embodiedmats` containing same. Default is "H".
#' @param eta_p The name for product-based efficiencies on output. Default is "eta_p".
#' @param eta_s The name for final-demand-sector-based efficiencies on output. Default is "eta_s".
#'
#' @return A list or data frame containing embodied energy efficiencies.
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
#'This function calculates different embodied Energy Industry Own Use (EIOU) matrices (see details) for a given energy conversion chain
#'and final demand, using a data frame of input-output matrices in the physical supply-use table (PSUT) format.
#'
#'The argument `.iomats` should be a wide-by-matrices data frame, obtained combining the `calc_iomats()` and `calc_E_EIOU()`
#'functions as described in the example.
#'
#' This function adds many additional columns to `.iomats`, each one containing particular embodied EIOU matrices.
#'
#' The embodied EIOU matrices are calculated either:
#' * by final demand sector (subscript "_s" appears in the name);
#' * by final demand products (subscript "_p" appears in the name);
#' * including only EIOU required for feedstock inputs production (subscript "_feed" appears in the name);
#' * including both EIOU required for feedstock and EIOU inputs production (no additional subscript).
#'
#' Note: All matrix multiplication (`%*%`) is performed "by name" using
#' `matsbyname::matrixproduct_byname()`.
#'
#' Output columns include:
#' * `Q_EIOU_s`: matrix of embodied EIOU by final demand sectors, including both energy use for feedstock and EIOU production.
#'   `Q_EIOU_s` is calculated by `e_EIOU_hat %*% L_ixp %*% Y`.
#' * `Q_EIOU_p`: matrix of embodied EIOU by final demand products, including both energy use for feedstock and EIOU production.
#'   `Q_EIOU_p` is calculated by `e_EIOU_hat %*% L_ixp %*% y_hat`.
#' * `Q_EIOU_feed_s`: matrix of embodied EIOU by final demand sectors, including only energy use for feedstock production.
#'   `Q_EIOU_feed_s` is calculated by `e_EIOU_hat %*% L_ixp_feed %*% Y`.
#' * `Q_EIOU_feed_p`: matrix of embodied EIOU by final demand products, including only energy use for feedstock production.
#'   `Q_EIOU_feed_p` is calculated by `e_EIOU_hat %*% L_ixp_feed %*% y_hat`.
#'
#' @param .iomats A wide-by-matrices data frame containing matrices that describe the Input-Output structure
#'        (using the supply-use table format) of an Energy Conversion Chain.
#'        `.iomats` will likely have been obtained combining the `calc_io_mats()` and `calc_E_EIOU()` functions.
#'        See the examples.
#' @param e_EIOU A direct energy use extension vector or name of column in `.iomats` containing same. Default is "e_EIOU".
#' @param Y A final demand matrix (**Y**) or name of column in `.iomats` containing same. Default is "Y".
#' @param y A **y** vector or name of column in `.iomats` containing same. Default is "y".
#' @param L_ixp An **L_ixp** matrix or name of column in `.iomats` containing same. Default is "L_ixp".
#' @param L_ixp_feed An **L_ixp_feed** matrix or name of column in `.iomats` containing same. Default is "L_ixp_feed".
#' @param Q_EIOU_s The name of the output column containing the EIOU embodied by final demand sectors, including both energy use for feedstock and EIOU production.
#'                 Default is "Q_EIOU_s".
#' @param Q_EIOU_p The name of the output column containing the EIOU embodied by final demand products, including both energy use for feedstock and EIOU production.
#'                 Default is "Q_EIOU_p".
#' @param Q_EIOU_feed_s The name of the output column containing the EIOU embodied by final demand sectors, including only energy use for feedstock production.
#'                 Default is "Q_EIOU_feed_s".
#' @param Q_EIOU_feed_p The name of the output column containing the EIOU embodied by final demand products, including only energy use for feedstock production.
#'                 Default is "Q_EIOU_feed_p".
#'
#' @return A data frame that contains several embodied EIOU matrices in added columns.
#'         See description for details.
#'
#' @export
#'
#' @examples
#' library(IEATools)
#' UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
#'   calc_io_mats() %>%
#'   calc_E_EIOU() %>%
#'   calc_embodied_EIOU()
calc_embodied_EIOU <- function(.iomats = NULL,
                               # Input names
                               e_EIOU = "e_EIOU",
                               Y = "Y", y = "y",
                               L_ixp = "L_ixp", L_ixp_feed = "L_ixp_feed",
                               # Output names
                               #G = "G", H = "H", G_feed = "G_feed", H_feed = "H_feed",
                               Q_EIOU_s = "Q_EIOU_s", Q_EIOU_p = "Q_EIOU_p",
                               Q_EIOU_feed_s = "Q_EIOU_feed_s", Q_EIOU_feed_p = "Q_EIOU_feed_p"){

  embodied_EIOU_func <- function(e_EIOU_vec, y_vec, Y_mat, L_ixp_mat, L_ixp_feed_mat){

    e_EIOU_hat_vec <- matsbyname::hatize_byname(e_EIOU_vec, keep = "rownames")
    y_hat_vec <- matsbyname::hatize_byname(y_vec, keep = "rownames")


    Q_EIOU_p_mat <- matsbyname::matrixproduct_byname(e_EIOU_hat_vec,
                                                     matsbyname::matrixproduct_byname(L_ixp_mat, y_hat_vec))

    Q_EIOU_s_mat <- matsbyname::matrixproduct_byname(e_EIOU_hat_vec,
                                                     matsbyname::matrixproduct_byname(L_ixp_mat, Y_mat))


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
# EAR, 11/09/2020
