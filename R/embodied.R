#
# This file contains functions that help to calculate embodied primary energy
#

#' Calculate various embodied energy matrices
#'
#' @param .iodata a data frame containing matrices that describe the Input-Output structure
#' (using the supply-use table format) of an Energy Conversion Chain.
#' \code{.iodata} will likely have been obtained from the \code{calc_io_mats} function.
#' @param Y_colname the name of the column in \code{.iodata} containing final demand (\strong{Y}) matrices.
#' @param y_colname the name of the column in \code{.iodata} containing final demand (\strong{y}) vectors.
#' @param q_colname the name of the column in \code{.iodata} containing final demand (\strong{q}) vectors.
#' @param L_ixp_colname the name of the column in \code{.iodata} containing Industry-by-Product
#' Leontief (\code{L_ixp}) matrices.
#' @param G_colname the name of the output column containing \code{G} matrices.
#' \code{G} is calculated by \code{L_ixp * y_hat}.
#' @param H_colname the name of the output column containing \code{H} matrices.
#' \code{H} is calculated by \code{L_ixp * Y}.
#' @param E_colname the name of the output column containing \code{E} matrices.
#' \code{E} is calculated by \code{W * g_hat_inv}.
#' @param M_colname the name of the output column containing matrices of embodied energy.
#' These matrices give the energy of type shown in rows embodied in the Products shown in columns.
#' \code{M} is formed from column sums of positive entries in the various Qx matrices
#' @param F_footprint_p_colname the name of the output column containing \strong{F_footprint_p} matrices.
#' \strong{F}\code{_footprint_p} is calculated by \strong{M_p} (\strong{M_p}^T\strong{i})_hat_inv.
#' @param F_effects_p_colname the name of the output column containing \strong{F_effects_p} matrices.
#' \strong{F}\code{_effects_p} is calculated by \strong{M_p i}_hat_inv \strong{M_p}.
#' @param F_footprint_s_colname the name of the output column containing \strong{F_footprint_s} matrices.
#' \strong{F}\code{_footprint_s} is calculated by \strong{M_s} (\strong{M_s}^T\strong{i})_hat_inv.
#' @param F_effects_s_colname the name of the output column containing \strong{F_effects_s} matrices.
#' \strong{F}\code{_effects_p} is calculated by \strong{M_s i}_hat_inv \strong{M_s}.
#'
#' @return \code{.iodata} with columns
#' \code{G_colname}, \code{H_colname}, \code{E_colname}, and \code{Q_colname} added
#'
#' @export
calc_embodied_mats <- function(.iodata = NULL,
                               # Input columns
                               Y_colname = "Y", q_colname = "q",
                               L_ixp_colname = "L_ixp", g_colname = "g", W_colname = "W", U_EIOU_colname = "U_EIOU",
                               # Output columns
                               G_colname = "G", H_colname = "H", E_colname = "E",
                               M_p_colname = "M_p", M_s_colname = "M_s",
                               F_footprint_p_colname = "F_footprint_p", F_effects_p_colname = "F_effects_p",
                               F_footprint_s_colname = "F_footprint_s", F_effects_s_colname = "F_effects_s"){
  embodied_func <- function(Y, q, L_ixp, g, W, U_EIOU){
    GH_list <- calc_GH(Y_colname = Y, L_ixp_colname = L_ixp,
                       G_colname = G_colname, H_colname = H_colname)
    G <- GH_list$G
    E_list <- calc_E(g_colname = g, W_colname = W, U_EIOU_colname = U_EIOU,
                     E_colname = E_colname)
    E <- E_list$E
    M_list <- calc_M(Y_colname = Y, q_colname = q, G_colname = G, E_colname = E,
                     M_p_colname = M_p_colname, M_s_colname = M_s_colname)
    M_p <- M_list$M_p
    M_s <- M_list$M_s
    F_list <- calc_F_footprint_effects(M_p_colname = M_p, M_s_colname = M_s,
                                     F_footprint_p_colname = F_footprint_p_colname, F_effects_p_colname = F_effects_p_colname,
                                     F_footprint_s_colname = F_footprint_s_colname, F_effects_s_colname = F_effects_s_colname)
    c(GH_list, E_list, M_list, F_list) %>% set_names(c(names(GH_list), names(E_list), names(M_list), names(F_list)))
  }
  matsindf_apply(.iodata, FUN = embodied_func, Y = Y_colname, q = q_colname,
                 L_ixp = L_ixp_colname, g = g_colname, W = W_colname, U_EIOU = U_EIOU_colname)
}

#' Calculate the G and H matrices for embodied energy calculations
#'
#' @param .iodata a data frame containing matrices that describe the Input-Output structure of an Energy Conversion Chain.
#' \code{.iodata} will likely have been obtained from the \code{calc_io_mats} function.
#' @param y_colname the name of the column in \code{.iodata} containing final demand (\code{y}) vectors.
#' @param Y_colname the name of the column in \code{.iodata} containing final demand (\code{Y}) matrices.
#' @param L_ixp_colname the name of the column in \code{.iodata} containing Industry-by-Product
#' Leontief (\code{L_ixp}) matrices.
#' @param G_colname the name of the output column containing \code{G} matrices.
#' \code{G} is calculated by \code{L_ixp * y_hat}.
#' @param H_colname the name of the output column containing \code{H} matrices.
#' \code{G} is calculated by \code{L_ixp * Y}.
#'
#' @return \code{.iodata} with columns \code{G_colname} and \code{H_colname} added.
#'
#' @export
calc_GH <- function(.iodata = NULL,
                    # Input columns
                    Y_colname = "Y", L_ixp_colname = "L_ixp",
                    # Output columns
                    G_colname = "G", H_colname = "H"){
  GH_func <- function(Y, L_ixp){
    y <- rowsums_byname(Y)
    G <- matrixproduct_byname(L_ixp, hatize_byname(y))
    H <- matrixproduct_byname(L_ixp, Y)
    list(G, H) %>% set_names(G_colname, H_colname)
  }
  matsindf_apply(.iodata, FUN = GH_func, Y = Y_colname, L_ixp = L_ixp_colname)
}

#' Calculate the E matrix for embodied energy calculations
#'
#' @param .iodata a data frame containing matrices that describe the Input-Output structure of an Energy Conversion Chain.
#' \code{.iodata} will likely have been obtained from the \code{calc_io_mats} function.
#' @param g_colname the name of the column in \code{.iodata} containing final demand (\code{g}) vectors
#' @param W_colname the name of the column in \code{.iodata} containing Product-by-Industry
#' value added (\code{W}) matrices
#' @param U_EIOU_colname the name of the column in \code{.iodata} containing energy industry own use matrices
#' @param E_colname the name of the output column containing \code{E} matrices.
#' \code{E} is calculated by \code{W * g_hat_inv}.
#'
#' @return \code{.iodata} with column \code{E_colname} added
#'
#' @export
calc_E <- function(.iodata = NULL,
                   # Input columns
                   g_colname = "g", W_colname = "W", U_EIOU_colname = "U_EIOU",
                   # Output columns
                   E_colname = "E"){
  E_func <- function(g, W, U_EIOU){
    E <- matrixproduct_byname(sum_byname(W, U_EIOU), g %>% hatize_byname() %>% invert_byname())
    list(E) %>% set_names(E_colname)
  }
  matsindf_apply(.iodata, FUN = E_func, g = g_colname, W = W_colname, U_EIOU = U_EIOU_colname)
}


#' Add embodied matrices colums to a data frame
#'
#' @param .YqGHEdata a data frame containing columns with \strong{q} vectors
#' and \strong{Y}, \strong{G}, \strong{H}, and \strong{E} matrices.
#' \code{.YqGEdata} will likely have been obtained from the \code{calc_G} and \code{calc_E} functions.
#' @param Y_colname the name of the output column containing \strong{Y} matrices.
#' \strong{Y} is the final demand matrix.
#' @param q_colname the name of the output column containing \strong{q} column vectors.
#' \strong{q} is calculated by \code{\strong{Ui} + \strong{y}}.
#' @param G_colname the name of the output column containing \code{G} matrices.
#' \code{G} is calculated by \code{\strong{L_ixp} * \strong{y_hat}}.
#' @param E_colname the name of the output column containing \strong{E} matrices.
#' \code{E} is calculated by \code{\strong{W} * \strong{g_hat_inv}}.
#' @param Q_colname the name of the output column containing lists of \strong{Q} matrices.
#' \strong{Q} is calculated by \code{\strong{e_hat} * \strong{G}},
#' but the e_hat column contains lists of matrices,
#' so the \code{Q} column will also contain lists of matrices.
#' In each list, there is one Q matrix for each Product in the Energy Conversion Chain.
#' @param M_p_colname the name of the output column containing matrices of embodied energy in products.
#' These matrices contain embodied products in rows and embodying products in columns.
#' @param M_s_colname the name of the output column containing matrices of embodied energy consumed by final demand sectors.
#' These matrices contain embodied products in rows and consuming final demand sectors in columns.
#' @param e_colname the name of the column containing \strong{e} vectors.
#' Each \strong{e} vector is a row of the \strong{E} matrix.
#' Thus, column \code{e_colname} contains lists of \strong{e} vectors,
#' one \strong{e} vector for each row in the corresponding \strong{E} matrix.
#' \code{e_colname} is a column of intermediate results that is not included in the output.
#' @param e_hat_colname the name of the column containing \code{e_hat} matrices.
#' Each \strong{e} vector is converted to an \strong{e_hat} matrix.
#' Thus the \code{e_hat_colname} column contains lists of \strong{e_hat} matrices.
#' \code{e_hat_colname} is a column of intermediate results that is not included in the output.
#' @param Qpos_colname the name of the column containing \strong{Qpos} matrices.
#' Embodied energy entries in \strong{Q} are positive.
#' @param Qposcolsums_colname the name of the column containing column sums of \strong{Qpos} matrices.
#'
#' @return \code{.YqGHEdata} with columns \code{Q_colname}, \code{M_p_colname}, and \code{M_s_colname} added
#'
#' @export
calc_M <- function(.YqGHEdata = NULL,
                   # Input columns
                   Y_colname = "Y", q_colname = "q", G_colname = "G", E_colname = "E",
                   # Output columns
                   M_p_colname = "M_p", M_s_colname = "M_s"){
                   # Column names for intermediate results
                   # e_colname = ".evectors",
                   # e_hat_colname = ".ehatmatrices",
                   # Q_colname = ".Qmatrices",
                   # Qpos_colname = ".Qposmatrices",
                   # Qposcolsums_colname = ".Qposcolsums"
  M_func <- function(Y, q, G, E){
    # Form one e vector for each row of the E matrix.
    # All vectors for a given row of the data frame are stored in a list
    # in the e_colname column of the data frame.
    e <- list_of_rows_or_cols(E, margin = 1)
    # Form one e_hat matrix for each e vector in each list.
    # !!e_hat_colname := hatize_byname(!!as.name(e_colname)),
    e_hat_list <- lapply(e, FUN = hatize_byname)
    # Calculate Q matrices
    G_list <- make_list(G, n = length(e_hat_list), lenx = 1)
    Q_list <- Map(matrixproduct_byname, e_hat_list, G_list)
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
                       setrowtype(rowtype(m)) %>%
                       setcoltype(coltype(m))
                   })
    # Calculate column sums for each matrix in Qpos_list.
    # These column sums give the amount of energy of the type given by
    # the Q matrix in the Product of its column name.
    Qposcolsums_list <- lapply(Qpos_list, FUN = colsums_byname)
    # rbind the column sums of each Qpos in Qposcolsums_list into a matrix,
    # with row names taken from the name of the Q matrix whose column sums comprise the row.
    M_p <- do.call(rbind, Qposcolsums_list) %>%
      setrownames_byname(names(Qposcolsums_list)) %>%
      setrowtype(rowtype(E)) %>% setcoltype(rowtype(E))
    # Calculate the "per-sector" embodied energy.
    M_s <- matrixproduct_byname(M_p, q %>% hatize_byname() %>% invert_byname() %>% matrixproduct_byname(Y))
    list(M_p, M_s) %>% set_names(M_p_colname, M_s_colname)
  }
  matsindf_apply(.YqGHEdata, FUN = M_func, Y = Y_colname, q = q_colname, G = G_colname, E = E_colname)
}

#' Upstream footprint and downstream effects matrices
#'
#' Calculates upstream footprint matrices (\strong{F_footprint_p}, \strong{F_footprint_s})
#' and downstream effects matrices (\strong{F_effects_p}, \strong{F_effects_s})
#' given an embodied matries \strong{M_p} and \strong{M_s}.
#' Column sums of \strong{F_footprint} are 1.
#' Row sums of \strong{F_effects} are 1.
#'
#' @param .Mdata a data frame containing a column of embodied matrices
#' @param M_p_colname the name of the column in \code{.Mdata} containing embodied product  matrices (default is \strong{"M_p"})
#' @param M_s_colname the name of the column in \code{.Mdata} containing embodied sector matrices (default is \strong{"M_s"})
#' @param F_footprint_p_colname the name of the column in the output containing \strong{F_footprint_p} matrices (as a string)
#' @param F_effects_p_colname the name of the column in the output containing \strong{F_effects_p} matrices (as a string)
#' @param F_footprint_s_colname the name of the column in the output containing \strong{F_footprint_s} matrices (as a string)
#' @param F_effects_s_colname the name of the column in the output containing \strong{F_effects_s} matrices (as a string)
#'
#' @return \code{.Mdata} with columns \code{F_footprint_p}, \code{F_effects_p},
#' \code{F_footprint_s}, and \code{F_effects_s} added
#' @export
#'
calc_F_footprint_effects <- function(.Mdata = NULL,
                                     # Input columns
                                     M_p_colname = "M_p",
                                     M_s_colname = "M_s",
                                     # Output columns
                                     F_footprint_p_colname = "F_footprint_p",
                                     F_effects_p_colname = "F_effects_p",
                                     F_footprint_s_colname = "F_footprint_s",
                                     F_effects_s_colname = "F_effects_s"){
  F_func <- function(M_p, M_s){
    F_footprint_p <- matrixproduct_byname(M_p, colsums_byname(M_p) %>% hatize_byname() %>% invert_byname())
    F_effects_p <- matrixproduct_byname(rowsums_byname(M_p) %>% hatize_byname() %>% invert_byname(), M_p)
    F_footprint_s <- matrixproduct_byname(M_s, colsums_byname(M_s) %>% hatize_byname() %>% invert_byname())
    F_effects_s <- matrixproduct_byname(rowsums_byname(M_s) %>% hatize_byname() %>% invert_byname(), M_s)
    list(F_footprint_p, F_effects_p, F_footprint_s, F_effects_s) %>%
      set_names(F_footprint_p_colname, F_effects_p_colname, F_footprint_s_colname, F_effects_s_colname)
  }
  matsindf_apply(.Mdata, FUN = F_func, M_p = M_p_colname, M_s = M_s_colname)
}

#' Embodied energy efficiencies
#'
#' @param .embodiedmats a data frame containing columns of \strong{Y}, \strong{G}, and \strong{H} matrices
#' @param primary_machine_names a list of strings representing names of Industries whose output is counted in TPES
#' @param Y_colname a string for the name of a column of Y matrices in \code{.embodiedmats} (default is \code{Y})
#' @param G_colname a string for the name of a column of G matrices in \code{.embodiedmats} (default is \code{G})
#' @param H_colname a string for the name of a column of H matrices in \code{.embodiedmats} (default is \code{H})
#' @param eta_p_colname a string for the name of the output column containing vectors of product-based efficiencies
#' @param eta_s_colname a string for the name of the output column containing vectors of final-demand-sector-based efficiencies
#'
#' @return \code{.embodiedmats} with columns \code{eta_p_colname} and \code{eta_s_colname} added
#'
#' @export
calc_embodied_etas <- function(.embodiedmats = NULL,
                               # Input information
                               primary_machine_names,
                               # Input columns of .embodiedmats
                               Y_colname = "Y", G_colname = "G", H_colname = "H",
                               # Output columns
                               eta_p_colname = "eta_p", eta_s_colname = "eta_s"){
  eta_func <- function(Y, G, H){
    eta_p <- elementquotient_byname(
      rowsums_byname(Y) %>% transpose_byname(),
      G %>% select_rows_byname(retain_pattern = make_pattern(primary_machine_names, pattern_type = "leading")) %>% colsums_byname()
    ) %>%
      transpose_byname() # Make it a column vector
    eta_s <- elementquotient_byname(
      colsums_byname(Y) %>% setrownames_byname("row") %>% setrowtype("row"),
      H %>% select_rows_byname(retain_pattern = make_pattern(primary_machine_names, pattern_type = "leading")) %>%
        colsums_byname() %>% setrownames_byname("row") %>% setrowtype("row")
    ) %>%
      transpose_byname() # Make it a column vector
    list(eta_p, eta_s) %>% set_names(eta_p_colname, eta_s_colname)
  }
  matsindf_apply(.embodiedmats, FUN = eta_func, Y = Y_colname, G = G_colname, H = H_colname)
}
