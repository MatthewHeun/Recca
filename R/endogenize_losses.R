#' Endogenize losses into PSUT matrices
#'
#' When a conversion chain does _not_ include losses in the
#' **RUVY** matrices of the PSUT framework,
#' it may be helpful to endogenize the losses.
#' This function performs the endogenization.
#'
#' This function endogenizes losses into the **V** and **Y** matrices,
#' because losses are made (**V**) by industries
#' and gathered by final demand (**Y**).
#' By default, this function creates new
#' `V_prime` and `Y_prime` matrices.
#' Setting `replace_cols = TRUE`
#' (default is `FALSE`)
#' replaces the existing
#' `V` and `Y` matrices with `V_prime` and `Y_prime`,
#' respectively.
#'
#' All losses are allocated to the
#' `loss_sector` column in the **Y** matrix,
#' by default named
#' [Recca::balance_cols]`$losses_sector or
#' "`r Recca::balance_cols$losses_sector`".
#'
#' ## Endogenizing algorithm
#'
#' The endogenizing algorithm is this:
#'
#' - If not present,
#'   calculate the balance vector with
#'   [calc_intra_industry_balance()].
#' - Hatize the balance vector
#'   with [matsbyname::hatize_byname()].
#' - Matrix multiply the hatized balance vector and
#'   the losses_allocation matrix (`losses_alloc`)
#'   to obtain a matrix to be added to **V**.
#' - Transpose the matrix to be added to **V**,
#'   calculate rowsums, and
#'   set the column name to `loss_sector`
#'   to obtain a matrix to be added to **Y**.
#' - Add the matrices to **V** and **Y**, respectively.
#'
#' ## Losses allocation matrix
#'
#' The losses allocation matrix (`losses_alloc`) tells how losses from
#' each industry should be allocated to products.
#' The losses allocation matrix should have
#' industries in rows and
#' products (losses) in rows.
#' The values in the losses allocation matrix are fractions
#' of each industry's losses (in rows) that are allocated
#' to loss products (in columns).
#' Rows of losses allocation matrix must sum to `1`.
#' The value of `losses_alloc` must resolve to a matrix
#' of this form.
#' Options include:
#'
#' * The default value, namely
#'   [Recca::balance_cols]`$default_losses_alloc`,
#'   a 1x1 matrix with
#'   a row named "All industries",
#'   a column named "Waste heat", and
#'   a value of `1`.
#'   This default matrix ascribes losses from all industries
#'   in the **V** matrix
#'   to a product called "Waste heat".
#' * A matrix of the same sense
#'   (industries in rows,
#'   waste products in columns,
#'   rows sum to 1.0)
#'   to be applied to all
#'   rows in the wide-by-matrices data frame supplied
#'   in `.sutmats`.
#'   All industries in the **V** matrix must be present
#'   in the rows of `losses_alloc`.
#'   If the matrix has a single row
#'   (as the default, [Recca::balance_cols]`$default_losses_alloc`),
#'   it is assumed to apply to all industries.
#' * The string name of a column in `.sutmats`
#'   that contains loss allocation matrices for every row
#'   in `.sutmats`.
#'   All industries in the **V** matrix must be present
#'   in the rows of the matrices in the `losses_alloc` column.
#'   If any of the matrices in the column names `losses_alloc`
#'   has a single row,
#'   the row is assumed to apply to all industries.
#'
#' ## Intra-industry balances
#'
#' The intra-industry losses to be endogenized
#' are found in `intra_industry_balance`
#' and can be calculated with [calc_intra_industry_balance()].
#' If `intra_industry_balance` is not present,
#' it is calculated internally via
#' [calc_intra_industry_balance()].
#'
#' ## Cleaning
#'
#' When this function operates
#' on a conversion chain with already-balanced industries,
#' the matrices to be added to **V** and **Y**
#' will be the **0** matrix to within `tol`.
#' Setting `clean = TRUE` removes those rows or columns
#' from the output `V_prime` and `Y_prime` matrices.
#'
#' ## Checks
#'
#' Prior to performing any calculations,
#' [verify_inter_industry_balance()] is checked.
#' There is no point endogenizing losses for
#' a conversion chain that is not internally consistent.
#'
#' After endogenizing the losses,
#' all industries in the conversion chain
#' should pass [verify_intra_industry_balance()],
#' a condition that is checked before returning.
#'
#' @param .sutmats A `matsindf` data frame, wide by matrices, or
#'                 a list of lists of matrices.
#'                 Default is `NULL`.
#' @param R Resources (**R**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "R".
#' @param U Use (**U**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Necessary for verifying calculating losses.
#'          Default is "U".
#' @param V Make (**V**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is "V".
#' @param Y Final demand (**Y**) matrix or name
#'          of the column in `.sutmats` that contains same.
#'          Default is "Y".
#' @param intra_industry_balance A vector or the name of the column containing
#'                               intra-industry balance vectors.
#'                               If missing, losses are calculated internally
#'                               with [calc_intra_industry_balance()]
#'                               before endogenizing.
#'                               Default is
#'                               [Recca::balance_cols]`$intra_industry_balance_colname` or
#'                               "`r Recca::balance_cols$intra_industry_balance_colname`".
#' @param losses_alloc A matrix or the name of the column containing
#'                     loss allocation matrices.
#'                     See details for structure of this matrix.
#'                     Default is [Recca::balance_cols]`$losses_alloc_colname` or
#'                     "`r Recca::balance_cols$losses_alloc_colname`".
#' @param loss_sector The string name of the sector
#'                    that will absorb losses in the **Y** matrix.
#'                    Default is [Recca::balance_cols]`$losses_sector`
#'                    or "`r Recca::balance_cols$losses_sector`".
#' @param replace_cols A boolean that tells whether to
#'                     (a) replace
#'                         the `V` and `Y` columns with
#'                         `V_prime` and `Y_prime` columns, respectively and
#'                     (b) delete the `V_prime`, `Y_prime`, `balance_colname`, and
#'                         `losses_alloc_colname` columns
#'                     after endogenizing the losses
#'                     when `.sutmats` is a data frame or a list.
#'                     Default is `FALSE`.
#' @param clean A boolean that tells whether the outgoing
#'              `V_prime` and `Y_prime` matrices should have
#'              `0` rows and columns removed.
#'              Default is `FALSE`.
#' @param tol The maximum allowable difference from `1` for the rowsums of
#'            loss allocation matrices.
#'            Default is `1e-6`.
#' @param V_prime The name of the **V** matrix with endogenized losses.
#' @param Y_prime The name of the **Y** matrix with endogenized losses.
#'
#' @returns A version of the conversion chain with losses endogenized.
#'
#' @export
#'
#' @examples
#' mats <- UKEnergy2000mats |>
#'   tidyr::pivot_wider(names_from = matrix.name,
#'                      values_from = matrix) |>
#'   dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
#'                   c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
#'   dplyr::mutate(
#'     # Add a matrix column of loss allocations.
#'     # This bit of code adds a default loss allocation matrix
#'     # to every row of the data frame.
#'     "{Recca::balance_cols$losses_alloc_colname}" :=
#'       RCLabels::make_list(Recca::balance_cols$default_losses_alloc,
#'                           n = dplyr::n(),
#'                           lenx = 1)
#'     )
#' dplyr::glimpse(mats)
#' mats |>
#'   calc_intra_industry_balance() |>
#'   endogenize_losses() |>
#'   dplyr::glimpse()
#' # Replace original matrices with endogenized matrices
#' mats |>
#'   calc_intra_industry_balance() |>
#'   endogenize_losses(replace_cols = TRUE) |>
#'   # Check the intra-industry balance.
#'   # Everything should be balanced now.
#'   calc_intra_industry_balance() |>
#'   verify_intra_industry_balance() |>
#'   dplyr::glimpse()
endogenize_losses <- function(
    .sutmats = NULL,
    R = Recca::psut_cols$R,
    U = Recca::psut_cols$U,
    V = Recca::psut_cols$V,
    Y = Recca::psut_cols$Y,
    intra_industry_balance = Recca::balance_cols$intra_industry_balance_colname,
    losses_alloc = Recca::balance_cols$losses_alloc_colname,
    loss_sector = Recca::balance_cols$losses_sector,
    replace_cols = FALSE,
    clean = FALSE,
    tol = 1e-6,
    # Output columns
    V_prime = "V_prime",
    Y_prime = "Y_prime") {

  endogenize_func <- function(R_mat,
                              U_mat,
                              V_mat,
                              Y_mat,
                              balance_vec = NULL, # optional
                              losses_alloc_mat) {

    # Verify that inter-industry balance is present at the beginning
    calc_inter_industry_balance(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat) |>
      verify_inter_industry_balance(tol = tol)

    # If intra-industry balances are not available, calculate them
    if (is.null(balance_vec)) {
      # Calculate intra-industry balances
      balance_vec <- calc_intra_industry_balance(U = U_mat, V = V_mat, balance = intra_industry_balance) |>
        magrittr::extract2(intra_industry_balance)
    }

    # Check for the case where losses_alloc_mat has only one row.
    # Repeat that row for every industry in V_mat.
    if (nrow(losses_alloc_mat) == 1) {
      # Repeat the row for every industry in V_mat
      industries <- matsbyname::getrownames_byname(V_mat)
      n_industries <- length(industries)
      losses_alloc_mat <- matrix(rep(losses_alloc_mat, n_industries),
                                 nrow = n_industries,
                                 byrow = TRUE,
                                 dimnames = list(industries,
                                                 colnames(losses_alloc_mat))) |>
        matsbyname::setrowtype(matsbyname::rowtype(losses_alloc_mat)) |>
        matsbyname::setcoltype(matsbyname::coltype(losses_alloc_mat))
    }

    # Verify that all industries in V_mat are represented
    # in losses_alloc_mat.
    unique_rows_V_mat <- setdiff(rownames(V_mat), rownames(losses_alloc_mat))
    if (length(unique_rows_V_mat != 0)) {
      msg <- paste0("Industries not same in Recca::endogenize_losses(). ",
                    "These rows are unique to V matrix ",
                    "and should be present (but are not) ",
                    "in the losses allocation matrix: ",
                    paste0(unique_rows_V_mat, collapse = ", "))
      stop(msg)
    }

    # Verify that all rows of the allocation matrix sum to 1 +/- tol
    rowsums_losses_alloc_mat <- matsbyname::rowsums_byname(losses_alloc_mat) - 1
    assertthat::assert_that(matsbyname::iszero_byname(rowsums_losses_alloc_mat, tol = tol),
                            msg = paste("Rows of the losses allocation matrix",
                                        "do not sum to 1 in Recca::endogenize_losses()."))

    # Hatize balance_vec
    balance_vec_hat <- matsbyname::hatize_byname(balance_vec)

    # Multiply balance_vec_hat into losses_alloc_mat
    # to obtain the matrix to be added to V.
    add_to_V <- matsbyname::matrixproduct_byname(balance_vec_hat, losses_alloc_mat)

    # Calculate V_prime
    V_prime_mat <- matsbyname::sum_byname(V_mat, add_to_V)

    # Calculate colsums and transpose
    # to obtain the matrix to be added to Y
    add_to_Y <- add_to_V |>
      matsbyname::transpose_byname() |>
      matsbyname::rowsums_byname(colname = loss_sector)

    # Calculate Y_prime
    Y_prime_mat <- matsbyname::sum_byname(Y_mat, add_to_Y)

    # Verify that inter-industry balance is still observed
    calc_inter_industry_balance(R = R_mat, U = U_mat, V = V_prime_mat, Y = Y_prime_mat) |>
      verify_inter_industry_balance(tol = tol)
    # Verify that endogenizing losses eliminates losses
    calc_intra_industry_balance(U = U_mat, V = V_prime_mat) |>
      verify_intra_industry_balance(tol = tol)

    if (clean) {
      V_prime_mat <- matsbyname::clean_byname(V_prime_mat, tol = tol)
      Y_prime_mat <- matsbyname::clean_byname(Y_prime_mat, tol = tol)
    }

    # Make a list and return
    list(V_prime_mat, Y_prime_mat) |>
      magrittr::set_names(c(V_prime, Y_prime))
  }
  out <- matsindf::matsindf_apply(.sutmats,
                                  FUN = endogenize_func,
                                  R_mat = R,
                                  U_mat = U,
                                  V_mat = V,
                                  Y_mat = Y,
                                  balance_vec = intra_industry_balance,
                                  losses_alloc_mat = losses_alloc)
  if ((is.data.frame(.sutmats) | is.list(.sutmats)) & replace_cols) {
    out <- out |>
      dplyr::mutate(
        "{V}" := .data[[V_prime]],
        "{Y}" := .data[[Y_prime]],
        "{V_prime}" := NULL,
        "{Y_prime}" := NULL,
        "{intra_industry_balance}" := NULL,
        "{losses_alloc}" := NULL
      )
  }
  return(out)
}


