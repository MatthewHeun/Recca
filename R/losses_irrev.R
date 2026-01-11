#' Calculate exergy losses and irreversibility
#'
#' Given a conversion chain quantified in conserved quantities,
#' such as mass or energy,
#' calculate exergy losses and irreversibility
#' (exergy destruction).
#'
#'
#' Additional inputs are the losses allocation matrix
#' (`losses_alloc`) and
#' the phi vector (`phi_vec`).
#' Internally, this function uses
#' [endogenize_losses()] and
#' [extend_to_exergy()].
#' The algorithm for performing these calculations is:
#'
#' - step 1
#' - step 2
#' - step 3
#'
#' @param .sutmats An optional wide-by-matrices data frame
#'                 containing conversion chains quantified
#'                 in conserved quantities (such as mass or energy).
#'                 Losses may be included in the matrices,
#'                 but that is not required.
#' @param R,U,V,Y PSUT matrices that describe the conversion chain.
#' @param intra_industry_balance
#' @param losses_alloc
#' @param loss_sector
#' @param phi_vec
#' @param replace_cols
#' @param clean
#' @param tol
#' @param exergy_loss
#' @param irreversibility
#'
#' @returns
#' @export
#'
#' @examples
calc_exergy_losses_irrev <- function (
  .sutmats = NULL,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  intra_industry_balance = Recca::balance_cols$intra_industry_balance_colname,
  losses_alloc = Recca::balance_cols$losses_alloc_colname,
  loss_sector = Recca::balance_cols$losses_sector,
  phi_vec = Recca::psut_cols$phi,
  replace_cols = FALSE,
  clean = FALSE,
  tol = 1e-6,
  # Output columns
  exergy_loss = Recca::psut_cols$exergy_loss,
  irreversibility = Recca::psut_cols$irreversibility
  ) {

  irrev_func <- function(R_mat, U_mat, V_mat, Y_mat,
                         intra_industry_balance_vec = NULL,
                         losses_alloc_mat = NULL,
                         phi_vector) {

    # Verify that everything is balanced before doing any calculations
    inter_balanced <- verify_inter_industry_balance(
      R = R_mat, U = U_mat, V = V_mat, Y = Y_mat,
      balances = Recca::balance_cols$inter_industry_balance_colname,
      balanced = Recca::balance_cols$inter_industry_balanced_colname,
      tol = tol)
    inter_balanced[[Recca::balance_cols$inter_industry_balanced_colname]] |>
      assertthat::assert_that(msg = paste0("Inter-industry balance not observed in ",
                                           "calc_exergy_losses_irrev(). ",
                                           "No sense calculating exergy losses and ",
                                           "irreversibilities."))

    # Calculate losses of the conserved quantity
    # Maybe set names to be V_losses, Y_losses

    Recca::endogenize_losses(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat,
                             losses_alloc = losses_alloc_mat,
                             loss_sector = loss_sector,
                             clean = FALSE,
                             tol = tol,
                             replace_cols = FALSE,
                             intra_industry_balance = Recca::balance_cols$intra_industry_balance_colname)


    # Convert everything to exergy


    # Calculate exergy losses based on the losses column


    # Calculated destruction of exergy
    # Maybe set names to be V_irreversibility, Y_irreversibility


  }


  out <- matsindf::matsindf_apply(.sutmats,
                                  FUN = irrev_func,
                                  R_mat = R,
                                  U_mat = U,
                                  V_mat = V,
                                  Y_mat = Y,
                                  losses_alloc_mat = losses_alloc,
                                  phi_vector = phi_vec)
}
