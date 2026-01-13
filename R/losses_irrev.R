#' Calculate exergy losses and irreversibility
#'
#' Given a conversion chain quantified in conserved quantities,
#' such as mass or energy,
#' calculate exergy losses and irreversibility
#' (exergy destruction)
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
#' @param R Resources (**R**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is [Recca::psut_cols]`$R` or
#'          "`r Recca::psut_cols$R`".
#' @param U Use (**U**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Necessary for verifying calculating losses.
#'          Default is [Recca::psut_cols]`$U` or
#'          "`r Recca::psut_cols$U`".
#' @param V Make (**V**) matrix or name of the column in `.sutmats`
#'          that contains same.
#'          Default is [Recca::psut_cols]`$V` or
#'          "`r Recca::psut_cols$V`".
#' @param Y Final demand (**Y**) matrix or name
#'          of the column in `.sutmats` that contains same.
#'          Default is [Recca::psut_cols]`$Y` or
#'          "`r Recca::psut_cols$Y`".
#' @param U_feed The feedstock portion of the use (**U**) matrix
#'               or name of the column in `.sutmats` that contains same.
#'               Default is [Recca::psut_cols]`$U_feed` or
#'               "`r Recca::psut_cols$U_feed`".
#' @param U_eiou The energy industry own use portion of the use (**U**) matrix
#'               or name of the column in `.sutmats` that contains same.
#'               Default is [Recca::psut_cols]`$U_eiou` or
#'               "`r Recca::psut_cols$U_eiou`".
#' @param r_eiou A matrix of the ratio of energy industry own use
#'               to total use for the use (**U**) matrix
#'               or name of the column in `.sutmats` that contains same.
#'               Default is [Recca::psut_cols]`$r_eiou` or
#'               "`r Recca::psut_cols$r_eiou`".
#' @param intra_industry_balance A vector or the name of the column containing
#'                               intra-industry balance vectors for the conserved quantity.
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
#' @param irrev_alloc An irreversibility allocation matrix or the name of a column in
#'                    `.sutmats` containing same.
#'                    Default is [Recca::balance_cols]`$irrev_alloc_colname` or
#'                    "`r Recca::balance_cols$irrev_alloc_colname`".
#' @param loss_sector The string name of the sector
#'                    that will absorb losses in the **Y** matrix for the conserved quantity.
#'                    Default is [Recca::balance_cols]`$losses_sector`
#'                    or "`r Recca::balance_cols$losses_sector`".
#' @param irrev_sector The string name of the sector that will absorb irreversibilities
#'                     (exergy destruction)
#'                     in the **Y** matrix.
#'                     Default is [Recca::balance_cols]`$irrev_sector`
#'                     or "`r Recca::balance_cols$irrev_sector`".
#' @param phi_vec A vector of exergy-to-energy ratios (phi)
#'                or the string name of a column in `.sutmats` containing same.
#'                Default is [Recca::psut_cols]`$phi` or
#'                "[Recca::psut_cols$phi]".
#' @param replace_cols A boolean that tells whether to
#'                     (a) replace
#'                         the `V` and `Y` columns with
#'                         `V_prime` and `Y_prime` columns, respectively and
#'                     (b) delete the `V_prime`, `Y_prime`, `balance_colname`, and
#'                         `losses_alloc_colname` columns
#'                         after endogenizing the losses
#'                         when `.sutmats` is a data frame or a list.
#'                     Default is `FALSE`.
#' @param clean A boolean that tells whether the outgoing
#'              `V_prime` and `Y_prime` matrices should have
#'              `0` rows and columns removed.
#'              Default is `FALSE`.
#' @param tol The maximum allowable difference from `1` for the rowsums of
#'            loss allocation matrices.
#'            Default is `1e-6`.
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
  U_feed = Recca::psut_cols$U_feed,
  U_eiou = Recca::psut_cols$U_eiou,
  r_eiou = Recca::psut_cols$r_eiou,
  intra_industry_balance = Recca::balance_cols$intra_industry_balance_colname,
  losses_alloc = Recca::balance_cols$losses_alloc_colname,
  irrev_alloc = Recca::balance_cols$irrev_alloc_colname,
  loss_sector = Recca::balance_cols$losses_sector,
  irrev_sector = Recca::balance_cols$irrev_sector,
  phi_vec = Recca::psut_cols$phi,
  replace_cols = FALSE,
  clean = FALSE,
  tol = 1e-6,
  # Output columns
  exergy_loss = Recca::psut_cols$exergy_loss,
  irreversibility = Recca::psut_cols$irreversibility) {

  irrev_func <- function(R_mat, U_mat, V_mat, Y_mat,
                         U_feed_mat, U_eiou_mat, r_eiou_mat,
                         intra_industry_balance_vec = NULL,
                         losses_alloc_mat,
                         irrev_alloc_mat,
                         phi_vector) {

    # Verify that everything is balanced between industries
    # before performing any calculations
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
    V_losses <- "V_losses"
    Y_losses <- "Y_losses"
    heat_losses <- Recca::endogenize_losses(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat,
                                            losses_alloc = losses_alloc_mat,
                                            loss_sector = loss_sector,
                                            clean = FALSE,
                                            tol = tol,
                                            intra_industry_balance = intra_industry_balance_vec,
                                            V_prime = V_losses,
                                            Y_prime = Y_losses)
    V_losses_mat <- heat_losses[[V_losses]]
    Y_losses_mat <- heat_losses[[Y_losses]]

    # Verify that inter-industry balances are preserved
    Recca::verify_inter_industry_balance(R = R_mat, U = U_mat, V = V_losses_mat, Y = Y_losses_mat)

    # Verify all industries are now balanced
    Recca::verify_intra_industry_balance(U = U_mat, V = V_losses_mat)

    # Convert everything to exergy
    exergy_suffix <- "_exergy"
    R_exergy <- "R_exergy"
    U_exergy <- "U_exergy"
    V_exergy <- "V_exergy"
    Y_exergy <- "Y_exergy"
    U_feed_exergy <- "U_feed_exergy"
    U_eiou_exergy <- "U_eiou_exergy"
    r_eiou_exergy <- "r_eiou_exergy"
    exergy_versions <- Recca::extend_to_exergy(R = R_mat, U = U_mat,
                                               V = V_losses_mat, Y = Y_losses_mat,
                                               U_feed = U_feed_mat, U_eiou = U_eiou_mat, r_eiou = r_eiou_mat,
                                               phi = phi_vector,
                                               mat_piece = "noun",
                                               notation = list(RCLabels::from_notation,
                                                               RCLabels::arrow_notation),
                                               .exergy_suffix = exergy_suffix)

    # Verify that inter-industry balances remain
    Recca::verify_inter_industry_balance(R = exergy_versions[[R_exergy]], U = exergy_versions[[U_exergy]],
                                         V = exergy_versions[[V_exergy]], Y = exergy_versions[[Y_exergy]])

    # Calculate exergy losses, which are actually exergy destruction (irreversibility)
    irreversibility <- Recca::endogenize_losses(R = exergy_versions[[R_exergy]],
                                                U = exergy_versions[[U_exergy]],
                                                V = exergy_versions[[V_exergy]],
                                                Y = exergy_versions[[Y_exergy]],
                                                losses_alloc = irrev_alloc_mat,
                                                loss_sector = irrev_sector)


    # Calculated destruction of exergy
    # Maybe set names to be V_irreversibility, Y_irreversibility


  }


  out <- matsindf::matsindf_apply(.sutmats,
                                  FUN = irrev_func,
                                  R_mat = R,
                                  U_mat = U,
                                  V_mat = V,
                                  Y_mat = Y,
                                  U_feed_mat = U_feed,
                                  U_eiou_mat = U_eiou,
                                  r_eiou_mat = r_eiou,
                                  losses_alloc_mat = losses_alloc,
                                  irrev_alloc_mat = irrev_alloc,
                                  phi_vector = phi_vec)
}
