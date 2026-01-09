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
#' @param .sutmats
#' @param R
#' @param U
#' @param V
#' @param Y
#' @param U_feed
#' @param U_eiou
#' @param r_eiou
#' @param intra_industry_balance
#' @param losses_alloc
#' @param loss_sector
#' @param phi_vec
#' @param replace_cols
#' @param clean
#' @param tol
#' @param R_prime
#' @param U_prime
#' @param V_prime
#' @param Y_prime
#' @param U_feed_prime
#' @param U_eiou_prime
#' @param r_eiou_prime
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
  loss_sector = Recca::balance_cols$losses_sector,
  phi_vec = Recca::psut_cols$phi,
  replace_cols = FALSE,
  clean = FALSE,
  tol = 1e-6,
  # Output columns
  R_prime = "R_prime",
  U_prime = "U_prime",
  V_prime = "V_prime",
  Y_prime = "Y_prime",
  U_feed_prime = "U_feed_prime",
  U_eiou_prime = "U_EIOU_prime",
  r_eiou_prime = "r_EIOU_prime") {

  # Calculate losses of the conserved quantity


  # Convert everything to exergy


  # Calculate exergy losses based on the losses column


  # Calculated destruction of exergy


}
