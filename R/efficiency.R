# This file contains functions that calculate direct efficiencies for an ECC.


calc_eta <- function(.sutdata,
                     # Input columns
                     g_colname = "g", U_colname = "U", S_units_colname = "S_units",
                     # Output columns
                     eta_colname = "eta"){
  eta_func <- function(g, U, S_units){

    U_bar <- matrixproduct_byname(transpose_byname(S_units), U)
    if (nrow(U_bar) > 1) {
      stop("")
    }
    iU_bar_hat <- hatize_byname(colsums_byname(U_bar))
    eta <- 1
  }


  matsindf_apply(.sutdata, FUN = eta_func, g = g_colname, U = U_colname, S_units = S_units_colname)
}
