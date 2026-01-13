test_that("calc_exergy_losses_irrev() works as expected", {
  # Create the losses allocation matrix
  losses_alloc_mat <- matrix(1, dimnames = list("All industries",
                                                "MTH.200.C -> Transformation losses")) |>
    matsbyname::setrowtype("Industry") |>
    matsbyname::setcoltype("Product")
  # Create a phi vector
  phi_vec <- matrix(c(1.06, 1.04,    # Crude, NG
                      1.06, 1, 1.06, # Diesel, Elect, Petrol
                      1,             # MD
                      0.143616257,   # LTH (assumed 50 C)
                      0.956,         # Light
                      0.36986157),   # MTH.200.C
                    dimnames = list(c("Crude", "NG",
                                      "Diesel", "Elect", "Petrol",
                                      "MD",
                                      "LTH",
                                      "Light",
                                      "MTH.200.C"),
                                    "phi")) |>
    matsbyname::setrowtype("Product") |>
    matsbyname::setcoltype("phi")

  UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] != IEATools::last_stages$services) |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" := RCLabels::make_list(x = losses_alloc_mat, n = 2,lenx = 1),
      "{Recca::psut_cols$phi}" := RCLabels::make_list(x = phi_vec, n = 2, lenx = 1),
      "{Recca::balance_cols$irrev_alloc_colname}" := RCLabels::make_list(x = Recca::balance_cols$default_destruction_alloc_mat, n = 2, lenx = 1)
    ) |>
    calc_exergy_losses_irrev()
})

