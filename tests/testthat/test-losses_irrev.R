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

  res <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] != IEATools::last_stages$services) |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" := RCLabels::make_list(x = losses_alloc_mat, n = 2,lenx = 1),
      "{Recca::psut_cols$phi}" := RCLabels::make_list(x = phi_vec, n = 2, lenx = 1),
      "{Recca::balance_cols$irrev_alloc_colname}" := RCLabels::make_list(x = Recca::balance_cols$default_destruction_alloc_mat, n = 2, lenx = 1)
    ) |>
    extend_to_exergy_with_losses_irrev()

  # Ensure that correct columns are obtained
  expect_equal(colnames(res),
               c(Recca::psut_cols$country,
                 Recca::psut_cols$year,
                 Recca::psut_cols$energy_type,
                 Recca::psut_cols$last_stage,
                 Recca::psut_cols$R,
                 Recca::psut_cols$U,
                 Recca::psut_cols$U_eiou,
                 Recca::psut_cols$U_feed,
                 Recca::psut_cols$V,
                 Recca::psut_cols$Y,
                 Recca::psut_cols$r_eiou,
                 Recca::psut_cols$S_units))

  # Spot check values with tests.
  # R matrix with exergy values
  res |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                  .data[[Recca::psut_cols$last_stage]] == "Final") |>
    magrittr::extract2("R") |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(53000, 0,
                          0, 44720), byrow = TRUE, nrow = 2,
                        dimnames = list(c("Resources [of Crude]", "Resources [of NG]"),
                                        c("Crude", "NG"))) |>
                   matsbyname::setrowtype("Industry") |>
                   matsbyname::setcoltype("Product"))


})




