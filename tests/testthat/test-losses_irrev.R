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

  # Establish some expected values.
  R_expected <- matrix(c(53000, 0,
                         0, 44720), byrow = TRUE, nrow = 2,
                       dimnames = list(c("Resources [of Crude]", "Resources [of NG]"),
                                       c("Crude", "NG"))) |>
    matsbyname::setrowtype("Industry") |>
    matsbyname::setcoltype("Product")
  V_trans_losses_col_expected <- c(203.4238635,
                                   129.4515495,
                                   46.23269625,
                                   767.4627578,
                                   18.4930785,
                                   952.3935428,
                                   1877.047468,
                                   277.3961775,
                                   3587.657229) |>
    magrittr::set_names(c("Crude dist.",
                          "Diesel dist.",
                          "Elect. grid",
                          "Gas wells & proc.",
                          "NG dist.",
                          "Oil fields",
                          "Oil refineries",
                          "Petrol dist.",
                          "Power plants"))

  V_destroyed_exergy_col_expected <- c(378.0761365,
                                       241.5484505,
                                       78.76730375,
                                       1390.537242,
                                       33.0069215,
                                       1775.606457,
                                       3497.952532,
                                       517.6038225,
                                       6752.342771) |>
    magrittr::set_names(c("Crude dist.",
                          "Diesel dist.",
                          "Elect. grid",
                          "Gas wells & proc.",
                          "NG dist.",
                          "Oil fields",
                          "Oil refineries",
                          "Petrol dist.",
                          "Power plants"))

  Y_trans_losses_row_expected <- c(0, 0, 7859.558363, 0) |>
    magrittr::set_names(c("Irreversibilities", "Residential",
                          "Transformation losses", "Transport"))

  Y_destoyed_exergy_row_expected <- c(14665.44164, 0, 0, 0) |>
    magrittr::set_names(c("Irreversibilities", "Residential",
                          "Transformation losses", "Transport"))


  # Set up two ways of doing the calculations.
  # One with losses not accounted; the other with losses accounted.
  psut <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] != IEATools::last_stages$services) |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" := RCLabels::make_list(x = losses_alloc_mat, n = 2,lenx = 1),
      "{Recca::psut_cols$phi}" := RCLabels::make_list(x = phi_vec, n = 2, lenx = 1),
      "{Recca::balance_cols$irrev_alloc_colname}" := RCLabels::make_list(x = Recca::balance_cols$default_destruction_alloc_mat, n = 2, lenx = 1))

  res_losses_not_accounted <- psut |>
    extend_to_exergy_with_losses_irrev()

  res_losses_accounted <- psut |>
    endogenize_losses() |>
    dplyr::mutate(
      V = V_prime,
      V_prime = NULL,
      Y = Y_prime,
      Y_prime = NULL
    ) |>
    verify_intra_industry_balance() |>
    extend_to_exergy_with_losses_irrev() |>
    dplyr::mutate(
      SUTIntraIndustryBalanced = NULL
    )

  # Now run the tests
  for (this_res in list(res_losses_not_accounted, res_losses_accounted)) {

    # Ensure that correct columns are obtained in the data frame
    expect_equal(colnames(this_res),
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
    R <- this_res |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                    .data[[Recca::psut_cols$last_stage]] == "Final") |>
      magrittr::extract2("R") |>
      magrittr::extract2(1)
    expect_equal(R, R_expected)

    # V matrix
    V <- this_res |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                    .data[[Recca::psut_cols$last_stage]] == "Final") |>
      magrittr::extract2("V") |>
      magrittr::extract2(1)

    # Transformation losses column
    expect_equal(V[, "MTH.200.C -> Transformation losses"], V_trans_losses_col_expected)

    # Destroyed exergy column
    expect_equal(V[, "Destroyed exergy"], V_destroyed_exergy_col_expected)

    # Y matrix
    Y <- this_res |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                    .data[[Recca::psut_cols$last_stage]] == "Final") |>
      magrittr::extract2("Y") |>
      magrittr::extract2(1)

    # Y Transformation losses row
    expect_equal(Y["MTH.200.C -> Transformation losses", ], Y_trans_losses_row_expected)

    # Y Destroyed exergy row
    expect_equal(Y["Destroyed exergy", ], Y_destoyed_exergy_row_expected)
  }
})




