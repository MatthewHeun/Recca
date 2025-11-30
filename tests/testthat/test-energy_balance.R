
test_that("verify_SUT_energy_balance() works with energy only", {
  # This test will need to be deleted after removing verify_SUT_energy_balance()
  spread <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in% c(IEATools::last_stages$final, IEATools::last_stages$useful))
   spread %>%
    verify_SUT_energy_balance() |>
    expect_silent()
  # Try with missing R matrix
  UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    ) %>%
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
    verify_SUT_energy_balance() |>
    expect_silent()

  # Now test with single matrices
  verify_SUT_energy_balance(R = spread$R[[1]],
                            U = spread$U[[1]],
                            V = spread$V[[1]],
                            Y = spread$Y[[1]]) |>
    unlist() |>
    expect_true()
})


test_that("SUT matrix energy balance works with energy only", {
  UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    calc_inter_industry_balance() |>
    verify_inter_industry_balance() |>
    expect_silent()
  # Try with missing R matrix
  UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    ) %>%
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
    calc_inter_industry_balance() |>
    verify_inter_industry_balance() |>
    expect_silent()
})


test_that("SUT matrix energy balance fails when a number has changed", {
  mats <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] == IEATools::last_stages$final)
  R <- mats$R[[1]]
  U <- mats$U[[1]]
  V <- mats$V[[1]]
  Y <- mats$Y[[1]]
  # expect_equal(verify_SUT_energy_balance(R = R, U = U, V = V, Y = Y),
  #              list(.SUT_energy_balance = TRUE))
  calc_inter_industry_balance(R = R, U = U, V = V, Y = Y) |>
    verify_inter_industry_balance() |>
    magrittr::extract2(2) |>
    expect_equal(TRUE)

  Y[2, 2] <- 42 # Replace a 0 with a value
  # expect_warning(verify_SUT_energy_balance(R = R, U = U, V = V, Y = Y),
  #                "Energy not conserved")
  calc_inter_industry_balance(R = R, U = U, V = V, Y = Y) |>
    verify_inter_industry_balance() |>
    magrittr::extract2(2) |>
    expect_equal(FALSE) |>
    expect_warning()
})


test_that("SUT matrix energy balance with units works as expected", {
  result <- verify_SUT_energy_balance_with_units(
    UKEnergy2000mats %>%
      tidyr::spread(key = matrix.name, value = matrix),
    tol = 1e-3)
  expect_true(all(result$.SUT_prod_energy_balanced %>% as.logical()))
  expect_true(all(result$.SUT_ind_energy_balanced %>% as.logical()))

  # Now try when R is gone.
  result_noR <- verify_SUT_energy_balance_with_units(
    UKEnergy2000mats %>%
      tidyr::spread(key = matrix.name, value = matrix) %>%
      dplyr::mutate(
        V = matsbyname::sum_byname(R, V),
        R = NULL
      ),
    tol = 1e-3)
  expect_true(all(result_noR$.SUT_prod_energy_balanced %>% as.logical()))
  expect_true(all(result_noR$.SUT_ind_energy_balanced %>% as.logical()))
})


test_that("SUT matrix energy balance fails as expected when out of balance", {
  UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = "matrix.name",
                       values_from = "matrix") |>
    verify_SUT_energy_balance_with_units() |>
    expect_error(regexp = "Energy not conserved by product in verify_SUT_energy_balance_with_units")
})


test_that("all industries are producing energy", {
  UKspread <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name,
                       values_from = matrix)
  UKspread |>
    calc_inter_industry_balance() |>
    verify_inter_industry_balance(tol = 1e-4) |>
    expect_silent()

  expect_silent(verify_SUT_industry_production(UKspread))
  result <- verify_SUT_industry_production(UKspread)
  expect_true(all(result[[".industry_production_OK"]] %>% as.logical()))

  # Try it when something doesn't produce energy
  R <- UKspread$R[[1]]
  U <- UKspread$U[[1]]
  V <- UKspread$V[[1]]
  expect_equal(V["Crude dist.", "Crude [from Dist.]"], 47500)
  V["Crude dist.", "Crude [from Dist.]"] <- 0 # Zero out production of Crude - Dist. from Crude dist.
  expect_warning(result <- verify_SUT_industry_production(R = R, U = U, V = V),
                 "There are some industries that consume but do not produce energy.")
  expect_false(result$.industry_production_OK)
  expect_equal(expected = "Crude dist.", result$.problem_industries)

  # Try when there is no R matrix
  UKspread_noR <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    )
  expect_silent(verify_SUT_industry_production(UKspread_noR))
  result_noR <- verify_SUT_industry_production(UKspread_noR)
  expect_true(all(result_noR[[".industry_production_OK"]] %>% as.logical()))
})


test_that("calc_inter_industry_balance() works with single matrices", {
  UKspread <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
  R <- UKspread$R[[1]]
  U <- UKspread$U[[1]]
  V <- UKspread$V[[1]]
  Y <- UKspread$Y[[1]]
  # expect_silent(verify_SUT_energy_balance(R = R, U = U, V = V, Y = Y))
  calc_inter_industry_balance(R = R, U = U, V = V, Y = Y) |>
    verify_inter_industry_balance() |>
    expect_silent()
})


test_that("IEA energy balance works correctly", {
  # Make sure that it works.
  expect_silent(
    UKEnergy2000tidy %>%
      dplyr::group_by(Country, Year, EnergyType, LastStage) %>%
      # On Matt's new M1 Pro MacBook Pro, this test has small errors in energy differences.
      # Not sure if that is due to the processor being less precise?
      # Anyway, setting tol = 0.2 so that the results can slip under that level and prevent
      # errors in tests.
      # Note that is 0.2 out of 1e14.  So not a big deal.
      verify_IEATable_energy_balance(energy = IEATools::iea_cols$e_dot, tol = 0.2)
  )

  # Introduce something to make the energy balance fail.
  Unbalanced <- UKEnergy2000tidy
  # Change from 5e4 to 1e4
  Unbalanced[[IEATools::iea_cols$e_dot]][[1]] <- 1e4
  # Now try energy balance. It should fail.
  expect_error(Unbalanced %>%
                 dplyr::group_by(Country, Year, EnergyType, LastStage) %>%
                 verify_IEATable_energy_balance(energy = IEATools::iea_cols$e_dot),
                 "Energy not balanced in verify_IEATable_energy_balance.")
})


test_that("calc_inter_industry_balance() works correctly", {
  UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
                    c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
    calc_inter_industry_balance() |>
    verify_inter_industry_balance() |>
    # Should not throw an error or warning
    expect_silent()
})


test_that("calc_intra_industry_balance() works correctly", {
  res <- UKEnergy2000mats |>
    dplyr::filter(LastStage %in% c("Final", "Useful")) |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    calc_intra_industry_balance()
  expected_1 <- matrix(c(550,
                         350,
                         125,
                         2075,
                         50,
                         2575,
                         5075,
                         750,
                         9700),
                       ncol = 1,
                       dimnames = list(c("Crude dist.",
                                         "Diesel dist.",
                                         "Elect. grid",
                                         "Gas wells & proc.",
                                         "NG dist.",
                                         "Oil fields",
                                         "Oil refineries",
                                         "Petrol dist.",
                                         "Power plants"),
                                       "Product")) |>
    matsbyname::setrowtype("Industry") |>
    matsbyname::setcoltype("Product")
  expect_equal(res$SUTIntraIndustryBalance[[1]], expected_1)

  expected_2 <- matrix(c(22999.6,
                         545,
                         367.9998,
                         125,
                         5000,
                         2075,
                         4800,
                         45,
                         2575,
                         5075,
                         526.9997,
                         9700,
                         13250.02),
                       ncol = 1,
                       dimnames = list(c("Car engines",
                                         "Crude dist.",
                                         "Diesel dist.",
                                         "Elect. grid",
                                         "Furnaces",
                                         "Gas wells & proc.",
                                         "Light fixtures",
                                         "NG dist.",
                                         "Oil fields",
                                         "Oil refineries",
                                         "Petrol dist.",
                                         "Power plants",
                                         "Truck engines"),
                                       "Product")) |>
    matsbyname::setrowtype("Industry") |>
    matsbyname::setcoltype("Product")
  expect_equal(res$SUTIntraIndustryBalance[[2]], expected_2)
})


test_that("verify_intra_industry_balance()", {
  res <- UKEnergy2000mats |>
    dplyr::filter(LastStage %in% c("Final", "Useful")) |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    calc_intra_industry_balance() |>
    verify_intra_industry_balance() |>
    expect_warning(regexp = "Industries are not balanced")


  expect_warning(
    res <- UKEnergy2000mats |>
      dplyr::filter(LastStage %in% c("Final", "Useful")) |>
      tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
      calc_intra_industry_balance() |>
      verify_intra_industry_balance(),
    regexp = "Industries are not balanced"
  )
  expect_equal(res$SUTIntraIndustryBalanced, c(FALSE, FALSE))
})


test_that("endogenize_losses() works correctly", {
  UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
                    c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
    endogenize_losses()

})

