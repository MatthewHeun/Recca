test_that("extend_to_exergy() works as expected", {
  # Create a vector of phi values.
  # Each final and useful energy carrier needs a phi value.
  # Use the UKEnergy2000mats data frame for the tests.
  # Get the list of final energy carriers from the matrices in UKEnergy2000mats.

  sutmats <- UKEnergy2000mats |>
    # Put in wide-by-matrix format.
    tidyr::spread(key = matrix.name, value = matrix) |>
    # Eliminate services ECCs.
    dplyr::filter(LastStage %in% c("Final", "Useful")) |>
    dplyr::mutate(
      phi = RCLabels::make_list(Recca::phi_vec, n = dplyr::n(), lenx = 1)
    )
  res <- extend_to_exergy(sutmats)
  expect_true(Recca::energy_types$x %in% res[[Recca::energy_types$energy_type]] %>% unique())

  # Check a couple values

  # R matrix
  energy_val <- res[[Recca::psut_cols$R]][[1]]["Resources [of Crude]", "Crude"]
  exergy_val <- res[[Recca::psut_cols$R]][[3]]["Resources [of Crude]", "Crude"]
  phi <- Recca::phi_vec["Crude", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$R]][[1]]["Resources [of NG]", "NG"]
  exergy_val <- res[[Recca::psut_cols$R]][[3]]["Resources [of NG]", "NG"]
  phi <- Recca::phi_vec["NG", ]

  expect_true((res[[Recca::psut_cols$R]] %>% matsbyname::rowtype() == "Industry") %>% all())
  expect_true((res[[Recca::psut_cols$R]] %>% matsbyname::coltype() == "Product") %>% all())

  # U matrix
  energy_val <- res[[Recca::psut_cols$U]][[1]]["Diesel [from Dist.]", "NG dist."]
  exergy_val <- res[[Recca::psut_cols$U]][[3]]["Diesel [from Dist.]", "NG dist."]
  phi <- Recca::phi_vec["Diesel [from Dist.]", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$U_feed]][[2]]["Elect [from Grid]", "Light fixtures"]
  exergy_val <- res[[Recca::psut_cols$U_feed]][[4]]["Elect [from Grid]", "Light fixtures"]
  phi <- Recca::phi_vec["Elect [from Grid]", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$U_eiou]][[2]]["Diesel [from Dist.]", "Gas wells & proc."]
  exergy_val <- res[[Recca::psut_cols$U_eiou]][[4]]["Diesel [from Dist.]", "Gas wells & proc."]
  phi <- Recca::phi_vec["Diesel [from Dist.]", ]
  expect_equal(energy_val*phi, exergy_val)

  expect_true((res[[Recca::psut_cols$U]] %>% matsbyname::rowtype() == "Product") %>% all())
  expect_true((res[[Recca::psut_cols$U]] %>% matsbyname::coltype() == "Industry") %>% all())

  # V matrix
  energy_val <- res[[Recca::psut_cols$V]][[1]]["Power plants", "Elect"]
  exergy_val <- res[[Recca::psut_cols$V]][[3]]["Power plants", "Elect"]
  phi <- Recca::phi_vec["Elect", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$V]][[2]]["Furnaces", "LTH"]
  exergy_val <- res[[Recca::psut_cols$V]][[4]]["Furnaces", "LTH"]
  phi <- Recca::phi_vec["LTH", ]
  expect_equal(energy_val*phi, exergy_val)

  expect_true((res[[Recca::psut_cols$V]] %>% matsbyname::rowtype() == "Industry") %>% all())
  expect_true((res[[Recca::psut_cols$V]] %>% matsbyname::coltype() == "Product") %>% all())

  # Y matrix
  energy_val <- res[[Recca::psut_cols$Y]][[2]]["Light", "Residential"]
  exergy_val <- res[[Recca::psut_cols$Y]][[4]]["Light", "Residential"]
  phi <- Recca::phi_vec["Light", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$Y]][[2]]["LTH", "Transport"]
  exergy_val <- res[[Recca::psut_cols$Y]][[4]]["LTH", "Transport"]
  phi <- Recca::phi_vec["LTH", ]
  expect_equal(energy_val*phi, exergy_val)

  expect_true((res[[Recca::psut_cols$Y]] %>% matsbyname::rowtype() == "Product") %>% all())
  expect_true((res[[Recca::psut_cols$Y]] %>% matsbyname::coltype() == "Industry") %>% all())
})


test_that("extend_to_exergy() works correctly with specified products", {
  # Make an example R matrix
  R <- matrix(c(250.129, 0,
                0, 2087.513), byrow = TRUE, nrow = 2, ncol = 2,
              dimnames = list(c("Resources [of Hydro]",
                                "Resources [of Primary solid biofuels]"),
                              c("Hydro [from Resources]", "Primary solid biofuels [from Resources]"))) %>%
    matsbyname::setrowtype("Industry") %>% matsbyname::setcoltype("Product")

  # Create an example U matrices
  U <- matrix(c(250.129, 0,
                0, 2087.513), byrow = TRUE, nrow = 2, ncol = 2,
              dimnames = list(c("Hydro [from Resources]",
                                "Primary solid biofuels [from Resources]"),
                              c("Manufacture [of Hydro]", "Manufacture [of Primary solid biofuels]"))) %>%
    matsbyname::setrowtype("Product") %>% matsbyname::setcoltype("Industry")

  U_feed <- U
  U_eiou <- matsbyname::hadamardproduct_byname(U_feed, 0.1)
  r_eiou <- matsbyname::quotient_byname(U_eiou, U) %>%
    matsbyname::replaceNaN_byname()

  # Create an example V matrix
  V <- matrix(c(250.129, 0,
                0, 2087.513), byrow = TRUE, nrow = 2, ncol = 2,
              dimnames = list(c("Manufacture [of Hydro]",
                                "Manufacture [of Primary solid biofuels]"),
                              c("Hydro", "Primary solid biofuels"))) %>%
    matsbyname::setrowtype("Industry") %>% matsbyname::setcoltype("Product")

  # Create an example Y matrix
  Y <- matrix(c(250.129, 0,
                0, 2087.513), byrow = TRUE, nrow = 2, ncol = 2,
              dimnames = list(c("Hydro", "Primary solid biofuels"),
                              c("Exports [of Hydro]", "Exports [of Primary solid biofuels]"))) %>%
    matsbyname::setrowtype("Product") %>% matsbyname::setcoltype("Industry")

  # Create an example phi vector
  phi <- matrix(c(42,
                  1,
                  1.11,
                  43), ncol = 1,
                dimnames = list(c("Bogus1",
                                  "Hydro",
                                  "Primary solid biofuels",
                                  "Bogus2"),
                                "phi")) %>%
    matsbyname::setrowtype("Product") %>% matsbyname::setcoltype("phi")

  # Call extend_to_exergy() with the example matrices.
  # Expect no warning.
  res <- extend_to_exergy(R = R, U = U, U_feed = U_feed, U_eiou = U_eiou,
                          V = V, Y = Y, phi = phi,
                          mat_piece = "noun")

  phi_mat <- matrix(c(1, 0,
                      0, 1.11), byrow = TRUE, nrow = 2, ncol = 2)

  expected_R_X <- R %*% phi_mat
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(res$R_exergy[[i, j]], expected_R_X[[i, j]])
    }
  }

  expected_U_X <- phi_mat %*% U
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(res$U_exergy[[i, j]], expected_U_X[[i, j]])
    }
  }

  expected_U_feed_X <- phi_mat %*% U_feed
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(res$U_feed_exergy[[i, j]], expected_U_feed_X[[i, j]])
    }
  }

  expected_U_EIOU_X <- phi_mat %*% U_eiou
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(res$U_EIOU_exergy[[i, j]], expected_U_EIOU_X[[i, j]])
    }
  }

  expected_V_X <- V %*% phi_mat
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(res$V_exergy[[i, j]], expected_V_X[[i, j]])
    }
  }

  expected_Y_X <- phi_mat %*% Y
  for (i in 1:2) {
    for (j in 1:2) {
      expect_equal(res$Y_exergy[[i, j]], expected_Y_X[[i, j]])
    }
  }

})


test_that("extend_fu_details_to_exergy() works as expected", {
  details_mat <- Matrix::sparseMatrix(i = c(1, 2, 3),
                                      j = c(1, 3, 2),
                                      x = c(10, 20, 100),
                                      dimnames = list(c("Electricity -> Households",
                                                        "Electricity -> Industry",
                                                        "Natural gas -> Households"),
                                                      c("Light [from Electric lamps]",
                                                        "MTH.100.C [from Furnaces]",
                                                        "KE [from Fans]"))) |>
    matsbyname::setrowtype("Product -> Industry") |>
    matsbyname::setcoltype("Product [from Industry]")
  phi_vec <- Matrix::sparseMatrix(i = c(1, 2, 3, 4),
                                  j = c(1, 1, 1, 1),
                                  x = c(1.0, 1-(25+273.15)/(100+273.15), 0.96, 1-(25+273.15)/(1000+273.15)),
                                  dimnames = list(c("KE", "MTH.100.C", "Light", "HTH.1000.C"),
                                                  "phi")) |>
    matsbyname::setrowtype("Product") |>
    matsbyname::setcoltype("phi")
  expected <- details_mat
  expected[1,1] <- 10*0.96 # Light
  expected[2,3] <- 20 # No change for KE
  expected[3,2] <- 100 * (1-(25+273.15)/(100+273.15))
  expected <- expected |>
    matsbyname::setrowtype("Product -> Industry") |>
    matsbyname::setcoltype("Product [from Industry]")
  res <- extend_fu_details_to_exergy(Y_fu_details = details_mat,
                                     U_eiou_fu_details = details_mat,
                                     phi = phi_vec)
  expect_true(matsbyname::equal_byname(res$Y_fu_details_exergy, expected))
  expect_true(matsbyname::equal_byname(res$U_EIOU_fu_details_exergy, expected))

  # Make a data frame and do calculations within.
  df <- tibble::tibble(Country = "USA",
                       EnergyType = "E",
                       Y_fu_details = list(details_mat, details_mat),
                       U_EIOU_fu_details = list(details_mat, details_mat),
                       phi = list(phi_vec, phi_vec))
  res_df <- df |>
    extend_fu_details_to_exergy()
  # Test that the results are as expected.
  res_df |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2("Y_fu_details") |>
    matsbyname::equal_byname(list(expected, expected)) |>
    unlist() |>
    all() |>
    expect_true()
  res_df |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2("U_EIOU_fu_details") |>
    matsbyname::equal_byname(list(expected, expected)) |>
    unlist() |>
    all() |>
    expect_true()
})


test_that("extend_fu_details_to_exergy() works with NULL matrices", {
  extend_fu_details_to_exergy(Y_fu_details = NULL,
                              U_eiou_fu_details = NULL,
                              phi = NULL) |>
    expect_equal(list(Y_fu_details_mat = list(),
                      U_eiou_fu_details_mat = list(),
                      phi_vec = list()))
})


test_that("extend_fu_details_to_exergy() works with a data frame without the details matrices", {
  no_details_df <- tibble::tribble(~R, ~U, ~V, ~Y,
                                   1,  2,  3,  4)
  # A data frame without the details matrices should return NULL.
  expect_null(extend_fu_details_to_exergy(no_details_df))
})


test_that("extend_fu_details_to_exergy() fails when not all EnergyType is 'E'", {
  details_df <- tibble::tribble(~EnergyType, ~R, ~U, ~V, ~Y, ~Y_fu_details, ~U_EIOU_fu_details,
                                "X",          1,  2,  3,  4,    5,             6)
  # A data frame without the details matrices should return NULL.
  extend_fu_details_to_exergy(details_df) |>
    expect_error(regexp = "non-energy rows were found")
})


test_that("extend_fu_details_to_exergy() gives NULL when matrices are NULL", {
  details_mat <- Matrix::sparseMatrix(i = c(1, 2, 3),
                                      j = c(1, 3, 2),
                                      x = c(10, 20, 100),
                                      dimnames = list(c("Electricity -> Households",
                                                        "Electricity -> Industry",
                                                        "Natural gas -> Households"),
                                                      c("Light [from Electric lamps]",
                                                        "MTH.100.C [from Furnaces]",
                                                        "KE [from Fans]"))) |>
    matsbyname::setrowtype("Product -> Industry") |>
    matsbyname::setcoltype("Product [from Industry]")
  phi_vec <- Matrix::sparseMatrix(i = c(1, 2, 3, 4),
                                  j = c(1, 1, 1, 1),
                                  x = c(1.0, 1-(25+273.15)/(100+273.15), 0.96, 1-(25+273.15)/(1000+273.15)),
                                  dimnames = list(c("KE", "MTH.100.C", "Light", "HTH.1000.C"),
                                                  "phi")) |>
    matsbyname::setrowtype("Product") |>
    matsbyname::setcoltype("phi")

  Y_fu_details <- details_mat
  U_EIOU_fu_details <- details_mat
  details_df <- tibble::tribble(~EnergyType, ~phi,    ~R, ~U, ~V, ~Y, ~Y_fu_details, ~U_EIOU_fu_details,
                                "E",          phi_vec, 1,  1,  1,  1,  NULL,          U_EIOU_fu_details,
                                "E",          phi_vec, 2,  2,  2,  2,  Y_fu_details,  NULL)
  # A data frame without the details matrices should return NULL.
  res <- extend_fu_details_to_exergy(details_df)

  res |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X", R == 1) |>
    magrittr::extract2("Y_fu_details") |>
    magrittr::extract2(1) |>
    expect_null()
  res |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X", R == 2) |>
    magrittr::extract2("U_EIOU_details") |>
    magrittr::extract2(1) |>
    expect_null()
})


test_that("extend_to_exergy() works with NULL U_EIOU_mat" , {
  sutmats_with_null_U_EIOU <- UKEnergy2000mats %>%
    # Put in wide-by-matrix format.
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Eliminate services ECCs.
    dplyr::filter(LastStage %in% c("Final", "Useful")) %>%
    dplyr::mutate(
      phi = RCLabels::make_list(Recca::phi_vec, n = dplyr::n(), lenx = 1),
      "{IEATools::psut_cols$U_eiou}" := list(NULL, NULL),
      "{IEATools::psut_cols$U_feed}" := .data[[IEATools::psut_cols$U]]
    )
  expect_equal(sutmats_with_null_U_EIOU[[IEATools::psut_cols$U]],
               sutmats_with_null_U_EIOU[[IEATools::psut_cols$U_feed]])
  with_exergy <- sutmats_with_null_U_EIOU |>
    extend_to_exergy() |>
    dplyr::filter(.data[[IEATools::iea_cols$energy_type]] == IEATools::energy_types$x)
  expect_equal(matsbyname::iszero_byname(with_exergy[[IEATools::psut_cols$U_eiou]]),
               list(TRUE, TRUE))
})


test_that("extend_to_exergy() works when endogenizing losses and calculating irreversibilities", {
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

  # Use the internal calculation of losses by not supplying losses
  # in the incoming data frame
  res_losses_not_accounted <- psut |>
    extend_to_exergy(endogenize_losses_irrev = TRUE,
                     clean = TRUE,
                     mat_piece = "noun",
                     notation = list(RCLabels::from_notation,
                                     RCLabels::arrow_notation))

  # Calculate losses and irreversibility prior to calling the
  # function.
  res_losses_accounted <- psut |>
    endogenize_losses() |>
    dplyr::mutate(
      V = V_prime,
      V_prime = NULL,
      Y = Y_prime,
      Y_prime = NULL
    ) |>
    verify_intra_industry_balance() |>
    extend_to_exergy(endogenize_losses_irrev = TRUE,
                     clean = TRUE,
                     mat_piece = "noun",
                     notation = list(RCLabels::from_notation,
                                     RCLabels::arrow_notation)) |>
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
