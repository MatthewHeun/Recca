test_that("extend_to_exergy() works as expected", {
  # Create a vector of phi values.
  # Each final and useful energy carrier needs a phi value.
  # Use the UKEnergy2000mats data frame for the tests.
  # Get the list of final energy carriers from the matrices in UKEnergy2000mats.

  sutmats <- UKEnergy2000mats %>%
    # Put in wide-by-matrix format.
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Eliminate services ECCs.
    dplyr::filter(Last.stage %in% c("Final", "Useful")) %>%
    dplyr::mutate(
      phi = RCLabels::make_list(Recca::phi_vec, n = nrow(.), lenx = 1)
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

  # Try an erroneous case, when the Energy.type column has something other than E
  sutmats %>%
    dplyr::mutate(
      "{Recca::energy_types$energy_type}" := c("W", "X")
    ) %>%
    extend_to_exergy() %>%
    expect_error("non-energy rows were found:")
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
                0, 2087.519), byrow = TRUE, nrow = 2, ncol = 2,
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
