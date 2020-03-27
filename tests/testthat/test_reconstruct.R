###########################################################
context("Reconstructing PSUT matrices from a new Y matrix")
###########################################################

test_that("reconstructing U and V from single matrices works as expected", {
  alliomats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_io_mats()
  allUV <- new_Y(alliomats, Y_prime = "Y")
  for (i in 1:nrow(allUV)) {
    UV <- new_Y(Y_prime = alliomats$Y[[i]],
                L_ixp = alliomats$L_ixp[[i]],
                L_pxp = alliomats$L_pxp[[i]],
                Z = alliomats$Z[[i]],
                D = alliomats$D[[i]])
    expect_equal(UV$U_prime, allUV$U_prime[[i]])
    expect_equal(UV$V_prime, allUV$V_prime[[i]])
  }
})

test_that("reconstructing U and V from a new Y matrix works as expected", {
  # Try with Y_prime <- Y, thereby simply trying to duplicate the original U and V matrices
  Reconstructed <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units) %>%
    calc_io_mats() %>%
    dplyr::mutate(
      Y_prime = Y
    ) %>%
    new_Y() %>%
    dplyr::mutate(
      # Take the difference between U_prime and U and V_prime and V
      U_diff = difference_byname(U_prime, U),
      V_diff = difference_byname(V_prime, V),
      # The differences should be the 0 matrix, within tolerance
      UOK = matsbyname::iszero_byname(U_diff, tol = 5e-5),
      VOK = matsbyname::iszero_byname(V_diff, tol = 5e-5)
    )
  expect_true(all(as.logical(Reconstructed$UOK)))
  expect_true(all(as.logical(Reconstructed$VOK)))


  # Try a list of new Y matrices, each of which contains only the final demand for residential lighting.
  Y_prime_finalE <- matrix(6000, nrow = 1, ncol = 1, dimnames = list("Elect - Grid", "Residential")) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  Y_prime_usefulE <- matrix(1200, nrow = 1, ncol = 1, dimnames = list("Light", "Residential")) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  Y_prime_servicesE <- matrix(5e14, nrow = 1, ncol = 1, dimnames = list("Illumination [lumen-hrs/yr]", "Residential")) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  Y_prime_servicesX <- matrix(5e14, nrow = 1, ncol = 1, dimnames = list("Illumination [lumen-hrs/yr]", "Residential")) %>%
    setrowtype("Product") %>% setcoltype("Industry")

  Reconstructed_Residential <- Reconstructed %>%
    dplyr::select(-Y_prime, -U_prime, -V_prime, -U_diff, -V_diff, -UOK, -VOK) %>%
    dplyr::mutate(
      Y_prime = list(Y_prime_finalE, Y_prime_servicesE, Y_prime_usefulE, Y_prime_servicesX)
    ) %>%
    new_Y() %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, U_prime, V_prime) %>%
    tidyr::gather(key = "matnames", value = "matvals", U_prime, V_prime) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "U_prime", rownames == "Crude - Dist.", colnames == "Crude dist.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.3481179450)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "V_prime", rownames == "Truck engines", colnames == "MD - Truck engines") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    7.748625)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "V_prime", rownames == "Gas wells & proc.", colnames == "NG - Wells") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    16220.3637987185)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "U_prime", rownames == "Elect", colnames == "Elect. grid") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    6238.6014610456)
})


###########################################################
context("New perfectly substitutable inputs in k")
###########################################################

test_that("new_k_ps works as expected", {
  perfectsub_mats <- PerfectSubmats %>%
    tidyr::spread(key = "matrix.name", value = "matrix")

  io_mats <- perfectsub_mats %>% calc_io_mats()
  K <- io_mats$K[[1]]
  expect_equal(K["FF", "FF extraction"], 1)
  expect_equal(K["FF elec", "Buildings"], 0.2725225225)
  expect_equal(K["FF elec", "Electric transport"], 0.0251256281)
  expect_equal(K["Ren elec", "Electric transport"], 0.97487437)
  expect_equal(K["Ren elec", "Solar and wind plants"], 0.03)
  expect_equal(K["Rens", "Solar and wind plants"], 0.97)

  # Figure out a new column vector for k_prime.
  k_prime_vec <- K[, "Electric transport", drop = FALSE]
  k_prime_vec["FF elec", "Electric transport"] <- 0.5
  k_prime_vec["Ren elec", "Electric transport"] <- 0.5
  # Add this vector to the io_mats data frame.
  io_mats <- io_mats %>%
    dplyr::mutate(
      # Set up a new k_prime vector for Electric transport.
      # That vector will be used for the infininte substitution calculation.
      # k_prime = select_cols_byname(K, retain_pattern = make_pattern("Electric transport", pattern_type = "exact")),
      k_prime = matsbyname::make_list(k_prime_vec, n = 1)
    )
  # Now do the calculation of U_prime and V_prime matrices.
  # First test when we don't have an R matrix.
  new_UV_noR <- io_mats %>%
    dplyr::mutate(
      R_plus_V = matsbyname::sum_byname(R, V),
      R = NULL
    ) %>%
    # Use the R_plus_V matrix in place of V
    new_k_ps(V = "R_plus_V")
  # Verify that we didn't make an R_prime column
  expect_null(new_UV_noR[["R_prime"]])


  new_UV_noR %<>%
    dplyr::select(Country, Year, Energy.type, Last.stage, U_prime, V_prime) %>%
    tidyr::gather(key = "matnames", value = "matvals", U_prime, V_prime) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(new_UV_noR %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "U_prime", rownames == "FF elec", colnames == "Buildings") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    12.1)
  expect_equivalent(new_UV_noR %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "V_prime", rownames == "Buildings", colnames == "Bldg services") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    25.2)
  expect_equivalent(new_UV_noR %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "V_prime", rownames == "Resources - Rens", colnames == "Rens") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    49.75)

  # Now test when an R matrix is present.
  new_UV_withR <- io_mats %>%
    new_k_ps() %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, R_prime, U_prime, V_prime) %>%
    tidyr::gather(key = "matnames", value = "matvals", R_prime, U_prime, V_prime) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(new_UV_withR %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "U_prime", rownames == "FF elec", colnames == "Buildings") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    12.1)
  expect_equivalent(new_UV_withR %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "V_prime", rownames == "Buildings", colnames == "Bldg services") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    25.2)
  # This number is no longer found in the V_prime matrix.
  # It is found in the R_prime matrix.
  # So we should get an error with this one.
  expect_equal(new_UV_withR %>%
                 dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "V_prime", rownames == "Resources - Rens", colnames == "Rens") %>%
                 dplyr::select(matvals) %>%
                 unlist() %>%
                 length(),
               0)
  expect_equivalent(new_UV_withR %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "R_prime", rownames == "Resources - Rens", colnames == "Rens") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    49.75)
})


test_that("1-industry ECC works with new_k_ps", {
  # This test arises from interactions with Jianwei Du at University of Texas at Austin.
  # To investigate the issues that Jianwei raised,
  # I'll make the simplest possible ECC that retains the features to be tested,
  # a minimum working example (MWE).
  # This example has two resource industries (R1 and R2),
  # one intermediate industry (I), and
  # two final demand sectors (Y1 and Y2).
  # R1 makes product R1p.  R2 makes product R2p.  I makes product Ip.
  #
  # Here are the U, V, Y, and S_units matrices.

  U <- matrix(c(0, 0, 10,
                0, 0, 10,
                0, 0,  0),
              byrow = TRUE, nrow = 3, ncol = 3,
              dimnames = list(c("R1p", "R2p", "Ip"), c("R1", "R2", "I"))) %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries")

  V <- matrix(c(10,  0, 0,
                0, 10, 0,
                0,  0, 4),
              byrow = TRUE, nrow = 3, ncol = 3,
              dimnames = list(c("R1", "R2", "I"), c("R1p", "R2p", "Ip"))) %>%
    matsbyname::setrowtype("Industries") %>% matsbyname::setcoltype("Products")

  Y <- matrix(c(0, 0,
                0, 0,
                2, 2),
              byrow = TRUE, nrow = 3, ncol = 2,
              dimnames = list(c("R1p", "R2p", "Ip"), c("Y1", "Y2"))) %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries")

  S_units <- matrix(c(1,
                      1,
                      1),
                    byrow = TRUE, nrow = 3, ncol = 1,
                    dimnames = list(c("R1p", "R2p", "Ip"), c("quad"))) %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Units")

  # Now calculate the IO matrices
  iomats <- calc_io_mats(U = U, V = V, Y = Y, S_units = S_units)

  # Recalculate the matrices with updated k column
  new_k <- matrix(c(1,
                    0),
                  byrow = TRUE, nrow = 2, ncol = 1,
                  dimnames = list(c("R1p", "R2p"), c("I"))) %>%
    matsbyname::setrowtype("Products") %>% matsbyname::setcoltype("Industries")
  prime1 <- new_k_ps(c(iomats, list(U = U, V = V, Y = Y, S_units = S_units, k_prime = new_k)))
  expect_equal(prime1$U_prime["R1p", "I"], 20)
  expect_equal(prime1$U_prime["R2p", "I"], 0)
  expect_equal(prime1$V_prime["R1", "R1p"], 20)
  expect_equal(prime1$V_prime["R2", "R2p"], 0)
})


###########################################################
context("New primary industries")
###########################################################

test_that("new_R works as expected", {
  setup <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "Final" and "Useful" as the Last.stage.
    filter(Last.stage != IEATools::last_stages$services) %>%
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats() %>%
    # Calculate the efficiency of every industry in the ECC.
    calc_eta_i() %>%
    # Make an R_prime matrix that gives the same the resource inputs to the economy.
    # For testing purposes!
    mutate(
      R_prime = R
    )
    # Now call the new_R_ps function which will calculate
    # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
    # given R_prime.
    # Each of the *_prime matrices should be same as their originals,
    # because R_prime is equal to R.
  newRsameasoldR <- setup %>%
    new_R_ps() %>%
    # Clean the rows of U_prime and Y_prime, because they contain Products that are not present in U.
    mutate(
      U_prime = clean_byname(U_prime, margin = 1),
      Y_prime = clean_byname(Y_prime, margin = 1)
    ) %>%
    # Set up the expectations
    mutate(
      # When R_prime = R, we expect to recover same U, V, and Y.
      expected_U = U,
      expected_V = V,
      expected_Y = Y
    )

  # Test that everything worked as expected
  for (i in 1:2) {
    expect_true(matsbyname::equal_byname(newRsameasoldR$U_prime[[i]], newRsameasoldR$expected_U[[i]]))
    expect_true(matsbyname::equal_byname(newRsameasoldR$V_prime[[i]], newRsameasoldR$expected_V[[i]]))
    expect_true(matsbyname::equal_byname(newRsameasoldR$Y_prime[[i]], newRsameasoldR$expected_Y[[i]]))
  }

  # Also try when the maxiter argument is set too small.
  expect_warning(setup[1, ] %>% new_R_ps(maxiter = 1), "maxiter = 1 reached without convergence in new_R")

  doubleR <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "Final" and "Useful" as the Last.stage.
    filter(Last.stage != IEATools::last_stages$services) %>%
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats() %>%
    # Calculate the efficiency of every industry in the ECC.
    calc_eta_i() %>%
    # Make an R_prime matrix that gives twice the resource inputs to the economy.
    mutate(
      R_prime = matsbyname::hadamardproduct_byname(2, R)
    ) %>%
    # Now call the new_R function which will calculate
    # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
    # given R_prime.
    # Each of the *_prime matrices should be 2x their originals,
    # because R_prime is 2x relative to R.
    new_R_ps() %>%
    # Clean the rows of U_prime, because they contain Products that are not present in U.
    mutate(
      # Eliminate zero rows or cols that appear after the new_R_ps() call.
      U_prime = matsbyname::clean_byname(U_prime, margin = 1),
      V_prime = matsbyname::clean_byname(V_prime, margin = 2),
      Y_prime = matsbyname::clean_byname(Y_prime, margin = 1)
    ) %>%
    mutate(
      expected_U = matsbyname::hadamardproduct_byname(2, U),
      expected_V = matsbyname::hadamardproduct_byname(2, V),
      expected_Y = matsbyname::hadamardproduct_byname(2, Y)
    )

  # Test that everything worked as expected
  for (i in 1:2) {
    expect_equal(doubleR$U_prime[[i]], doubleR$expected_U[[i]])
    expect_equal(doubleR$V_prime[[i]], doubleR$expected_V[[i]])
    expect_equal(doubleR$Y_prime[[i]], doubleR$expected_Y[[i]])
  }

  # Test when the units on Products in U are not all same.
  # Under those conditions, we expect that U_prime, V_prime, and Y_prime are all NA.
  # Input units are not all same for the Last.stage = "services" cases.
  # So don't filter out the "services" rows.
  WithDiffUnits <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats() %>%
    # Calculate the efficiency of every industry in the ECC.
    calc_eta_i() %>%
    # Make an R_prime matrix that gives twice the resource inputs to the economy.
    mutate(
      R_prime = hadamardproduct_byname(2, R)
    ) %>%
    # Now call the new_R function which will calculate
    # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
    # given R_prime.
    # Each of the *_prime matrices should be 2x their originals,
    # because R_prime is 2x relative to R.
    new_R_ps()
  for (i in c(2,4)) {
    expect_true(is.na(WithDiffUnits$U_prime[[i]]))
    expect_true(is.na(WithDiffUnits$V_prime[[i]]))
    expect_true(is.na(WithDiffUnits$Y_prime[[i]]))
  }
})
