test_that("reconstructing R, U, and V, from single matrices works as expected", {

  alliomats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    calc_io_mats()

  allUV <- new_Y(alliomats, Y_prime = "Y")

  for (i in 1:nrow(allUV)) {
    UV <- new_Y(Y_prime = alliomats$Y[[i]],
                L_ixp = alliomats$L_ixp[[i]],
                L_pxp = alliomats$L_pxp[[i]],
                Z = alliomats$Z[[i]],
                Z_feed = alliomats$Z_feed[[i]],
                D = alliomats$D[[i]],
                O = alliomats$O[[i]])
    expect_equal(UV$R_prime, allUV$R_prime[[i]])
    expect_equal(UV$U_prime, allUV$U_prime[[i]])
    expect_equal(UV$U_feed_prime, allUV$U_feed_prime[[i]])
    expect_equal(UV$U_EIOU_prime, allUV$U_EIOU_prime[[i]])
    expect_equal(UV$r_EIOU_prime, allUV$r_EIOU_prime[[i]])
    expect_equal(UV$V_prime, allUV$V_prime[[i]])
  }
})


test_that("new_Y() works as expected", {
  # Try with Y_prime <- Y, thereby simply trying to duplicate the original U and V matrices
  Reconstructed <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, R, U, U_feed, U_EIOU, r_EIOU, V, Y, S_units) %>%
    calc_io_mats() %>%
    dplyr::mutate(
      Y_prime = Y
    ) %>%
    Recca::new_Y() %>%
    dplyr::mutate(
      # Take the difference between U_prime and U and V_prime and V
      R_diff = matsbyname::difference_byname(R_prime, R),
      U_diff = matsbyname::difference_byname(U_prime, U),
      U_feed_diff = matsbyname::difference_byname(U_feed_prime, U_feed),
      U_eiou_diff = matsbyname::difference_byname(U_EIOU_prime, U_EIOU),
      r_eiou_diff = matsbyname::difference_byname(r_EIOU_prime, r_EIOU),
      V_diff = matsbyname::difference_byname(V_prime, V),
      # The differences should be the 0 matrix, within tolerance
      ROK = matsbyname::iszero_byname(R_diff, tol = 5e-5),
      UOK = matsbyname::iszero_byname(U_diff, tol = 5e-5),
      U_feedOK = matsbyname::iszero_byname(U_feed_diff, tol = 5e-5),
      U_eiouOK = matsbyname::iszero_byname(U_eiou_diff, tol = 5e-5),
      r_eiouOK = matsbyname::iszero_byname(r_eiou_diff, tol = 5e-5),
      VOK = matsbyname::iszero_byname(V_diff, tol = 5e-5)
    )
  expect_true(all(as.logical(Reconstructed$ROK)))
  expect_true(all(as.logical(Reconstructed$UOK)))
  expect_true(all(as.logical(Reconstructed$U_feedOK)))
  expect_true(all(as.logical(Reconstructed$U_eiouOK)))
  expect_true(all(as.logical(Reconstructed$r_eiouOK)))
  expect_true(all(as.logical(Reconstructed$VOK)))

  # Try a list of new Y matrices, each of which contains only final demand for residential lighting.
  Y_prime_finalE <- matrix(6000, nrow = 1, ncol = 1, dimnames = list("Elect [from Grid]", "Residential")) %>%
    matsbyname::setrowtype("Product") %>% matsbyname::setcoltype("Industry")
  Y_prime_usefulE <- matrix(1200, nrow = 1, ncol = 1, dimnames = list("Light", "Residential")) %>%
    matsbyname::setrowtype("Product") %>% matsbyname::setcoltype("Industry")
  Y_prime_servicesE <- matrix(5e14, nrow = 1, ncol = 1, dimnames = list("Illumination [lumen-hrs/yr]", "Residential")) %>%
    matsbyname::setrowtype("Product") %>% matsbyname::setcoltype("Industry")
  Y_prime_servicesX <- matrix(5e14, nrow = 1, ncol = 1, dimnames = list("Illumination [lumen-hrs/yr]", "Residential")) %>%
    matsbyname::setrowtype("Product") %>% matsbyname::setcoltype("Industry")

  Reconstructed_Residential <- Reconstructed %>%
    dplyr::select(-Y_prime, -R_prime, -U_prime, -U_feed_prime, -U_EIOU_prime, -r_EIOU_prime, -V_prime, -R_diff, -U_diff, -V_diff, -ROK, -UOK, -VOK) %>%
    dplyr::mutate(
      Y_prime = list(Y_prime_finalE, Y_prime_servicesE, Y_prime_usefulE, Y_prime_servicesX)
    ) %>%
    new_Y() %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, R_prime, U_prime, V_prime) %>%
    tidyr::gather(key = "matnames", value = "matvals", R_prime, U_prime, V_prime) %>%
    matsindf::expand_to_tidy(drop = 0)

  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "U_prime", rownames == "Crude [from Dist.]", colnames == "Crude dist.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.3481179450)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "V_prime", rownames == "Truck engines", colnames == "MD [from Truck engines]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    7.748625)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "V_prime", rownames == "Gas wells & proc.", colnames == "NG [from Wells]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    16220.3637987185)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "U_prime", rownames == "Elect", colnames == "Elect. grid") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    6238.6014610456)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "R_prime", rownames == "Resources [of NG]", colnames == "NG") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    16356.84944)
  expect_equivalent(Reconstructed_Residential %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "R_prime", rownames == "Resources [of Crude]", colnames == "Crude") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    102.20081)

  # Double Y matrix
  Reconstructed_Double_Y <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, R, U, U_feed, V, Y, r_EIOU, S_units) %>%
    calc_io_mats() %>%
    dplyr::mutate(
      Y_prime = matsbyname::hadamardproduct_byname(Y, 2)
    ) %>%
    new_Y()

  # Testing R_prime matrix:
  Reconstructed_Double_Y %>%
    magrittr::extract2("R_prime") %>%
    matsindf::expand_to_tidy() %>%
    dplyr::filter(rownames == "Resources [of Crude]", colnames == "Crude") %>%
    magrittr::extract2("matvals") %>%
    dplyr::first() %>%
    expect_equal(100000)

  Reconstructed_Double_Y %>%
    magrittr::extract2("R_prime") %>%
    matsindf::expand_to_tidy() %>%
    dplyr::filter(rownames == "Resources [of Crude]", colnames == "Crude") %>%
    magrittr::extract2("matvals") %>%
    dplyr::last() %>%
    expect_equal(53500*2)

  Reconstructed_Double_Y %>%
    magrittr::extract2("R_prime") %>%
    matsindf::expand_to_tidy() %>%
    dplyr::filter(rownames == "Resources [of NG]", colnames == "NG") %>%
    magrittr::extract2("matvals") %>%
    dplyr::first() %>%
    expect_equal(86000)

  # Test all matrices:
  res <- Reconstructed_Double_Y %>%
    dplyr::mutate(
      R_double = matsbyname::hadamardproduct_byname(R, 2),
      U_double = matsbyname::hadamardproduct_byname(U, 2),
      V_double = matsbyname::hadamardproduct_byname(V, 2),
      # Take the difference between primes and doubles
      R_diff = matsbyname::difference_byname(R_double, R_prime),
      U_diff = matsbyname::difference_byname(U_double, U_prime),
      V_diff = matsbyname::difference_byname(V_double, V_prime),
      # Check if it is the 0 matrix
      ROK = matsbyname::iszero_byname(R_diff, tol = 1e-3),
      UOK = matsbyname::iszero_byname(U_diff, tol = 1e-3),
      VOK = matsbyname::iszero_byname(V_diff, tol = 1e-3)
    )

  expect_true(all(as.logical(res$ROK)))
  expect_true(all(as.logical(res$UOK)))
  expect_true(all(as.logical(res$VOK)))


  # Test to define a NULL new Y matrix
  Reconstructed_NULL <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, R, U, U_feed, U_EIOU, r_EIOU, V, Y, S_units) %>%
    calc_io_mats() %>%
    dplyr::mutate(
      Y_prime = matsbyname::select_cols_byname(
        Y,
        "a_pattern_that_does_not_exist_anywhere"
      )
    ) %>%
    Recca::new_Y(
      Y_prime = "Y_prime"
    )

  # Reconstructed_NULL %>%
  #   dplyr::filter(! is.null(R_prime))

  expect_equal(Reconstructed_NULL$Y_prime[[1]], NULL)
  expect_equal(Reconstructed_NULL$R_prime[[1]], NULL)
  expect_equal(Reconstructed_NULL$U_prime[[1]], NULL)
  expect_equal(Reconstructed_NULL$V_prime[[1]], NULL)
  expect_equal(Reconstructed_NULL$V_prime[[2]], NULL)
  expect_equal(Reconstructed_NULL$R_prime[[4]], NULL)
})


test_that("new_k_ps() works as expected", {
  perfectsub_mats <- PerfectSubmats %>%
    tidyr::spread(key = "matrix.name", value = "matrix")

  io_mats <- perfectsub_mats %>% Recca::calc_io_mats()
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
      k_prime = RCLabels::make_list(k_prime_vec, n = 1)
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

  # This test below is part of the R_prime matrix.
  # So we need to change the new_k_ps() matrix before implementing the test.
  # expect_equivalent(new_UV_noR %>%
  #                     dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "V_prime", rownames == "Resources - Rens", colnames == "Rens") %>%
  #                     dplyr::select(matvals) %>%
  #                     unlist(),
  #                   49.75)

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
  # expect_equivalent(new_UV_withR %>%
  #                     dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "R_prime", rownames == "Resources - Rens", colnames == "Rens") %>%
  #                     dplyr::select(matvals) %>%
  #                     unlist(),
  #                   49.75)
})


test_that("1-industry ECC works with new_k_ps()", {
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
  iomats <- calc_io_mats(U = U, U_feed = U, V = V, Y = Y, S_units = S_units)

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


test_that("new_R_ps() works as expected", {
  setup <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "Final" and "Useful" as the Last.stage.
    dplyr::filter(Last.stage != IEATools::last_stages$services) %>%
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats(direction = "downstream") %>%
    # Make an R_prime matrix that gives the same the resource inputs to the economy.
    # For testing purposes!
    dplyr::mutate(
      R_prime = R
    )
  # Now call new_R_ps() which will calculate
  # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
  # given R_prime.
  # Each of the *_prime matrices should be same as their originals,
  # because R_prime is equal to R.
  newRsameasoldR <- setup %>%
    new_R_ps() %>%
    # Clean the rows of U_prime and Y_prime, because they contain Products that are not present in U.
    dplyr::mutate(
      W_prime = matsbyname::difference_byname(matsbyname::transpose_byname(V_prime), U_prime),
      U_prime = matsbyname::clean_byname(U_prime, margin = 1),
      Y_prime = matsbyname::clean_byname(Y_prime, margin = 1),
      W_prime = matsbyname::clean_byname(W_prime, margin = 1)
    ) %>%
    # Set up the expectations
    dplyr::mutate(
      # When R_prime = R, we expect to recover same U, V, and Y.
      expected_U = U,
      expected_V = V,
      expected_Y = Y,
      expected_W = W
    )

  # Test that everything worked as expected
  for (i in 1:2) {
    expect_true(matsbyname::equal_byname(newRsameasoldR$U_prime[[i]], newRsameasoldR$expected_U[[i]], tol = 1e-6))
    expect_true(matsbyname::equal_byname(newRsameasoldR$V_prime[[i]], newRsameasoldR$expected_V[[i]], tol = 1e-6))
    expect_true(matsbyname::equal_byname(newRsameasoldR$Y_prime[[i]], newRsameasoldR$expected_Y[[i]], tol = 1e-6))
    expect_true(matsbyname::equal_byname(newRsameasoldR$W_prime[[i]], newRsameasoldR$expected_W[[i]], tol = 1e-6))
  }

  doubleR <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "Final" and "Useful" as the Last.stage.
    dplyr::filter(Last.stage != IEATools::last_stages$services) %>%
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats(direction = "downstream") %>%
    # Make an R_prime matrix that gives twice the resource inputs to the economy.
    dplyr::mutate(
      R_prime = matsbyname::hadamardproduct_byname(2, R)
    ) %>%
    # Now call the new_R_ps function which will calculate
    # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
    # given R_prime.
    # Each of the *_prime matrices should be 2x their originals,
    # because R_prime is 2x relative to R.
    new_R_ps() %>%
    # Clean the rows of U_prime, because they contain Products that are not present in U.
    dplyr::mutate(
      # Eliminate zero rows or cols that appear after the new_R_ps() call.
      W_prime = matsbyname::difference_byname(matsbyname::transpose_byname(V_prime), U_prime),
      U_prime = matsbyname::clean_byname(U_prime, margin = 1),
      V_prime = matsbyname::clean_byname(V_prime, margin = 2),
      Y_prime = matsbyname::clean_byname(Y_prime, margin = 1),
      W_prime = matsbyname::clean_byname(W_prime, margin = 1)
    ) %>%
    dplyr::mutate(
      expected_U = matsbyname::hadamardproduct_byname(2, U),
      expected_V = matsbyname::hadamardproduct_byname(2, V),
      expected_Y = matsbyname::hadamardproduct_byname(2, Y),
      expected_W = matsbyname::hadamardproduct_byname(2, W)
    )

  # Test that everything worked as expected
  for (i in 1:2) {
    expect_equal(doubleR$U_prime[[i]], doubleR$expected_U[[i]])
    expect_equal(doubleR$V_prime[[i]], doubleR$expected_V[[i]])
    expect_equal(doubleR$Y_prime[[i]], doubleR$expected_Y[[i]])
    expect_equal(doubleR$W_prime[[i]], doubleR$expected_W[[i]])
  }

  # Check some values when the new R matrix has 1's in it.
  # These tests are in preparation for converting new_R_ps()
  # to use new calc_io_mats(direction =- "downstream").
  unitaryR <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "Final" and "Useful" as the Last.stage.
    dplyr::filter(Last.stage != IEATools::last_stages$services) %>%
    dplyr::mutate(
      R_prime = matsbyname::hadamardproduct_byname(2, R)
    )
  unitaryR$R_prime[[1]]["Resources [of Crude]", "Crude"] <- 1
  unitaryR$R_prime[[1]]["Resources [of NG]", "NG"] <- 1
  unitaryR <- unitaryR %>%
    calc_io_mats(direction = "downstream") %>%
    new_R_ps()
  expect_equal(unitaryR$U_prime[[1]]["Crude [from Dist.]", "Oil refineries"], 0.940157103)
  expect_equal(unitaryR$U_prime[[1]]["Elect [from Grid]", "Crude dist."], 0.0005812516)
  expect_equal(unitaryR$V_prime[[1]]["Petrol dist.", "Petrol [from Dist.]"], 0.53022566)
  expect_equal(unitaryR$V_prime[[1]]["Oil refineries", "Diesel"], 0.41017456)
  expect_equal(unitaryR$Y_prime[[1]]["Elect [from Grid]", "Residential"], 0.1395004)
  expect_equal(unitaryR$Y_prime[[1]]["Petrol [from Dist.]", "Transport"], 0.5202214)


  # The tests below fail on UNIX systems in GitHub actions, especially
  # ubuntu-release and ubuntu-oldrel-1.
  # These older systems report
  # "system is computationally singular: reciprocal condition number = 0"
  # when mattrices containing NA values are inverted.
  # So for now (24 Jan 2023), I am
  # disabling these tests.
  # Later (maybe in 2024), the old versions may be
  # pushed out of the testing suite.
  # At that date, I could try to re-enable these tests
  # to see if they work in all ubuntu systems.
  # (Note that the tests work in the ubuntu-latest (devel) test rig!)

  # Test when the units on Products in U are not all same.
  # Under those conditions, we expect that U_prime, V_prime, and Y_prime are all NA.
  # Input units are not all same for the Last.stage = "services" cases.
  # So don't filter out the "services" rows.

  # <<commenting begins>>
  # WithDiffUnits <- UKEnergy2000mats %>%
  #   tidyr::spread(key = "matrix.name", value = "matrix") %>%
  #   # Calculate the input-output matrices which are inputs to the new_R function.
  #   calc_io_mats(direction = "downstream") %>%
  #   # Make an R_prime matrix that gives twice the resource inputs to the economy.
  #   dplyr::mutate(
  #     R_prime = matsbyname::hadamardproduct_byname(2, R)
  #   ) %>%
  #   # Now call the new_R function which will calculate
  #   # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
  #   # given R_prime.
  #   # Each of the *_prime matrices should be 2x their originals,
  #   # because R_prime is 2x relative to R.
  #   new_R_ps()
  #
  # for (i in c(2,4)) {
  #   expect_true(all(is.na(WithDiffUnits$U_prime[[i]])))
  #   expect_true(all(is.na(WithDiffUnits$V_prime[[i]])))
  #   expect_true(all(is.na(WithDiffUnits$Y_prime[[i]])))
  # }
  # <<commenting ends>>
})
