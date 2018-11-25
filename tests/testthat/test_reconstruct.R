library(dplyr)
library(Hmisc)
library(matsbyname)
library(matsindf)
library(Recca)
library(testthat)
library(tidyr)


###########################################################
context("Reconstructing PSUT matrices from a new Y matrix")
###########################################################

test_that("reconstructing U and V from single matrices works as expected", {
  alliomats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_io_mats()
  allUV <- new_Y(alliomats, Y_prime_colname = "Y")
  for (i in 1:nrow(allUV)) {
    UV <- new_Y(Y_prime_colname = alliomats$Y[[i]],
                L_ixp_colname = alliomats$L_ixp[[i]],
                L_pxp_colname = alliomats$L_pxp[[i]],
                Z_colname = alliomats$Z[[i]],
                D_colname = alliomats$D[[i]])
    expect_equal(UV$U_prime, allUV$U_prime[[i]])
    expect_equal(UV$V_prime, allUV$V_prime[[i]])
  }
})

test_that("reconstructing U and V from a new Y matrix works as expected", {
  # Try with Y_prime <- Y, thereby simply trying to duplicate the original U and V matrices
  Reconstructed <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units) %>%
    calc_io_mats() %>%
    mutate(
      Y_prime = Y
    ) %>%
    new_Y() %>%
    mutate(
      # Take the difference between U_prime and U and V_prime and V
      U_diff = difference_byname(U_prime, U),
      V_diff = difference_byname(V_prime, V),
      # The differences should be the 0 matrix, within tolerance
      UOK = iszero_byname(U_diff, tol = 5e-5),
      VOK = iszero_byname(V_diff, tol = 5e-5)
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
    select(-Y_prime, -U_prime, -V_prime, -U_diff, -V_diff, -UOK, -VOK) %>%
    mutate(
      Y_prime = list(Y_prime_finalE, Y_prime_servicesE, Y_prime_usefulE, Y_prime_servicesX)
    ) %>%
    new_Y() %>%
    select(Country, Year, Energy.type, Last.stage, U_prime, V_prime) %>%
    gather(key = "matnames", value = "matvals", U_prime, V_prime) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(Reconstructed_Residential %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", matnames == "U_prime", rownames == "Crude - Dist.", colnames == "Crude dist.") %>% select(matvals) %>% unlist(),
                    0.3481179450)
  expect_equivalent(Reconstructed_Residential %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "useful", matnames == "V_prime", rownames == "Truck engines", colnames == "MD - Truck engines") %>% select(matvals) %>% unlist(),
                    7.748625)
  expect_equivalent(Reconstructed_Residential %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "V_prime", rownames == "Gas wells & proc.", colnames == "NG - Wells") %>% select(matvals) %>% unlist(),
                    16220.3637987185)
  expect_equivalent(Reconstructed_Residential %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "U_prime", rownames == "Elect", colnames == "Elect. grid") %>% select(matvals) %>% unlist(),
                    6238.6014610456)
})


###########################################################
context("New perfectly substitutable inputs in k")
###########################################################

test_that("new_k_ps works as expected", {
  perfectsub_mats <- PerfectSubmats %>%
    spread(key = "matrix.name", value = "matrix")

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
    mutate(
      # Set up a new k_prime vector for Electric transport.
      # That vector will be used for the infininte substitution calculation.
      # k_prime = select_cols_byname(K, retain_pattern = make_pattern("Electric transport", pattern_type = "exact")),
      k_prime = make_list(k_prime_vec, n = 1)
    )
  # Now do the calculation of U_prime and V_prime matrices.
  new_UV <- io_mats %>%
    new_k_ps() %>%
    select(Country, Year, Energy.type, Last.stage, U_prime, V_prime) %>%
    gather(key = "matnames", value = "matvals", U_prime, V_prime) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(new_UV %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", matnames == "U_prime", rownames == "FF elec", colnames == "Buildings") %>% select(matvals) %>% unlist(),
                    12.1)
  expect_equivalent(new_UV %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", matnames == "V_prime", rownames == "Buildings", colnames == "Bldg services") %>% select(matvals) %>% unlist(),
                    25.2)
  expect_equivalent(new_UV %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", matnames == "V_prime", rownames == "Resources - Rens", colnames == "Rens") %>% select(matvals) %>% unlist(),
                    49.75)
})


###########################################################
context("New primary industries")
###########################################################

test_that("new_R works as expected", {
  newRsameasoldR <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "final" and "useful" as the Last.stage.
    filter(Last.stage != "services") %>%
    # At present, UKEnergy2000mats has V matrices that are the sum of both V and R.
    # Change to use the R matrix.
    rename(
      R_plus_V = V
    ) %>%
    separate_RV() %>%
    # At this point, the matrices are they way we want them.
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats() %>%
    # Calculate the efficiency of every industry in the ECC.
    calc_eta_i() %>%
    # Make an R_prime matrix that gives the same the resource inputs to the economy.
    # For testing purposes!
    mutate(
      R_prime = R
    ) %>%
    # Now call the new_R_ps function which will calculate
    # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
    # given R_prime.
    # Each of the *_prime matrices should be same as their originals,
    # because R_prime is equal to R.
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
    expect_true(equal_byname(newRsameasoldR$U_prime[[i]], newRsameasoldR$expected_U[[i]]))
    expect_true(equal_byname(newRsameasoldR$V_prime[[i]], newRsameasoldR$expected_V[[i]]))
    expect_true(equal_byname(newRsameasoldR$Y_prime[[i]], newRsameasoldR$expected_Y[[i]]))
    # expect_equivalent(newRsameasoldR$U_prime[[i]], newRsameasoldR$expected_U[[i]])
    # expect_equivalent(newRsameasoldR$V_prime[[i]], newRsameasoldR$expected_V[[i]])
    # expect_equivalent(newRsameasoldR$Y_prime[[i]], newRsameasoldR$expected_Y[[i]])
    if (i == 2) {
      print("")
      print(newRsameasoldR$Y_prime[[2]])
      print(newRsameasoldR$expected_Y[[2]])
      print(equal_byname(newRsameasoldR$Y_prime[[2]], newRsameasoldR$expected_Y[[2]]))
    }
  }

  doubleR <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "final" and "useful" as the Last.stage.
    filter(Last.stage != "services") %>%
    # At present, UKEnergy2000mats has V matrices that are the sum of both V and R.
    # Change to use the R matrix.
    rename(
      R_plus_V = V
    ) %>%
    separate_RV() %>%
    # At this point, the matrices are they way we want them.
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats() %>%
    # Calculate the efficiency of every industry in the ECC.
    calc_eta_i() %>%
    # Make an R_prime matrix that gives twice the resource inputs to the economy.
    mutate(
      R_prime = elementproduct_byname(2, R)
    ) %>%
    # Now call the new_R function which will calculate
    # updated U, V, and Y matrices (U_prime, V_prime, and Y_prime)
    # given R_prime.
    # Each of the *_prime matrices should be 2x their originals,
    # because R_prime is 2x relative to R.
    new_R_ps() %>%
    # Clean the rows of U_prime, because they contain Products that are not present in U.
    mutate(
      U_prime = clean_byname(U_prime, margin = 1),
      Y_prime = clean_byname(Y_prime, margin = 1)
    ) %>%
    mutate(
      expected_U = elementproduct_byname(2, U),
      expected_V = elementproduct_byname(2, V),
      expected_Y = elementproduct_byname(2, Y)
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
    # At present, UKEnergy2000mats has V matrices that are the sum of both V and R.
    # Change to use the R matrix.
    rename(
      R_plus_V = V
    ) %>%
    separate_RV() %>%
    # At this point, the matrices are they way we want them.
    # Calculate the input-output matrices which are inputs to the new_R function.
    calc_io_mats() %>%
    # Calculate the efficiency of every industry in the ECC.
    calc_eta_i() %>%
    # Make an R_prime matrix that gives twice the resource inputs to the economy.
    mutate(
      R_prime = elementproduct_byname(2, R)
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

