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
    new_Y()
  Recca:::test_against_file(Reconstructed_Residential, "expected_Reconstructed_Residential.rds", update = FALSE)
})


###########################################################
context("New perfectly substitutable inputs")
###########################################################

test_that("new_k_ps works as expected", {
  perfectsub_mats <- PerfectSubmats %>%
    spread(key = "matrix.name", value = "matrix")

  io_mats <- perfectsub_mats %>% calc_io_mats()
  K <- io_mats$K[[1]]
  Recca:::test_against_file(K, "expected_K.rds", update = FALSE)

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
  new_UV <- new_k_ps(io_mats)
  Recca:::test_against_file(new_UV, "expected_new_UV_from_new_k_ps.rds", update = FALSE)
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
      V_plus_R = V
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
    # Set up the expectations
    mutate(
      # When R_prime = R, we expect to recover same U, V, and Y.
      expected_U = U,
      expected_V = V,
      expected_Y = Y
    )

  # Test that everything worked as expected
  for (i in 1:2) {
    expect_equal(newRsameasoldR$U_prime[[i]], newRsameasoldR$expected_U[[i]])
    expect_equal(newRsameasoldR$V_prime[[i]], newRsameasoldR$expected_V[[i]])
    expect_equal(newRsameasoldR$Y_prime[[i]], newRsameasoldR$expected_Y[[i]])
  }

  doubleR <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    # When Last.stage is "services", we get units problems.
    # Avoid by using only ECCs with "final" and "useful" as the Last.stage.
    filter(Last.stage != "services") %>%
    # At present, UKEnergy2000mats has V matrices that are the sum of both V and R.
    # Change to use the R matrix.
    rename(
      V_plus_R = V
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
})
