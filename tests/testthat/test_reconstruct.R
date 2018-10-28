library(dplyr)
library(Hmisc)
library(magrittr)
library(matsbyname)
library(matsindf)
library(Recca)
library(testthat)
library(tidyr)

###########################################################
context("Reconstructing PSUT matrices")
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
  expec_path <- file.path("tests", "expectations")

  if (is_testing()) {
    # testthat sets the working directory to the folder containing this file.
    # We want the ability to use these tests interactively, too,
    # when the working directory will be the top level of this project.
    # So change the working directory if we're testing.
    # Save the current working directory, to be restored later
    currwd <- getwd()
    # Move the working directory up two levels, to the top level of this project.
    setwd(file.path("..", ".."))
  }

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
  expect_known_value(Reconstructed_Residential,
                     file.path(expec_path, "expected_Reconstructed_Residential.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})


###########################################################
context("Perfect substitution")
###########################################################

test_that("calculation of K matrix works", {
  expec_path <- file.path("tests", "expectations")

  if (is_testing()) {
    # testthat sets the working directory to the folder containing this file.
    # We want the ability to use these tests interactively, too,
    # when the working directory will be the top level of this project.
    # So change the working directory if we're testing.
    # Save the current working directory, to be restored later
    currwd <- getwd()
    # Move the working directory up two levels, to the top level of this project.
    setwd(file.path("..", ".."))
  }

  perfectsub_mats <- PerfectSubmats %>%
    spread(key = "matrix.name", value = "matrix")

  io_mats <- perfectsub_mats %>% calc_io_mats()
  K <- io_mats$K[[1]]

  expect_known_value(K, file.path(expec_path, "expected_K.rds"), update = FALSE)

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

  expect_known_value(new_UV, file.path(expec_path, "expected_new_UV_from_new_k_ps.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})
