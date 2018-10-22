library(dplyr)
library(Hmisc)
library(magrittr)
library(matsbyname)
library(matsindf)
library(Recca)
library(testthat)
library(tidyr)

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

