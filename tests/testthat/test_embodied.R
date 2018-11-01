library(dplyr)
library(matsbyname)
library(parallel)
library(tidyr)

###########################################################
context("Embodied energy calculations")
###########################################################

test_that("embodied energy calculations works as expected", {
  # Calculate all IO matrices
  io_mats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units) %>%
    calc_io_mats()

  # Calculate the G and H matrices
  GH <- io_mats %>%
    calc_GH() %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A, L_pxp, L_ixp,
           G, H)
  expect_known_value(GH, system.file("expectations", "expected_GH.rds", package = "Recca"), update = FALSE)

  # Calculate E matrices
  E <- GH %>%
    mutate(
      U_EIOU = elementproduct_byname(U, r_EIOU)
    ) %>%
    calc_E() %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A,
           L_pxp, L_ixp, U_EIOU, G, H, E)
  expect_known_value(E, system.file("expectations", "expected_E.rds", package = "Recca"), update = FALSE)

  # Calculate M matrices
  M <- E %>%
    calc_M() %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A,
           L_pxp, L_ixp, U_EIOU, G, H, E, M_p, M_s)
  expect_known_value(M, system.file("expectations", "expected_M.rds", package = "Recca"), update = FALSE)

  # Calculate footprint matrices
  F_fe <- M %>%
    calc_F_footprint_effects()
  expect_known_value(F_fe, system.file("expectations", "expected_F.rds", package = "Recca"), update = FALSE)

  # Calcualte efficiencies
  primary_machine_names <- c("Resources - Crude", "Resources - NG")
  embodied_etas <- F_fe %>%
    calc_embodied_etas(primary_machine_names = primary_machine_names)
  expect_known_value(embodied_etas, system.file("expectations", "expected_embodied_etas.rds", package = "Recca"), update = FALSE)

  # Calculate all embodied matrices
  embodied_mats <- io_mats %>%
    mutate(
      U_EIOU = elementproduct_byname(r_EIOU, U)
    ) %>%
    calc_embodied_mats()
  expect_known_value(embodied_mats, system.file("expectations", "expected_embodied_mats.rds", package = "Recca"), update = FALSE)
})




