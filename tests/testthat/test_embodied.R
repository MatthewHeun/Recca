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
  Recca:::test_against_file(GH, "expected_GH.rds", update = FALSE)

  # Calculate E matrices
  E <- GH %>%
    mutate(
      U_EIOU = elementproduct_byname(U, r_EIOU)
    ) %>%
    calc_E() %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A,
           L_pxp, L_ixp, U_EIOU, G, H, E)
  Recca:::test_against_file(E, "expected_E.rds", update = FALSE)

  # Calculate M matrices
  M <- E %>%
    calc_M() %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A,
           L_pxp, L_ixp, U_EIOU, G, H, E, M_p, M_s)
  Recca:::test_against_file(M, "expected_M.rds", update = FALSE)

  # Calculate footprint matrices
  F_fe <- M %>%
    calc_F_footprint_effects()
  Recca:::test_against_file(F_fe, "expected_F.rds", update = FALSE)

  # Calcualte efficiencies
  primary_machine_names <- c("Resources - Crude", "Resources - NG")
  embodied_etas <- F_fe %>%
    calc_embodied_etas(primary_machine_names = primary_machine_names)
  Recca:::test_against_file(embodied_etas, "expected_embodied_etas.rds", update = FALSE)

  # Calculate all embodied matrices
  embodied_mats <- io_mats %>%
    mutate(
      U_EIOU = elementproduct_byname(r_EIOU, U)
    ) %>%
    calc_embodied_mats()
  Recca:::test_against_file(embodied_mats, "expected_embodied_mats.rds", update = FALSE)
})




