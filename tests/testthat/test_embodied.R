library(parallel)

###########################################################
context("Embodied energy calculations")
###########################################################

test_that("embodied energy calculations works as expected", {
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

  # Calculate all IO matrices
  io_mats <- UKEnergy2000mats %>% calc_io_mats()

  # Calculate the G and H matrices
  GH <- io_mats %>%
    calc_GH(keep_cols = c("Country", "Year", "Energy.type", "Last.stage", "U", "V", "Y", "r_EIOU",
                          "S_units", "y", "q", "g", "W", "Z", "D", "C", "A", "L_pxp", "L_ixp")) %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A, L_pxp, L_ixp,
           G, H)
  expect_known_value(GH, file.path(expec_path, "expected_GH.rds"), update = FALSE)

  E <- GH %>%
    mutate(
      U_EIOU = elementproduct_byname(U, r_EIOU)
    ) %>%
    calc_E() %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A,
           L_pxp, L_ixp, U_EIOU, G, H, E)
  expect_known_value(E, file.path(expec_path, "expected_E.rds"), update = FALSE)

  M <- E %>%
    calc_M() %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units,
           y, q, g, W, Z, D, C, A,
           L_pxp, L_ixp, U_EIOU, G, H, E, M_p, M_s)
  expect_known_value(M, file.path(expec_path, "expected_M.rds"), update = FALSE)

  F <- M %>%
    calc_F_footprint_effects()
  expect_known_value(F, file.path(expec_path, "expected_F.rds"), update = FALSE)

  primary_machine_names <- c("Resources - Crude", "Resources - NG")
  embodied_etas <- F %>%
    calc_embodied_etas(primary_machine_names = primary_machine_names)
  expect_known_value(embodied_etas, file.path(expec_path, "expected_embodied_etas.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})




