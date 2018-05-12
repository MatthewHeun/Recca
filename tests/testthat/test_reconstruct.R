
###########################################################
context("Reconstructing PSUT matrices")
###########################################################

test_that("reconstructing U and V from single matrices works as expected", {
  alliomats <- calc_io_mats(UKEnergy2000mats, keep_cols = "Y")
  allUV <- reconstruct_UV(alliomats, Y_prime_colname = "Y")
  for (i in 1:nrow(allUV)) {
    UV <- reconstruct_UV(Y_prime_colname = alliomats$Y[[i]],
                         L_ixp_colname = alliomats$L_ixp[[i]],
                         L_pxp_colname = alliomats$L_pxp[[i]],
                         Z_colname = alliomats$Z[[i]],
                         D_colname = alliomats$D[[i]])
    expect_equal(UV$U_prime, allUV$U_prime[[i]])
    expect_equal(UV$V_prime, allUV$V_prime[[i]])
  }
})

test_that("reconstructing U and V with a data frame works as expected", {
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
    calc_io_mats(keep_cols = c("Country", "Year", "Energy.type", "Last.stage",
                               "U", "V", "Y", "r_EIOU", "S_units")) %>%
    mutate(
      Y_prime = Y
    ) %>%
    # reconstruct_UV(keep_cols = c("Country", "Year", "Energy.type", "Last.stage",
    #                "U", "V", "Y", "r_EIOU", "S_units", "y", "q", "g", "W", "Z", "D", "C", "A",
    #                "L_ixp", "L_pxp", "Y_prime")) %>%
    reconstruct_UV() %>%
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
    reconstruct_UV()
  expect_known_value(Reconstructed_Residential,
                     file.path(expec_path, "expected_Reconstructed_Residential.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})
