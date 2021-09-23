test_that("extend_to_exergy() works as expected", {
  # Create a vector of phi values.
  # Each final and useful energy carrier needs a phi value.
  # Use the UKEnergy2000mats data frame for the tests.
  # Get the list of final energy carriers from the matrices in UKEnergy2000mats.



  sutmats <- UKEnergy2000mats %>%
    # Put in wide-by-matrix format.
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Eliminate services ECCs.
    dplyr::filter(Last.stage %in% c("Final", "Useful")) %>%
    dplyr::mutate(
      phi = matsbyname::make_list(Recca::phi_vec, n = nrow(.), lenx = 1)
    )
  res <- extend_to_exergy(sutmats)
  expect_true(Recca::energy_types$x %in% res[[Recca::energy_types$energy_type]] %>% unique())

  # Check a couple values

  # R matrix
  energy_val <- res[[Recca::psut_cols$R]][[1]]["Resources - Crude", "Crude"]
  exergy_val <- res[[Recca::psut_cols$R]][[3]]["Resources - Crude", "Crude"]
  phi <- Recca::phi_vec["Crude", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$R]][[1]]["Resources - NG", "NG"]
  exergy_val <- res[[Recca::psut_cols$R]][[3]]["Resources - NG", "NG"]
  phi <- Recca::phi_vec["NG", ]
  expect_equal(energy_val*phi, exergy_val)

  # U matrix
  energy_val <- res[[Recca::psut_cols$U]][[1]]["Diesel - Dist.", "NG dist."]
  exergy_val <- res[[Recca::psut_cols$U]][[3]]["Diesel - Dist.", "NG dist."]
  phi <- Recca::phi_vec["Diesel - Dist.", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$U_feed]][[2]]["Elect - Grid", "Light fixtures"]
  exergy_val <- res[[Recca::psut_cols$U_feed]][[4]]["Elect - Grid", "Light fixtures"]
  phi <- Recca::phi_vec["Elect - Grid", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$U_eiou]][[2]]["Diesel - Dist.", "Gas wells & proc."]
  exergy_val <- res[[Recca::psut_cols$U_eiou]][[4]]["Diesel - Dist.", "Gas wells & proc."]
  phi <- Recca::phi_vec["Diesel - Dist.", ]
  expect_equal(energy_val*phi, exergy_val)

  # V matrix
  energy_val <- res[[Recca::psut_cols$V]][[1]]["Power plants", "Elect"]
  exergy_val <- res[[Recca::psut_cols$V]][[3]]["Power plants", "Elect"]
  phi <- Recca::phi_vec["Elect", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$V]][[2]]["Furnaces", "LTH"]
  exergy_val <- res[[Recca::psut_cols$V]][[4]]["Furnaces", "LTH"]
  phi <- Recca::phi_vec["LTH", ]
  expect_equal(energy_val*phi, exergy_val)


  # Y matrix
  energy_val <- res[[Recca::psut_cols$Y]][[2]]["Light", "Residential"]
  exergy_val <- res[[Recca::psut_cols$Y]][[4]]["Light", "Residential"]
  phi <- Recca::phi_vec["Light", ]
  expect_equal(energy_val*phi, exergy_val)

  energy_val <- res[[Recca::psut_cols$Y]][[2]]["LTH", "Transport"]
  exergy_val <- res[[Recca::psut_cols$Y]][[4]]["LTH", "Transport"]
  phi <- Recca::phi_vec["LTH", ]
  expect_equal(energy_val*phi, exergy_val)


  # Try an erroneous case, when the Energy.type column has something other than E
  sutmats %>%
    dplyr::mutate(
      "{Recca::energy_types$energy_type}" := c("W", "X")
    ) %>%
    extend_to_exergy() %>%
    expect_error("non-energy rows were found:")

})
