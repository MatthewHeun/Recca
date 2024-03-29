
test_that("calc_E_EIOU returns correct E_EIOU matrix and e_EIOU vector",{
  EIOU_mats <- UKEnergy2000mats %>%
    dplyr::filter(Last.stage == "Final", Energy.type == "E") %>%
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
    calc_io_mats() %>%
    calc_E_EIOU()

  E_EIOU <- EIOU_mats[["E_EIOU"]][[1]]
  e_EIOU <- EIOU_mats[["e_EIOU"]][[1]]

  # The expected values for the E_EIOU matrix and e_EIOU vector were calculated in LibreOffice.
  # See file "UK_2000_EROI_example.ods"
  # --- EAR, September 1st 2020

  # Checking E_EIOU
  expect_equal(E_EIOU["Crude [from Dist.]", "Crude dist."], 0.0105263157894737)
  expect_equal(E_EIOU["Diesel [from Dist.]", "Gas wells & proc."], 0.00116279069767442)
  expect_equal(E_EIOU["Diesel [from Dist.]", "NG dist."], 0.000609756097560976)
  expect_equal(E_EIOU["Elect [from Grid]", "Gas wells & proc."], 0.000581395348837209)

  # Checking e_EIOU
  expect_equal(e_EIOU["Crude dist.", "Product"], 0.0115789473684211)
  expect_equal(e_EIOU["Diesel dist.", "Product"], 0.0225806451612903)
  expect_equal(e_EIOU["Elect. grid", "Product"], 0)
  expect_equal(e_EIOU["Gas wells & proc.", "Product"], 0.0482558139534884)
  expect_equal(e_EIOU["NG dist.", "Product"], 0.00121951219512195)
  expect_equal(e_EIOU["Oil fields", "Product"], 0.0515)
  expect_equal(e_EIOU["Oil refineries", "Product"], 0.107978723404255)
  expect_equal(e_EIOU["Petrol dist.", "Product"], 0.0283018867924528)
  expect_equal(e_EIOU["Power plants", "Product"], 0.015625)
})


test_that("calc_erois() returns appropriate EROIs",{
  EIOU_mats <- UKEnergy2000mats %>%
    dplyr::filter(Last.stage == "Final", Energy.type == "E") %>%
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
    calc_io_mats() %>%
    calc_E_EIOU()

  EROI_mats <- EIOU_mats %>% calc_erois()

  # The expected values for the E_EIOU matrix and e_EIOU vector were calculated in LibreOffice.
  # See file "UK_2000_EROI_example.ods"
  # --- EAR, September 1st 2020

  # Non-feed EROIs
  eroi_g_p <- EROI_mats$eroi_g_p[[1]]
  #eroi_n_p <- EROI_mats$eroi_n_p[[1]]
  eroi_g_i <- EROI_mats$eroi_g_i[[1]]
  #eroi_n_i <- EROI_mats$eroi_n_i[[1]]

  # Feed EROIs
  eroi_g_p_feed <- EROI_mats$eroi_g_p_feed[[1]]
  #eroi_n_p_feed <- EROI_mats$eroi_n_p_feed[[1]]
  eroi_g_i_feed <- EROI_mats$eroi_g_i_feed[[1]]
  #eroi_n_i_feed <- EROI_mats$eroi_n_i_feed[[1]]

  # Checking non-feed EROIs

  # g_eroi_p
  expect_equal(eroi_g_p["Diesel", "Industry"], 5.09969733020006)
  expect_equal(eroi_g_p["NG [from Wells]", "Industry"], 19.6173481664719)
  expect_equal(eroi_g_p["Petrol [from Dist.]", "Industry"], 4.33166026595272)

  # n_eroi_p
  # expect_equal(eroi_n_p["Diesel", "Industry"], 4.09969733020006)
  # expect_equal(eroi_n_p["NG - Wells", "Industry"], 18.6173481664719)
  # expect_equal(eroi_n_p["Petrol - Dist.", "Industry"], 3.33166026595272)

  # g_eroi_i
  expect_equal(eroi_g_i["Elect. grid", "Industry"], 6.57766701757502)
  expect_equal(eroi_g_i["Oil refineries", "Industry"], 5.09969733020006)
  expect_equal(eroi_g_i["Crude dist.", "Industry"], 14.9235690942869)

  # n_eroi_i
  # expect_equal(eroi_n_i["Elect. grid", "Industry"], 5.57766701757502)
  # expect_equal(eroi_n_i["Oil refineries", "Industry"], 4.09969733020006)
  # expect_equal(eroi_n_i["Crude dist.", "Industry"], 13.9235690942869)

  # Checking feed EROIs

  # g_eroi_p_feed
  expect_equal(eroi_g_p_feed["Crude [from Dist.]", "Industry"], 15.8531497705465)
  expect_equal(eroi_g_p_feed["Petrol", "Industry"], 5.84598162410927)
  expect_equal(eroi_g_p_feed["NG", "Industry"], Inf)

  # n_eroi_p_feed
  # expect_equal(eroi_n_p_feed["Crude - Dist.", "Industry"], 14.8531497705465)
  # expect_equal(eroi_n_p_feed["Petrol", "Industry"], 4.84598162410927)
  # expect_equal(eroi_n_p_feed["NG", "Industry"], Inf)

  # g_eroi_i_feed
  expect_equal(eroi_g_i_feed["Diesel dist.", "Industry"], 5.16426718119679)
  expect_equal(eroi_g_i_feed["NG dist.", "Industry"], 20.2120951562052)
  expect_equal(eroi_g_i_feed["Power plants", "Industry"], 7.17806476238947)


  # n_eroi_i_feed
  # expect_equal(eroi_n_i_feed["Diesel dist.", "Industry"], 4.16426718119679)
  # expect_equal(eroi_n_i_feed["NG dist.", "Industry"], 19.2120951562052)
  # expect_equal(eroi_n_i_feed["Power plants", "Industry"], 6.17806476238947)
})


