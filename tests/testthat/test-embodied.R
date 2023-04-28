
test_that("embodied energy calculations works as expected", {
  # Get a vector of primary industries for the example data set.
  # The vector of primary industries comes from the resource matrix (R).
  primary_machine_names <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    magrittr::extract2("R") %>%
    magrittr::extract2(1) %>%
    rownames()

  # Calculate all embodied matrices
  emb_mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, R, U, U_feed, V, Y, r_EIOU, S_units) %>%
    calc_io_mats() %>%
    dplyr::mutate(
      U_EIOU = matsbyname::hadamardproduct_byname(r_EIOU, U)
    ) %>%
    calc_embodied_mats() %>%
    calc_embodied_etas(primary_machine_names = primary_machine_names)

  # Focus on G and H matrices
  GH <- emb_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, G, H) %>%
    tidyr::gather(key = "matnames", value = "matvals", G, H) %>%
    matsindf::expand_to_tidy(drop = 0)

  expect_equivalent(GH %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$final, Energy.type == IEATools::energy_types$e, matnames == "G", rownames == "Crude dist.", colnames == "Diesel [from Dist.]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    17098.4157270469)
  expect_equivalent(GH %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$useful, Energy.type == IEATools::energy_types$e, matnames == "H", rownames == "Diesel dist.", colnames == "Residential") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    453.4462184729)
  expect_equivalent(GH %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$x, matnames == "G", rownames == "Gas wells & proc.", colnames == "Space heating [m3-K]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    26083.5277231996)
  expect_equivalent(GH %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$x, matnames == "H", rownames == "NG dist.", colnames == "Residential") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    42303.8915219182)

  # Focus on E matrices
  E <- emb_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, E) %>%
    tidyr::gather(key = "matnames", value = "matvals", E) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(E %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$final, Energy.type == IEATools::energy_types$e, rownames == "Crude [from Dist.]", colnames == "Crude dist.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    1)
  expect_equivalent(E %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$x, rownames == "NG", colnames == "Gas wells & proc.") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    -1.0487804878)
  expect_equivalent(E %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$e, rownames == "NG [from Dist.]", colnames == "Power plants") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    -2.5)

  # Focus on M matrices
  M <- emb_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, M_p, M_s) %>%
    tidyr::gather(key = "matnames", value = "matvals", M_p, M_s) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(M %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, matnames == "M_s", Energy.type == IEATools::energy_types$e, rownames == "Diesel [from Dist.]", colnames == "Residential") %>% select(matvals) %>% unlist(),
                    218.8438205116)
  expect_equivalent(M %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$useful, matnames == "M_p", Energy.type == IEATools::energy_types$e, rownames == "NG [from Wells]", colnames == "MD [from Car engines]") %>% select(matvals) %>% unlist(),
                    207.9296211559)
  expect_equivalent(M %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, matnames == "M_p", Energy.type == IEATools::energy_types$x, rownames == "Petrol", colnames == "Space heating [m3-K]") %>% select(matvals) %>% unlist(),
                    93.2008177323)

  # Focus on footprint matrices
  F_fe <- emb_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, F_footprint_p, F_effects_p, F_footprint_s, F_effects_s) %>%
    tidyr::gather(key = "matnames", value = "matvals", F_footprint_p, F_effects_p, F_footprint_s, F_effects_s) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(F_fe %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$final, Energy.type == IEATools::energy_types$e, matnames == "F_effects_p", rownames == "Crude", colnames == "Diesel [from Dist.]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.3599666468)
  expect_equivalent(F_fe %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$final, Energy.type == IEATools::energy_types$e, matnames == "F_footprint_p", rownames == "NG [from Wells]", colnames == "NG [from Dist.]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.3371597830)
  expect_equivalent(F_fe %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$e, matnames == "F_effects_s", rownames == "Diesel", colnames == "Residential") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.0054217606)
  # Because this number is so small, divide by the expected value to generate a 1.
  expect_equivalent(F_fe %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$e, matnames == "F_footprint_s", rownames == "LTH", colnames == "Residential") %>%
                      dplyr::select(matvals) %>%
                      unlist() / 3.9993866199998427767e-11,
                    1)
  expect_equivalent(F_fe %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$x, matnames == "F_effects_p", rownames == "Crude [from Fields]", colnames == "Illumination [lumen-hrs/yr]") %>%
                      dplyr::select(matvals) %>%
                      unlist(),
                    0.002044016)
  # Because this number is so small, divide by the expected value to generate a 1.
  expect_equivalent(F_fe %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$x, matnames == "F_footprint_p", rownames == "MD [from Truck engines]", colnames == "Illumination [lumen-hrs/yr]") %>%
                      dplyr::select(matvals) %>%
                      unlist() / 1.549723e-14,
                    1)
  # Because this number is so small, divide by the expected value to generate a 1.
  expect_equivalent(F_fe %>%
                      dplyr::filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$x, matnames == "F_footprint_s", rownames == "Petrol [from Dist.]", colnames == "Transport") %>%
                      dplyr::select(matvals) %>%
                      unlist() / 4.3369065801119745e-08,
                    1)

  # Focus on efficiencies
  embodied_etas <- emb_mats %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, eta_p, eta_s) %>%
    tidyr::gather(key = "matnames", value = "matvals", eta_p, eta_s) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(embodied_etas %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$final, matnames == "eta_p", rownames == "Diesel [from Dist.]") %>% select(matvals) %>% unlist(),
                    0.81397293779504)
  expect_equivalent(embodied_etas %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$services, matnames == "eta_p", rownames == "Space heating [m3-K]") %>% select(matvals) %>% unlist(),
                    2833600.6039485)
  expect_equivalent(embodied_etas %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$e, Last.stage == IEATools::last_stages$useful, matnames == "eta_s", rownames == "Transport") %>% select(matvals) %>% unlist(),
                    0.48990686308539)
  expect_equivalent(embodied_etas %>%
                      dplyr::filter(Energy.type == IEATools::energy_types$x, Last.stage == IEATools::last_stages$services, matnames == "eta_p", rownames == "Illumination [lumen-hrs/yr]") %>% select(matvals) %>% unlist(),
                    29203976824)
})


test_that("calc_embodied_EIOU() works correctly",{
  embodied_EIOU_mats <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") %>%
    calc_io_mats() %>%
    calc_E_EIOU() %>%
    calc_embodied_EIOU()

  # Testing first Q_eiou_p
  Q_eiou_p_mat <- embodied_EIOU_mats$Q_EIOU_p[[1]]

  expect_equal(Q_eiou_p_mat["Crude dist.", "Diesel [from Dist.]"], 197.98165578686)
  expect_equal(Q_eiou_p_mat["Oil fields", "NG [from Dist.]"], 2.88296505131538)
  expect_equal(Q_eiou_p_mat["Oil refineries", "Petrol [from Dist.]"], 3238.95390966953)
  expect_equal(Q_eiou_p_mat["Elect. grid", "Elect [from Grid]"], 0)

  # Testing second Q_eiou_s
  Q_eiou_s_mat <- embodied_EIOU_mats$Q_EIOU_s[[1]]

  expect_equal(Q_eiou_s_mat["Diesel dist.", "Transport"], 348.277886494775)
  expect_equal(Q_eiou_s_mat["NG dist.", "Residential"], 49.6044837380755)
  expect_equal(Q_eiou_s_mat["Petrol dist.", "Residential"], 0)

  # Testing third Q_eiou_feed_p
  Q_eiou_feed_p_mat <- embodied_EIOU_mats$Q_EIOU_feed_p[[1]]

  expect_equal(Q_eiou_feed_p_mat["Crude dist.", "Diesel [from Dist.]"], 170.789473684211)
  expect_equal(Q_eiou_feed_p_mat["Gas wells & proc.", "Elect [from Grid]"], 738.256277216714)
  expect_equal(Q_eiou_feed_p_mat["Oil fields", "NG [from Dist.]"], 0)


  # Testing fourth Q_eiou_feed_s
  Q_eiou_feed_s_mat <- embodied_EIOU_mats$Q_EIOU_feed_s[[1]]

  expect_equal(Q_eiou_feed_s_mat["Power plants", "Residential"], 95.6175298804781)
  expect_equal(Q_eiou_feed_s_mat["NG dist.", "Residential"], 49.1448838791177)
  expect_equal(Q_eiou_feed_s_mat["Elect. grid", "Transport"], 0)
})
# EAR, 11/09/2020

