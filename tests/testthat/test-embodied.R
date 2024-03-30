
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
    dplyr::select(Country, Year, EnergyType, LastStage, R, U, U_feed, V, Y, r_EIOU, S_units) %>%
    calc_io_mats() %>%
    dplyr::mutate(
      U_EIOU = matsbyname::hadamardproduct_byname(r_EIOU, U)
    ) %>%
    calc_embodied_mats() %>%
    calc_embodied_etas(primary_machine_names = primary_machine_names)

  # Focus on G and H matrices
  GH <- emb_mats %>%
    dplyr::select(Country, Year, EnergyType, LastStage, G, H) %>%
    tidyr::gather(key = "matnames", value = "matvals", G, H) %>%
    matsindf::expand_to_tidy(drop = 0)
  GH %>%
    dplyr::filter(LastStage == IEATools::last_stages$final, EnergyType == IEATools::energy_types$e, matnames == "G", rownames == "Crude dist.", colnames == "Diesel [from Dist.]") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    expect_equal(17098.4157270469)
  GH %>%
    dplyr::filter(LastStage == IEATools::last_stages$useful, EnergyType == IEATools::energy_types$e, matnames == "H", rownames == "Diesel dist.", colnames == "Residential") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    expect_equal(453.4462184729)
  GH %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$x, matnames == "G", rownames == "Gas wells & proc.", colnames == "Space heating [m3-K]") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    expect_equal(26083.5277231996)
  GH %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$x, matnames == "H", rownames == "NG dist.", colnames == "Residential") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    expect_equal(42303.8915219182)

  # Focus on E matrices
  E <- emb_mats %>%
    dplyr::select(Country, Year, EnergyType, LastStage, E) %>%
    tidyr::gather(key = "matnames", value = "matvals", E) %>%
    matsindf::expand_to_tidy(drop = 0)
  E %>%
    dplyr::filter(LastStage == IEATools::last_stages$final, EnergyType == IEATools::energy_types$e, rownames == "Crude [from Dist.]", colnames == "Crude dist.") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    expect_equal(1)
  E %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$x, rownames == "NG", colnames == "Gas wells & proc.") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(-1.0487804878)
  E %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$e, rownames == "NG [from Dist.]", colnames == "Power plants") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(-2.5)

  # Focus on M matrices
  M <- emb_mats %>%
    dplyr::select(Country, Year, EnergyType, LastStage, M_p, M_s) %>%
    tidyr::gather(key = "matnames", value = "matvals", M_p, M_s) %>%
    matsindf::expand_to_tidy(drop = 0)
  M %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, matnames == "M_s", EnergyType == IEATools::energy_types$e, rownames == "Diesel [from Dist.]", colnames == "Residential") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(218.8438205116)
  M %>%
    dplyr::filter(LastStage == IEATools::last_stages$useful, matnames == "M_p", EnergyType == IEATools::energy_types$e, rownames == "NG [from Wells]", colnames == "MD [from Car engines]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(207.9296211559)
  M %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, matnames == "M_p", EnergyType == IEATools::energy_types$x, rownames == "Petrol", colnames == "Space heating [m3-K]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(93.2008177323)

  # Focus on footprint matrices
  F_fe <- emb_mats %>%
    dplyr::select(Country, Year, EnergyType, LastStage, F_footprint_p, F_effects_p, F_footprint_s, F_effects_s) %>%
    tidyr::gather(key = "matnames", value = "matvals", F_footprint_p, F_effects_p, F_footprint_s, F_effects_s) %>%
    matsindf::expand_to_tidy(drop = 0)
  F_fe %>%
    dplyr::filter(LastStage == IEATools::last_stages$final, EnergyType == IEATools::energy_types$e, matnames == "F_effects_p", rownames == "Crude", colnames == "Diesel [from Dist.]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0.3599666468)
  F_fe %>%
    dplyr::filter(LastStage == IEATools::last_stages$final, EnergyType == IEATools::energy_types$e, matnames == "F_footprint_p", rownames == "NG [from Wells]", colnames == "NG [from Dist.]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0.3371597830)
  F_fe %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$e, matnames == "F_effects_s", rownames == "Diesel", colnames == "Residential") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0.0054217606)
  # Because this number is so small, divide by the expected value to generate a 1.
  F_fe %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$e, matnames == "F_footprint_s", rownames == "LTH", colnames == "Residential") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    magrittr::divide_by(3.9993866199998427767e-11) |>
    expect_equal(1)
  F_fe %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$x, matnames == "F_effects_p", rownames == "Crude [from Fields]", colnames == "Illumination [lumen-hrs/yr]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0.0020440162005698)
  # Because this number is so small, divide by the expected value to generate a 1.
  F_fe %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$x, matnames == "F_footprint_p", rownames == "MD [from Truck engines]", colnames == "Illumination [lumen-hrs/yr]") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    magrittr::divide_by(1.549723e-14) |>
    expect_equal(1)
  # Because this number is so small, divide by the expected value to generate a 1.
  F_fe %>%
    dplyr::filter(LastStage == IEATools::last_stages$services, EnergyType == IEATools::energy_types$x, matnames == "F_footprint_s", rownames == "Petrol [from Dist.]", colnames == "Transport") %>%
    dplyr::select(matvals) %>%
    unlist() |>
    unname() |>
    magrittr::divide_by(4.3369065801119745e-08) |>
    expect_equal(1)

  # Focus on efficiencies
  embodied_etas <- emb_mats %>%
    dplyr::select(Country, Year, EnergyType, LastStage, eta_p, eta_s) %>%
    tidyr::gather(key = "matnames", value = "matvals", eta_p, eta_s) %>%
    matsindf::expand_to_tidy(drop = 0)
  embodied_etas %>%
    dplyr::filter(EnergyType == IEATools::energy_types$e, LastStage == IEATools::last_stages$final, matnames == "eta_p", rownames == "Diesel [from Dist.]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0.81397293779504)
  embodied_etas %>%
    dplyr::filter(EnergyType == IEATools::energy_types$e, LastStage == IEATools::last_stages$services, matnames == "eta_p", rownames == "Space heating [m3-K]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(2833600.6039485)
  embodied_etas %>%
    dplyr::filter(EnergyType == IEATools::energy_types$e, LastStage == IEATools::last_stages$useful, matnames == "eta_s", rownames == "Transport") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0.48990686308539)
  embodied_etas %>%
    dplyr::filter(EnergyType == IEATools::energy_types$x, LastStage == IEATools::last_stages$services, matnames == "eta_p", rownames == "Illumination [lumen-hrs/yr]") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(29203976824)
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

