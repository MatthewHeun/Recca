library(dplyr)
library(matsbyname)
library(parallel)
library(tidyr)

###########################################################
context("Embodied energy calculations")
###########################################################

test_that("embodied energy calculations works as expected", {
  primary_machine_names <- c("Resources - Crude", "Resources - NG")

  # Calculate all embodied matrices
  emb_mats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    select(Country, Year, Energy.type, Last.stage, U, V, Y, r_EIOU, S_units) %>%
    calc_io_mats() %>%
    mutate(
      U_EIOU = hadamardproduct_byname(r_EIOU, U)
    ) %>%
    calc_embodied_mats() %>%
    calc_embodied_etas(primary_machine_names = primary_machine_names)

  # Focus on G and H matrices
  GH <- emb_mats %>%
    select(Country, Year, Energy.type, Last.stage, G, H) %>%
    gather(key = "matnames", value = "matvals", G, H) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(GH %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "G", rownames == "Crude dist.", colnames == "Diesel - Dist.") %>% select(matvals) %>% unlist(),
                    17098.4157270469)
  expect_equivalent(GH %>%
                      filter(Last.stage == "useful", Energy.type == "E.ktoe", matnames == "H", rownames == "Diesel dist.", colnames == "Residential") %>% select(matvals) %>% unlist(),
                    453.4462184729)
  expect_equivalent(GH %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "G", rownames == "Gas wells & proc.", colnames == "Space heating [m3-K]") %>% select(matvals) %>% unlist(),
                    26083.5277231996)
  expect_equivalent(GH %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "H", rownames == "NG dist.", colnames == "Residential") %>% select(matvals) %>% unlist(),
                    42303.8915219182)

  # Focus on E matrices
  E <- emb_mats %>%
    select(Country, Year, Energy.type, Last.stage, E) %>%
    gather(key = "matnames", value = "matvals", E) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(E %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", rownames == "Crude - Dist.", colnames == "Crude dist.") %>% select(matvals) %>% unlist(),
                    1)
  expect_equivalent(E %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", rownames == "NG", colnames == "Gas wells & proc.") %>% select(matvals) %>% unlist(),
                    -1.0487804878)
  expect_equivalent(E %>%
                      filter(Last.stage == "services", Energy.type == "E.ktoe", rownames == "NG - Dist.", colnames == "Power plants") %>% select(matvals) %>% unlist(),
                    -2.5)

  # Focus on M matrices
  M <- emb_mats %>%
    select(Country, Year, Energy.type, Last.stage, M_p, M_s) %>%
    gather(key = "matnames", value = "matvals", M_p, M_s) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(M %>%
                      filter(Last.stage == "services", matnames == "M_s", Energy.type == "E.ktoe", rownames == "Diesel - Dist.", colnames == "Residential") %>% select(matvals) %>% unlist(),
                    218.8438205116)
  expect_equivalent(M %>%
                      filter(Last.stage == "useful", matnames == "M_p", Energy.type == "E.ktoe", rownames == "NG - Wells", colnames == "MD - Car engines") %>% select(matvals) %>% unlist(),
                    207.9296211559)
  expect_equivalent(M %>%
                      filter(Last.stage == "services", matnames == "M_p", Energy.type == "X.ktoe", rownames == "Petrol", colnames == "Space heating [m3-K]") %>% select(matvals) %>% unlist(),
                    93.2008177323)

  # Focus on footprint matrices
  F_fe <- emb_mats %>%
    select(Country, Year, Energy.type, Last.stage, F_footprint_p, F_effects_p, F_footprint_s, F_effects_s) %>%
    gather(key = "matnames", value = "matvals", F_footprint_p, F_effects_p, F_footprint_s, F_effects_s) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(F_fe %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "F_effects_p", rownames == "Crude", colnames == "Diesel - Dist.") %>% select(matvals) %>% unlist(),
                    0.3599666468)
  expect_equivalent(F_fe %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "F_footprint_p", rownames == "NG - Wells", colnames == "NG - Dist.") %>% select(matvals) %>% unlist(),
                    0.3371597830)
  expect_equivalent(F_fe %>%
                      filter(Last.stage == "services", Energy.type == "E.ktoe", matnames == "F_effects_s", rownames == "Diesel", colnames == "Residential") %>% select(matvals) %>% unlist(),
                    0.0054217606)
  # Because this number is so small, divide by the expected value to generate a 1.
  expect_equivalent(F_fe %>%
                      filter(Last.stage == "services", Energy.type == "E.ktoe", matnames == "F_footprint_s", rownames == "LTH", colnames == "Residential") %>% select(matvals) %>% unlist() / 3.9993866199998427767e-11,
                    1)
  expect_equivalent(F_fe %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "F_effects_p", rownames == "Crude - Fields", colnames == "Illumination [lumen-hrs/yr]") %>% select(matvals) %>% unlist(),
                    0.002044016)
  # Because this number is so small, divide by the expected value to generate a 1.
  expect_equivalent(F_fe %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "F_footprint_p", rownames == "MD - Truck engines", colnames == "Illumination [lumen-hrs/yr]") %>% select(matvals) %>% unlist() / 1.549723e-14,
                    1)
  # Because this number is so small, divide by the expected value to generate a 1.
  expect_equivalent(F_fe %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "F_footprint_s", rownames == "Petrol - Dist.", colnames == "Transport") %>% select(matvals) %>% unlist() / 4.3369065801119745e-08,
                    1)

  # Focus on efficiencies
  embodied_etas <- emb_mats %>%
    select(Country, Year, Energy.type, Last.stage, eta_p, eta_s) %>%
    gather(key = "matnames", value = "matvals", eta_p, eta_s) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(embodied_etas %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", matnames == "eta_p", rownames == "Diesel - Dist.") %>% select(matvals) %>% unlist(),
                    0.81397293779504)
  expect_equivalent(embodied_etas %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", matnames == "eta_p", rownames == "Space heating [m3-K]") %>% select(matvals) %>% unlist(),
                    2833600.6039485)
  expect_equivalent(embodied_etas %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "useful", matnames == "eta_s", rownames == "Transport") %>% select(matvals) %>% unlist(),
                    0.48990686308539)
  expect_equivalent(embodied_etas %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", matnames == "eta_p", rownames == "Illumination [lumen-hrs/yr]") %>% select(matvals) %>% unlist(),
                    29203976824)
})




