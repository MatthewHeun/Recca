library(dplyr)
library(lazyeval)
library(matsbyname)
library(testthat)
library(parallel)

###########################################################
context("SUT energy balance")
###########################################################

test_that("SUT matrix energy balance works with energy only", {
  expect_silent(
    UKEnergy2000mats %>%
      spread(key = matrix.name, value = matrix) %>%
      filter(Last.stage %in% c("final", "useful")) %>%
      verify_SUT_energy_balance()
  )
})

test_that("SUT matrix energy balance fails when a number has changed", {
  mats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    filter(Last.stage == "final")
  U <- mats$U[[1]]
  V <- mats$V[[1]]
  Y <- mats$Y[[1]]
  expect_equal(verify_SUT_energy_balance(U = U, V = V, Y = Y),
               list(.SUT_energy_balance = TRUE))
  Y[2, 2] <- 42 # Replace a 0 with a value
  expect_warning(verify_SUT_energy_balance(U = U, V = V, Y = Y),
                 "Energy not conserved")
})

test_that("SUT matrix energy balance works as expected", {
  result <- verify_SUT_energy_balance_with_units(
    UKEnergy2000mats %>% spread(key = matrix.name, value = matrix),
    tol = 1e-3)
  expect_true(all(result$.SUT_prod_energy_balance %>% as.logical()))
  expect_true(all(result$.SUT_ind_energy_balance %>% as.logical()))
})

test_that("all SUT industries are producing energy", {
  UKspread <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  expect_silent(verify_SUT_industry_production(UKspread))
  result <- verify_SUT_industry_production(UKspread)
  expect_true(all(result[[".industry_production_OK"]] %>% as.logical()))
  # Try it when something doesn't produce energy
  U <- UKspread$U[[1]]
  V <- UKspread$V[[1]]
  V[1, 2] <- 0 # Zero out production of Crude - Dist. from Crude dist.
  expect_warning(result <- verify_SUT_industry_production(U = U, V = V),
                 "There are some industries that consume but do not produce energy.")
  expect_false(result$.industry_production_OK)
  expect_equal(expected = "Crude dist.", result$.problem_industries)
})

test_that("SUT energy balance works with single matrices", {
  UKspread <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  U <- UKspread$U[[1]]
  V <- UKspread$V[[1]]
  Y <- UKspread$Y[[1]]
  expect_silent(verify_SUT_energy_balance(U = U, V = V, Y = Y))
})


###########################################################
context("IEA energy balance")
###########################################################

test_that("IEA energy balance works correctly", {
  # Make sure that it works.
  expect_silent(
    UKEnergy2000tidy %>%
      group_by(Country, Year, Energy.type, Last.stage) %>%
      verify_IEATable_energy_balance(energy = "EX.ktoe")
  )

  # Introduce something to make the energy balance fail.
  Unbalanced <- UKEnergy2000tidy
  # Change from 5e4 to 1e4
  Unbalanced$EX.ktoe[[1]] <- 1e4
  # Now try energy balance. It should fail.
  expect_error(Unbalanced %>%
                 group_by(Country, Year, Energy.type, Last.stage) %>%
                 verify_IEATable_energy_balance(energy = "EX.ktoe"),
                 "Energy not balanced in verify_IEATable_energy_balance.")

})

