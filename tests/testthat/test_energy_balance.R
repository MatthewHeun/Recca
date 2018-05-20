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
      filter(Last.stage %in% c("final", "useful")) %>%
      verify_SUT_energy_balance()
  )
})

test_that("SUT matrix energy balance fails when a number has changed", {
  mats <- UKEnergy2000mats %>% filter(Last.stage == "final")
  U <- mats$U[[1]]
  V <- mats$V[[1]]
  Y <- mats$Y[[1]]
  expect_equal(verify_SUT_energy_balance(U_colname = U, V_colname = V, Y_colname = Y),
               list(.SUT_energy_balance = TRUE))
  Y[2, 2] <- 42 # Replace a 0 with a value
  expect_warning(verify_SUT_energy_balance(U_colname = U, V_colname = V, Y_colname = Y),
                 "Energy not conserved")
})

test_that("SUT matrix energy balance works as expected", {
  result <- verify_SUT_energy_balance_with_units(UKEnergy2000mats, tol = 1e-3)
  expect_true(all(result$.SUT_prod_energy_balance %>% as.logical()))
  expect_true(all(result$.SUT_ind_energy_balance %>% as.logical()))
})

test_that("all SUT industries are producing energy", {
  expect_silent(verify_SUT_industry_production(UKEnergy2000mats))
  # Try it when something doesn't produce energy
  U <- UKEnergy2000mats$U[[1]]
  V <- UKEnergy2000mats$V[[1]]
  V[1, 2] <- 0 # Zero out production of Crude - Dist. from Crude dist.
  expect_warning(verify_SUT_industry_production(U_colname = U, V_colname = V),
                 "There are some industries that consume but do not produce energy.")
})

test_that("SUT energy balance works with single matrices", {
  U <- UKEnergy2000mats$U[[1]]
  V <- UKEnergy2000mats$V[[1]]
  Y <- UKEnergy2000mats$Y[[1]]
  expect_silent(verify_SUT_energy_balance(U_colname = U, V_colname = V, Y_colname = Y))
})
