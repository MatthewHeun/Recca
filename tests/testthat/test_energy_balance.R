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

test_that("SUT matrix energy balance works as expected", {
  expect_silent(verify_SUT_energy_balance_with_units(UKEnergy2000mats, tol = 1e-3))
})

test_that("all SUT industries are producing energy", {
  expect_silent(verify_SUT_industry_production(UKEnergy2000mats))
})

test_that("energy_balance fails when an intermediate column already exists", {
  expect_error(
    UKEnergy2000mats %>%
      mutate(
        # Add a column that it will try to overwrite.
        .err_ind = 42
      ) %>%
      verify_SUT_energy_balance_with_units(tol = 1e-3),
    Hmisc::escapeRegex("column(s) '.err_ind' is (are) already column names in data frame '.sutdata'")
  )
})
