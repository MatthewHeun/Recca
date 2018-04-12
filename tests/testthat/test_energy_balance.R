library(dplyr)
library(lazyeval)
library(matsbyname)
library(testthat)
library(parallel)

###########################################################
context("SUT energy balance")
###########################################################

test_that("SUT matrix energy balance works as expected", {
  expect_silent(verify_SUT_energy_balance_with_units(UKEnergy2000mats, tol = 1e-3))
})

test_that("all SUT industries are producing energy", {
  expect_silent(verify_SUT_industry_production(UKEnergy2000mats))
})
