library(dplyr)
library(lazyeval)
library(matsbyname)
library(testthat)

###########################################################
context("Tidy IEA energy balance")
###########################################################

test_that("SUT matrix energy balance works as expected", {
  expect_silent(verify_SUT_energy_balance(UKEnergy2000mats))
})
