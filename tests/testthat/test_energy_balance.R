library(dplyr)
library(lazyeval)
library(matsbyname)

###########################################################
context("Tidy IEA energy balance")
###########################################################

test_that("SUT matrix energy balance works as expected", {
  verify_SUT_energy_balance(UKEnergy2000mats)
})
