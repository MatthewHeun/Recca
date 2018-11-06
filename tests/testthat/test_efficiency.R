library(dplyr)
library(magrittr)
library(tidyr)

###########################################################
context("Efficiency")
###########################################################

test_that("efficiencies are calculated correctly", {
  result <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    calc_eta_i()

})



