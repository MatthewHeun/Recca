library(dplyr)
library(magrittr)
library(matsbyname)
library(matsindf)
library(testthat)
library(tidyr)


###########################################################
context("Efficiency")
###########################################################

test_that("efficiencies are calculated correctly", {
  result <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    calc_eta_i() %>%
    select(Country, Year, Energy.type, Last.stage, eta_i) %>%
    gather(key = matnames, value = matvals, eta_i) %>%
    expand_to_tidy() %>%
    mutate(
      # Make expected values
      expected = case_when(
        startsWith(rownames, "Resources - ") ~ Inf,
        Last.stage == "services" & endsWith(rownames, " dist.") ~ NA_real_,
        rownames %in% c("Cars", "Homes", "Rooms", "Trucks") ~ NA_real_,
        TRUE ~ matvals
      )
    )

  expect_equal(result$matvals, result$expected)

})



