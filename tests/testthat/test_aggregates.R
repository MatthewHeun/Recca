library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(lazyeval)
library(matsbyname)
library(testthat)

###########################################################
context("Aggregates")
###########################################################



test_that("aggregates of SUT data work as expected", {
  expectations <- file.path("tests", "expectations")
  # Load all expectations for tests into the global environment
  expected_primary_total_aggregates_sut <- load(file = file.path(expectations, "expected_primary_total_aggregates_sut.rda"),
                                                envir = .GlobalEnv)

  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  sutdata <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix)
  primary_total_aggregates_sut <- sutdata %>%
    primary_aggregates(p_industries = p_industries, by = "Total",
                       aggregate_primary_colname = "EX_total_agg.ktoe",
                       keep_cols = c("Country", "Year", "Energy.type", "Last.stage"))
  expect_equal(primary_total_aggregates_sut, expected_primary_total_aggregates_sut)
  primary_product_aggregates_sut <- sutdata %>%
    primary_aggregates(p_industries = p_industries, by = "Product",
                       aggregate_primary_colname = "EX_product_agg.ktoe",
                       keep_cols = c("Country", "Year", "Energy.type", "Last.stage")) %>% View
  # expect_equal(primary_product_aggregates_sut$EX_product_agg.ktoe, c(93000, 93000, 93000, 98220))
  #
  #
  #
  #
  # final_total_aggregates_sut <- UKEnergy2000mats %>%
  #   spread(key = matrix.name, value = matrix) %>%
  #   finaldemand_aggregates(p_industries = p_industries, aggregate_primary_colname = "EX_agg.ktoe",
  #                      keep_cols = c("Country", "Year", "Energy.type", "Last.stage"))
  # expect_equal(final_total_aggregates_sut$EX_agg.ktoe, c(93000, 93000, 93000, 98220))


})
