library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(lazyeval)
library(matsbyname)
library(testthat)
library(rlang)

###########################################################
context("Aggregates")
###########################################################

test_that("aggregates of SUT data work as expected", {
  expec_path <- file.path("tests", "expectations")

  if (is_testing()) {
    # testthat sets the working directory to the folder containing this file.
    # We want the ability to use these tests interactively, too,
    # when the working directory will be the top level of this project.
    # So change the working directory if we're testing.
    # Save the current working directory, to be restored later
    cwd <- getwd()
    # Move the working directory up two levels, to the top level of this project.
    setwd(file.path("..", ".."))
  }
  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  sutdata <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix)
  primary_total_aggregates_sut <- sutdata %>%
    primary_aggregates(p_industries = p_industries, by = "Total",
                       aggregate_primary_colname = "EX_total_agg.ktoe",
                       keep_cols = c("Country", "Year", "Energy.type", "Last.stage"))
  expect_known_value(primary_total_aggregates_sut,
                     file.path(expec_path, "expected_primary_total_aggregates_sut.rds"),
                     update = FALSE)
  primary_product_aggregates_sut <- sutdata %>%
    primary_aggregates(p_industries = p_industries, by = "Product",
                       aggregate_primary_colname = "EX_product_agg.ktoe",
                       keep_cols = c("Country", "Year", "Energy.type", "Last.stage"))
  expect_known_value(primary_product_aggregates_sut,
                     file.path(expec_path, "expected_product_total_aggregates_sut.rds"),
                     update = FALSE)
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


  if (is_testing()) {
    # Restore the previous working directory.
    setwd(cwd)
  }
})
