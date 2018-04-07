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

  # testthat sets the working directory to the folder containing this file.
  # However, the functions use_expected_data and load_expected_data
  # make different (default) assumptions about the location of the expectations directory,
  # namely that it is set to the
  # Make everything work by setting the working directory to the
  # top level directory for this project.

  # Save the current working directory, to be restored later
  cwd <- getwd()
  # Move the working directory up two levels, to the top level of this project.
  setwd(file.path("..", ".."))


  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  sutdata <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix)
  primary_total_aggregates_sut <- sutdata %>%
    primary_aggregates(p_industries = p_industries, by = "Total",
                       aggregate_primary_colname = "EX_total_agg.ktoe",
                       keep_cols = c("Country", "Year", "Energy.type", "Last.stage"))
  expect_equal(primary_total_aggregates_sut, load_expected_data("primary_total_aggregates_sut"))
  primary_product_aggregates_sut <- sutdata %>%
    primary_aggregates(p_industries = p_industries, by = "Product",
                       aggregate_primary_colname = "EX_product_agg.ktoe",
                       keep_cols = c("Country", "Year", "Energy.type", "Last.stage"))
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


  # Restore the previous working directory.
  setwd(cwd)
})
