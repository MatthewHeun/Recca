library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(lazyeval)
library(matsbyname)
library(matsindf)
library(parallel)
library(purrr)
library(testthat)
library(rlang)

###########################################################
context("Aggregates")
###########################################################

test_that("primary aggregates of SUT data work as expected", {
  expec_path <- file.path("tests", "expectations")

  if (is_testing()) {
    # testthat sets the working directory to the folder containing this file.
    # We want the ability to use these tests interactively, too,
    # when the working directory will be the top level of this project.
    # So change the working directory if we're testing.
    # Save the current working directory, to be restored later
    currwd <- getwd()
    # Move the working directory up two levels, to the top level of this project.
    setwd(file.path("..", ".."))
  }
  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  # Primary total aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Total",
                       aggregate_primary_colname = "EX_total_agg.ktoe")
  expect_known_value(primary_total_aggregates_sut,
                     file.path(expec_path, "expected_primary_total_aggregates_sut.rds"),
                     update = FALSE)
  # Primary product aggregates
  primary_product_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Product",
                       aggregate_primary_colname = "EX_product_agg.ktoe")
  expect_known_value(primary_product_aggregates_sut,
                     file.path(expec_path, "expected_primary_product_aggregates_sut.rds"),
                     update = FALSE)
  # Primary flow aggregates
  primary_flow_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Flow",
                       aggregate_primary_colname = "EX_flow_agg.ktoe")
  expect_known_value(primary_flow_aggregates_sut,
                     file.path(expec_path, "expected_primary_flow_aggregates_sut.rds"),
                     update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})

test_that("final demand aggregates of SUT data work as expected", {
  expec_path <- file.path("tests", "expectations")

  if (is_testing()) {
    # testthat sets the working directory to the folder containing this file.
    # We want the ability to use these tests interactively, too,
    # when the working directory will be the top level of this project.
    # So change the working directory if we're testing.
    # Save the current working directory, to be restored later
    currwd <- getwd()
    # Move the working directory up two levels, to the top level of this project.
    setwd(file.path("..", ".."))
  }

  # Define final demand sectors
  fd_sectors <- c("Residential", "Transport")

  # Total final demand aggregates
  final_demand_total_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Total",
                       net_aggregate_demand_colname = "EX_total_net_agg.ktoe",
                       gross_aggregate_demand_colname = "EX_total_gross_agg.ktoe")
  expect_known_value(final_demand_total_aggregates_sut,
                     file.path(expec_path, "expected_final_demand_total_aggregates_sut.rds"),
                     update = FALSE)

  # Final demand product aggregates
  final_demand_product_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Product",
                                      net_aggregate_demand_colname = "EX_product_net_agg.ktoe",
                                      gross_aggregate_demand_colname = "EX_product_gross_agg.ktoe")
  expect_known_value(final_demand_product_aggregates_sut,
                     file.path(expec_path, "expected_final_demand_product_aggregates_sut.rds"),
                     update = FALSE)

  # Final demand sector aggregates
  final_demand_sector_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Sector",
                                      net_aggregate_demand_colname = "EX_sector_net_agg.ktoe",
                                      gross_aggregate_demand_colname = "EX_sector_gross_agg.ktoe")
  expect_known_value(final_demand_sector_aggregates_sut,
                     file.path(expec_path, "expected_final_demand_sector_aggregates_sut.rds"),
                     update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})

test_that("primary_aggregates_IEA works as expected", {
  # Get the vector of primary industries for the example data set.
  r_ind <- resource_industries(UKEnergy2000mats %>%
                                spread(key = matrix.name, value = matrix))[["r_industries"]][[1]]

  result <- UKEnergy2000tidy %>%
    group_by(Country, Year, Energy.type, Last.stage) %>%
    primary_aggregates_IEA(p_industries = r_ind)
  expect_equal(result[["EX_p_IEA.ktoe"]], c(93000, 93000, 93000, 98220))
})

test_that("finaldemand_aggregates_IEA works as expected", {
  iea_result <- UKEnergy2000tidy %>%
    # Can calculate only when all entries are in same units, i.e., only when last stage is final or useful energy.
    filter(Last.stage %in% c("final", "useful")) %>%
    group_by(Country, Year, Energy.type, Last.stage) %>%
    finaldemand_aggregates_IEA()
  sut_result <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    filter(Last.stage %in% c("final", "useful")) %>%
    finaldemand_aggregates(fd_sectors = c("Residential", "Transport"))
  expect_equal(iea_result[["EX_fd_net_IEA.ktoe"]], sut_result[["EX_fd_net.ktoe"]] %>% unlist())
  expect_equal(iea_result[["EX_fd_gross_IEA.ktoe"]], sut_result[["EX_fd_gross.ktoe"]] %>% unlist())
})

