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
  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  # Primary total aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Total",
                       aggregate_primary_colname = "EX_total_agg.ktoe")
  expect_equivalent(primary_total_aggregates_sut %>% filter(Last.stage == "final") %>%
                      select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% filter(Last.stage == "useful") %>%
                      select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% filter(Energy.type == "E.ktoe", Last.stage == "services") %>%
                      select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% filter(Energy.type == "X.ktoe", Last.stage == "services") %>%
                      select("EX_total_agg.ktoe"), 98220)

  # Primary product aggregates
  primary_product_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Product",
                       aggregate_primary_colname = "EX_product_agg.ktoe") %>%
    select(Country, Year, Last.stage, Energy.type, EX_product_agg.ktoe) %>%
    gather(key = "matnames", value = "matvals", EX_product_agg.ktoe) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "E.ktoe" & rownames == "Crude") %>% select(matvals) %>% unlist(), rep(50000, 3))
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "E.ktoe" & rownames == "NG") %>% select(matvals) %>% unlist(), rep(43000, 3))
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "X.ktoe" & rownames == "Crude") %>% select(matvals) %>% unlist(), 53500)
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "X.ktoe" & rownames == "NG") %>% select(matvals) %>% unlist(), 44720)

  # Primary flow aggregates
  primary_flow_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Flow",
                       aggregate_primary_colname = "EX_flow_agg.ktoe") %>%
    select(Country, Year, Last.stage, Energy.type, EX_flow_agg.ktoe) %>%
    gather(key = "matnames", value = "matvals", EX_flow_agg.ktoe) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(primary_flow_aggregates_sut %>% filter(Energy.type == "E.ktoe" & colnames == "Resources - Crude") %>% select(matvals) %>% unlist(), rep(50000, 3))
  expect_equivalent(primary_flow_aggregates_sut %>% filter(Energy.type == "E.ktoe" & colnames == "Resources - NG") %>% select(matvals) %>% unlist(), rep(43000, 3))
  expect_equivalent(primary_flow_aggregates_sut %>% filter(Energy.type == "X.ktoe" & colnames == "Resources - Crude") %>% select(matvals) %>% unlist(), 53500)
  expect_equivalent(primary_flow_aggregates_sut %>% filter(Energy.type == "X.ktoe" & colnames == "Resources - NG") %>% select(matvals) %>% unlist(), 44720)
})

test_that("final demand aggregates of SUT data work as expected", {
  # Define final demand sectors
  fd_sectors <- c("Residential", "Transport")

  # Total final demand aggregates
  final_demand_total_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Total",
                       net_aggregate_demand_colname = "EX_total_net_agg.ktoe",
                       gross_aggregate_demand_colname = "EX_total_gross_agg.ktoe") %>%
    select(Country, Year, Last.stage, Energy.type, EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe) %>%
    gather(key = "matnames", value = "matvals", EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "EX_total_gross_agg.ktoe", colnames == "ktoe") %>% select(matvals) %>% unlist(),
                    83275)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      filter(Last.stage == "useful", Energy.type == "E.ktoe", matnames == "EX_total_net_agg.ktoe", colnames == "ktoe") %>% select(matvals) %>% unlist(),
                    25915.380499999999)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      filter(Last.stage == "services", Energy.type == "E.ktoe", matnames == "EX_total_net_agg.ktoe", colnames == "lumen-hrs/yr") %>% select(matvals) %>% unlist(),
                    5e14)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "EX_total_net_agg.ktoe", colnames == "tonne-km/yr") %>% select(matvals) %>% unlist(),
                    142916629629)

  # Final demand product aggregates
  final_demand_product_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Product",
                                      net_aggregate_demand_colname = "EX_product_net_agg.ktoe",
                                      gross_aggregate_demand_colname = "EX_product_gross_agg.ktoe")
  Recca:::test_against_file(final_demand_product_aggregates_sut,
                            "expected_final_demand_product_aggregates_sut.rds",
                            update = FALSE)

  # Final demand sector aggregates
  final_demand_sector_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Sector",
                                      net_aggregate_demand_colname = "EX_sector_net_agg.ktoe",
                                      gross_aggregate_demand_colname = "EX_sector_gross_agg.ktoe")
  Recca:::test_against_file(final_demand_sector_aggregates_sut,
                            "expected_final_demand_sector_aggregates_sut.rds",
                            update = FALSE)
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

