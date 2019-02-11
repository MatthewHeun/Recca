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

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Total",
                       aggregate_primary = "EX_total_agg.ktoe")
  expect_equivalent(primary_total_aggregates_sut %>% filter(Last.stage == "final") %>%
                      select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% filter(Last.stage == "useful") %>%
                      select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% filter(Energy.type == "E.ktoe", Last.stage == "services") %>%
                      select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% filter(Energy.type == "X.ktoe", Last.stage == "services") %>%
                      select("EX_total_agg.ktoe"), 98220)

  # Primary PRODUCT aggregates
  primary_product_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Product",
                       aggregate_primary = "EX_product_agg.ktoe") %>%
    select(Country, Year, Last.stage, Energy.type, EX_product_agg.ktoe) %>%
    gather(key = "matnames", value = "matvals", EX_product_agg.ktoe) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "E.ktoe" & rownames == "Crude") %>% select(matvals) %>% unlist(), rep(50000, 3))
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "E.ktoe" & rownames == "NG") %>% select(matvals) %>% unlist(), rep(43000, 3))
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "X.ktoe" & rownames == "Crude") %>% select(matvals) %>% unlist(), 53500)
  expect_equivalent(primary_product_aggregates_sut %>% filter(Energy.type == "X.ktoe" & rownames == "NG") %>% select(matvals) %>% unlist(), 44720)

  # Primary FLOW aggregates
  primary_flow_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Flow",
                       aggregate_primary = "EX_flow_agg.ktoe") %>%
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

  # Final demand TOTAL aggregates
  final_demand_total_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Total",
                       net_aggregate_demand = "EX_total_net_agg.ktoe",
                       gross_aggregate_demand = "EX_total_gross_agg.ktoe") %>%
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

  # Final demand PRODUCT aggregates
  final_demand_product_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Product",
                                      net_aggregate_demand = "EX_product_net_agg.ktoe",
                                      gross_aggregate_demand = "EX_product_gross_agg.ktoe") %>%
    select(Country, Year, Last.stage, Energy.type, EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    gather(key = "matnames", value = "matvals", EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "EX_product_gross_agg.ktoe", rownames == "Crude - Dist.") %>% select(matvals) %>% unlist(),
                    500)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "EX_product_net_agg.ktoe", rownames == "Elect - Grid") %>% select(matvals) %>% unlist(),
                    6000)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      filter(Last.stage == "useful", Energy.type == "E.ktoe", matnames == "EX_product_gross_agg.ktoe", rownames == "LTH") %>% select(matvals) %>% unlist(),
                    20000)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      filter(Last.stage == "useful", Energy.type == "E.ktoe", matnames == "EX_product_net_agg.ktoe", rownames == "MD - Car engines") %>% select(matvals) %>% unlist(),
                    3000.4)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      filter(Last.stage == "services", Energy.type == "E.ktoe", matnames == "EX_product_net_agg.ktoe", rownames == "Freight [tonne-km/year]") %>% select(matvals) %>% unlist(),
                    142916629629)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "EX_product_gross_agg.ktoe", rownames == "Space heating [m3-K]") %>% select(matvals) %>% unlist(),
                    7.500000e+10)

  # Final demand SECTOR aggregates
  final_demand_sector_aggregates_sut <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Sector",
                                      net_aggregate_demand = "EX_sector_net_agg.ktoe",
                                      gross_aggregate_demand = "EX_sector_gross_agg.ktoe") %>%
    select(Country, Year, Last.stage, Energy.type, EX_sector_net_agg.ktoe, EX_sector_gross_agg.ktoe) %>%
    gather(key = "matnames", value = "matvals", EX_sector_net_agg.ktoe, EX_sector_gross_agg.ktoe) %>%
    expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "EX_sector_gross_agg.ktoe", rownames == "Crude dist.") %>% select(matvals) %>% unlist(),
                    550)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      filter(Last.stage == "final", Energy.type == "E.ktoe", matnames == "EX_sector_net_agg.ktoe", rownames == "Residential") %>% select(matvals) %>% unlist(),
                    31000)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      filter(Last.stage == "useful", Energy.type == "E.ktoe", matnames == "EX_sector_gross_agg.ktoe", rownames == "Transport") %>% select(matvals) %>% unlist(),
                    21714.9805)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      filter(Last.stage == "useful", Energy.type == "E.ktoe", matnames == "EX_sector_net_agg.ktoe", rownames == "Residential") %>% select(matvals) %>% unlist(),
                    4200.4)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      filter(Last.stage == "services", Energy.type == "E.ktoe", matnames == "EX_sector_gross_agg.ktoe", rownames == "Gas wells & proc.") %>% select(matvals) %>% unlist(),
                    75)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      filter(Last.stage == "services", Energy.type == "X.ktoe", matnames == "EX_sector_net_agg.ktoe", rownames == "Transport", colnames == "tonne-km/yr") %>% select(matvals) %>% unlist(),
                    142916629629)
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

test_that("finaldemand_aggregates works for sectors", {
  sut_result <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    filter(Last.stage %in% c("final", "useful")) %>%
    finaldemand_aggregates(fd_sectors = c("Residential", "Transport"), by = "Sector")
  expect_equal(sut_result$EX_fd_net.ktoe[[1]][1,1], 31000)
  expect_equal(sut_result$EX_fd_net.ktoe[[1]][2,1], 40750)
  expect_equal(sut_result$EX_fd_net.ktoe[[2]][1,1], 4200.4)
  expect_equal(sut_result$EX_fd_net.ktoe[[2]][2,1], 21714.9805)

  expect_equal(sut_result$EX_fd_gross.ktoe[[1]][1,1], 550)
  expect_equal(sut_result$EX_fd_gross.ktoe[[1]][2,1], 350)
  expect_equal(sut_result$EX_fd_gross.ktoe[[1]][4,1], 2075)
  expect_equal(sut_result$EX_fd_gross.ktoe[[1]][11,1], 40750)

  expect_equal(sut_result$EX_fd_gross.ktoe[[2]][1,1], 0)
  expect_equal(sut_result$EX_fd_gross.ktoe[[2]][2,1], 45)
  expect_equal(sut_result$EX_fd_gross.ktoe[[2]][6,1], 75)
  expect_equal(sut_result$EX_fd_gross.ktoe[[2]][11,1], 26.9997)
  expect_equal(sut_result$EX_fd_gross.ktoe[[2]][14,1], 21714.9805)
})

