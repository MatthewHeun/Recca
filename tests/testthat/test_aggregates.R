library(dplyr)
library(IEATools)
library(magrittr)
library(matsbyname)
library(matsindf)
library(tidyr)

###########################################################
context("Aggregates")
###########################################################

test_that("primary aggregates of SUT data work as expected", {
  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Total",
                       aggregate_primary = "EX_total_agg.ktoe")
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$useful) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "E", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "X", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select("EX_total_agg.ktoe"), 98220)

  # Primary PRODUCT aggregates
  primary_product_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Product",
                       aggregate_primary = "EX_product_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_product_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_product_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(primary_product_aggregates_sut %>% dplyr::filter(Energy.type == "E" & rownames == "Crude") %>%
                      dplyr::select(matvals) %>% unlist(), rep(50000, 3))
  expect_equivalent(primary_product_aggregates_sut %>% dplyr::filter(Energy.type == "E" & rownames == "NG") %>%
                      dplyr::select(matvals) %>% unlist(), rep(43000, 3))
  expect_equivalent(primary_product_aggregates_sut %>% dplyr::filter(Energy.type == "X" & rownames == "Crude") %>%
                      dplyr::select(matvals) %>% unlist(), 53500)
  expect_equivalent(primary_product_aggregates_sut %>% dplyr::filter(Energy.type == "X" & rownames == "NG") %>%
                      dplyr::select(matvals) %>% unlist(), 44720)

  # Primary FLOW aggregates
  primary_flow_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    primary_aggregates(p_industries = p_industries, by = "Flow",
                       aggregate_primary = "EX_flow_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_flow_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_flow_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "E" & colnames == "Resources - Crude") %>%
                      dplyr::select(matvals) %>% unlist(), rep(50000, 3))
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "E" & colnames == "Resources - NG") %>%
                      dplyr::select(matvals) %>% unlist(), rep(43000, 3))
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "X" & colnames == "Resources - Crude") %>%
                      dplyr::select(matvals) %>% unlist(), 53500)
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "X" & colnames == "Resources - NG") %>%
                      dplyr::select(matvals) %>% unlist(), 44720)
})

test_that("final demand aggregates of SUT data work as expected", {
  # Define final demand sectors
  fd_sectors <- c("Residential", "Transport")

  # Final demand TOTAL aggregates
  final_demand_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Total",
                       net_aggregate_demand = "EX_total_net_agg.ktoe",
                       gross_aggregate_demand = "EX_total_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_total_gross_agg.ktoe", colnames == "ktoe") %>%
                      dplyr::select(matvals) %>% unlist(),
                    83275)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_total_net_agg.ktoe", colnames == "ktoe") %>%
                      dplyr::select(matvals) %>% unlist(),
                    25915.380499999999)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "E", matnames == "EX_total_net_agg.ktoe", colnames == "lumen-hrs/yr") %>%
                      dplyr::select(matvals) %>% unlist(),
                    5e14)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "X", matnames == "EX_total_net_agg.ktoe", colnames == "tonne-km/yr") %>%
                      dplyr::select(matvals) %>% unlist(),
                    142916629629)

  # Final demand PRODUCT aggregates
  final_demand_product_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Product",
                                      net_aggregate_demand = "EX_product_net_agg.ktoe",
                                      gross_aggregate_demand = "EX_product_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_product_gross_agg.ktoe", rownames == "Crude - Dist.") %>%
                      dplyr::select(matvals) %>% unlist(),
                    500)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_product_net_agg.ktoe", rownames == "Elect - Grid") %>%
                      dplyr::select(matvals) %>% unlist(),
                    6000)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_product_gross_agg.ktoe", rownames == "LTH") %>%
                      dplyr::select(matvals) %>% unlist(),
                    20000)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_product_net_agg.ktoe", rownames == "MD - Car engines") %>%
                      dplyr::select(matvals) %>% unlist(),
                    3000.4)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "E", matnames == "EX_product_net_agg.ktoe", rownames == "Freight [tonne-km/year]") %>%
                      dplyr::select(matvals) %>% unlist(),
                    142916629629)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "X", matnames == "EX_product_gross_agg.ktoe", rownames == "Space heating [m3-K]") %>%
                      dplyr::select(matvals) %>% unlist(),
                    7.500000e+10)

  # Final demand SECTOR aggregates
  final_demand_sector_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    finaldemand_aggregates_with_units(fd_sectors = fd_sectors, by = "Sector",
                                      net_aggregate_demand = "EX_sector_net_agg.ktoe",
                                      gross_aggregate_demand = "EX_sector_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_sector_net_agg.ktoe, EX_sector_gross_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_sector_net_agg.ktoe, EX_sector_gross_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_sector_gross_agg.ktoe", rownames == "Crude dist.") %>%
                      dplyr::select(matvals) %>% unlist(),
                    550)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_sector_net_agg.ktoe", rownames == "Residential") %>%
                      dplyr::select(matvals) %>% unlist(),
                    31000)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_sector_gross_agg.ktoe", rownames == "Transport") %>%
                      dplyr::select(matvals) %>% unlist(),
                    21714.9805)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_sector_net_agg.ktoe", rownames == "Residential") %>%
                      dplyr::select(matvals) %>% unlist(),
                    4200.4)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "E", matnames == "EX_sector_gross_agg.ktoe", rownames == "Gas wells & proc.") %>%
                      dplyr::select(matvals) %>% unlist(),
                    75)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "X", matnames == "EX_sector_net_agg.ktoe", rownames == "Transport", colnames == "tonne-km/yr") %>%
                      dplyr::select(matvals) %>% unlist(),
                    142916629629)
})

test_that("primary_aggregates_IEA works as expected", {
  # Get a vector of primary industries for the example data set.
  # The vector of primary industries comes from the resource matrix (R).
  r_ind <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    extract2("R") %>%
    extract2(1) %>%
    rownames()

  result <- UKEnergy2000tidy %>%
    dplyr::group_by(Country, Year, Energy.type, Last.stage) %>%
    primary_aggregates_IEA(p_industries = r_ind)
  expect_equal(result[["EX_p_IEA.ktoe"]], c(93000, 93000, 93000, 98220))
})

test_that("finaldemand_aggregates_IEA works as expected", {
  iea_result <- UKEnergy2000tidy %>%
    # Can calculate only when all entries are in same units, i.e., only when last stage is final or useful energy.
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    dplyr::group_by(Country, Year, Energy.type, Last.stage) %>%
    finaldemand_aggregates_IEA()
  sut_result <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    finaldemand_aggregates(fd_sectors = c("Residential", "Transport"))
  expect_equal(iea_result[["EX_fd_net_IEA.ktoe"]], sut_result[["EX_fd_net.ktoe"]] %>% unlist())
  expect_equal(iea_result[["EX_fd_gross_IEA.ktoe"]], sut_result[["EX_fd_gross.ktoe"]] %>% unlist())
})

test_that("finaldemand_aggregates works for sectors", {
  sut_result <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(Last.stage %in% c("final", "useful")) %>%
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

