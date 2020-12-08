library(dplyr)
library(IEATools)
library(magrittr)
library(matsbyname)
library(matsindf)
library(tidyr)


test_that("primary aggregates of SUT data work as expected", {
  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      p_industries = rep(list(p_industries), times = nrow(.))
    ) %>%
    primary_aggregates(p_industries = "p_industries", by = "Total",
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
    dplyr::mutate(
      p_industries = rep(list(p_industries), times = nrow(.))
    ) %>%
    primary_aggregates(p_industries = "p_industries", by = "Product",
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
    dplyr::mutate(
      p_industries = rep(list(p_industries), times = nrow(.))
    ) %>%
    primary_aggregates(p_industries = "p_industries", by = "Flow",
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


test_that("primary aggregates work when R is folded into V", {
  p_industries <- c("Resources - Crude", "Resources - NG")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    # Make it wide-by-matrices
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      p_industries = rep(list(p_industries), times = nrow(.)),
      V = matsbyname::sum_byname(R, V),
      R = NULL
    ) %>%
    primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = "EX_total_agg.ktoe")
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
})


test_that("primary aggregates work when p_industries is different for each row of a data frame", {
  p_industries_all <- c("Resources - Crude", "Resources - NG")
  p_industries_crude <- "Resources - Crude"
  p_industries_ng <- "Resources - NG"

  UKEnergy2000mats_wide <- UKEnergy2000mats %>%
    # Make it wide-by-matrices
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix")
  # Primary TOTAL aggregates for both primary industries
  primary_total_aggregates_both <- UKEnergy2000mats_wide %>%
    dplyr::mutate(
      p_industries = rep(list(p_industries_all), times = nrow(UKEnergy2000mats_wide))
    ) %>%
    primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = Recca::aggregate_cols$aggregate_primary)
  expect_equivalent(primary_total_aggregates_both %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select(Recca::aggregate_cols$aggregate_primary), 93000)


  # Primary TOTAL aggregates counting crude oil only
  primary_total_aggregates_crude <-UKEnergy2000mats_wide %>%
    dplyr::mutate(
      p_industries = p_industries_crude
    ) %>%
    primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = Recca::aggregate_cols$aggregate_primary)
  expect_equivalent(primary_total_aggregates_crude %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select(Recca::aggregate_cols$aggregate_primary), 50000)

  # Primary TOTAL aggregates counting NG only
  primary_total_aggregates_ng <- UKEnergy2000mats_wide %>%
    dplyr::mutate(
      p_industries = p_industries_ng
    ) %>%
    primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = Recca::aggregate_cols$aggregate_primary)
  expect_equivalent(primary_total_aggregates_ng %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select(Recca::aggregate_cols$aggregate_primary), 43000)

  # Try when the vectors of primary industries are different down the column in the data frame
  UK_final_only <- UKEnergy2000mats_wide %>%
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] == IEATools::last_stages$final)
  UK_final_3_rows <- dplyr::bind_rows(UK_final_only, UK_final_only, UK_final_only) %>%
  # Add a column containing various options for primary industries to be aggregated.
  dplyr::mutate(
    p_industries = list(p_industries_all, p_industries_crude, p_industries_ng)
  )
  res <- UK_final_3_rows %>%
    primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = Recca::aggregate_cols$aggregate_primary)
  # Check first row (should have both primary industries and have 93000 for the aggregate primary energy)
  expect_equal(res[[Recca::aggregate_cols$aggregate_primary]][[1]], 93000)
  # Check second row (should have only crude and have 50000 for the aggregate primary energy)
  expect_equal(res[[Recca::aggregate_cols$aggregate_primary]][[2]], 50000)
  # Check third row (should have only NG and have 43000 for the aggregate primary energy)
  expect_equal(res[[Recca::aggregate_cols$aggregate_primary]][[3]], 43000)
})


test_that("final demand aggregates of SUT data work as expected", {
  # Define final demand sectors
  fd_sectors <- c("Residential", "Transport")

  # Final demand TOTAL aggregates
  final_demand_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      fd_sectors = rep(list(fd_sectors), times = nrow(.))
    ) %>%
    finaldemand_aggregates_with_units(fd_sectors = "fd_sectors", by = "Total",
                       net_aggregate_demand = "EX_total_net_agg.ktoe",
                       gross_aggregate_demand = "EX_total_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, fd_sectors, EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe) %>%
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
    dplyr::mutate(
      fd_sectors = rep(list(fd_sectors), times = nrow(.))
    ) %>%
    finaldemand_aggregates_with_units(fd_sectors = "fd_sectors", by = "Product",
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
    dplyr::mutate(
      fd_sectors = rep(list(fd_sectors), times = nrow(.))
    ) %>%
    finaldemand_aggregates_with_units(fd_sectors = "fd_sectors", by = "Sector",
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


test_that("IEATools::primary_aggregates_IEA() works as expected on the UKEnergy2000tidy data frame", {
  result <- UKEnergy2000tidy %>%
    dplyr::mutate(
      Method = "PCM"
    ) %>%
    dplyr::group_by(Country, Method, Year, Energy.type, Last.stage) %>%
    IEATools::primary_aggregates()
  expect_equal(result[["E.dot"]], c(93000, 93000, 93000, 98220))
})


test_that("finaldemand_aggregates works for sectors", {
  sut_result <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      fd_sectors = rep(list(c("Residential", "Transport")), times = nrow(.))
    ) %>%
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Sector")
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[1]][1,1], 31000)
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[1]][2,1], 40750)
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[2]][1,1], 4200.4)
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[2]][2,1], 21714.9805)

  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]][1,1], 550)
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]][2,1], 350)
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]][4,1], 2075)
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]][11,1], 40750)

  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][1,1], 0)
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][2,1], 45)
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][6,1], 75)
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][11,1], 26.9997)
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][14,1], 21714.9805)
})

