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
    Recca::primary_aggregates(p_industries = "p_industries", by = "Total", aggregate_primary = "EX_total_agg.ktoe")
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
    Recca::primary_aggregates(p_industries = "p_industries", by = "Product",
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
    Recca::primary_aggregates(p_industries = "p_industries", by = "Flow",
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


test_that("primary aggregates works with leading pattern for names", {
  # Define primary industries
  p_industries <- c("Resources", "Resources")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      p_industries = rep(list(p_industries), times = nrow(.))
    ) %>%
    Recca::primary_aggregates(p_industries = "p_industries", pattern_type = "leading", by = "Total", aggregate_primary = "EX_total_agg.ktoe")
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$useful) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "E", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "X", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select("EX_total_agg.ktoe"), 98220)
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
    Recca::primary_aggregates(p_industries = "p_industries", by = "Total",
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
    Recca::primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = Recca::aggregate_cols$aggregate_primary)
  expect_equivalent(primary_total_aggregates_both %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select(Recca::aggregate_cols$aggregate_primary), 93000)


  # Primary TOTAL aggregates counting crude oil only
  primary_total_aggregates_crude <- UKEnergy2000mats_wide %>%
    dplyr::mutate(
      p_industries = p_industries_crude
    ) %>%
    Recca::primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = Recca::aggregate_cols$aggregate_primary)
  expect_equivalent(primary_total_aggregates_crude %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select(Recca::aggregate_cols$aggregate_primary), 50000)

  # Primary TOTAL aggregates counting NG only
  primary_total_aggregates_ng <- UKEnergy2000mats_wide %>%
    dplyr::mutate(
      p_industries = p_industries_ng
    ) %>%
    Recca::primary_aggregates(p_industries = "p_industries", by = "Total",
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
    Recca::primary_aggregates(p_industries = "p_industries", by = "Total",
                       aggregate_primary = Recca::aggregate_cols$aggregate_primary)
  # Check first row (should have both primary industries and have 93000 for the aggregate primary energy)
  expect_equal(res[[Recca::aggregate_cols$aggregate_primary]][[1]], 93000)
  # Check second row (should have only crude and have 50000 for the aggregate primary energy)
  expect_equal(res[[Recca::aggregate_cols$aggregate_primary]][[2]], 50000)
  # Check third row (should have only NG and have 43000 for the aggregate primary energy)
  expect_equal(res[[Recca::aggregate_cols$aggregate_primary]][[3]], 43000)
})


test_that("primary_aggregates() works for net and gross", {
  # Define primary industries
  p_industries <- c("Resources - Crude", "Resources - NG")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      p_industries = rep(list(p_industries), times = nrow(.))
    ) %>%
    Recca::primary_aggregates(p_industries = "p_industries",
                              add_net_gross_cols = TRUE,
                              by = "Total")
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select(Recca::aggregate_cols$net_aggregate_primary), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select(Recca::aggregate_cols$gross_aggregate_primary), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$useful) %>%
                      dplyr::select(Recca::aggregate_cols$net_aggregate_primary), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$useful) %>%
                      dplyr::select(Recca::aggregate_cols$gross_aggregate_primary), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "E", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select(Recca::aggregate_cols$net_aggregate_primary), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "E", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select(Recca::aggregate_cols$gross_aggregate_primary), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "X", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select(Recca::aggregate_cols$net_aggregate_primary), 98220)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "X", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select(Recca::aggregate_cols$gross_aggregate_primary), 98220)
})


test_that("finaldemand_aggregates() of SUT data work as expected without units", {
  # Define final demand sectors
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  # Final demand TOTAL aggregates
  final_demand_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      fd_sectors = rep(list(fd_sectors), times = nrow(.))
    ) %>%
    Recca::finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Total",
                           net_aggregate_demand = "EX_total_net_agg.ktoe",
                           gross_aggregate_demand = "EX_total_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, fd_sectors, EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_total_gross_agg.ktoe") %>%
                      dplyr::select(matvals) %>% unlist(),
                    74325)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_total_net_agg.ktoe") %>%
                      dplyr::select(matvals) %>% unlist(),
                    25915.380499999999)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "E", matnames == "EX_total_net_agg.ktoe") %>%
                      dplyr::select(matvals) %>% unlist(),
                    5.007179166e+14)

  # Final demand PRODUCT aggregates
  final_demand_product_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      fd_sectors = rep(list(fd_sectors), times = nrow(.))
    ) %>%
    Recca::finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Product",
                           net_aggregate_demand = "EX_product_net_agg.ktoe",
                           gross_aggregate_demand = "EX_product_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_product_gross_agg.ktoe", rownames == "Crude - Fields") %>%
                      dplyr::select(matvals) %>% unlist(),
                    2500)
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
    Recca::finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Sector",
                           net_aggregate_demand = "EX_sector_net_agg.ktoe",
                           gross_aggregate_demand = "EX_sector_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_sector_net_agg.ktoe, EX_sector_gross_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_sector_net_agg.ktoe, EX_sector_gross_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_sector_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_sector_gross_agg.ktoe", rownames == "Oil fields") %>%
                      dplyr::select(matvals) %>% unlist(),
                    2575)
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
                      dplyr::filter(Last.stage == "Services", Energy.type == "E", matnames == "EX_sector_gross_agg.ktoe", rownames == "Oil fields") %>%
                      dplyr::select(matvals) %>% unlist(),
                    75)
})


test_that("IEATools::primary_aggregates_IEA() works as expected on the UKEnergy2000tidy data frame", {
  result <- UKEnergy2000tidy %>%
    dplyr::mutate(
      Method = "PCM"
    ) %>%
    dplyr::group_by(Country, Method, Year, Energy.type, Last.stage) %>%
    IEATools::primary_aggregates()
  expect_equal(result[[Recca::aggregate_cols$aggregate_primary]], c(93000, 93000, 93000, 98220))
})


test_that("finaldemand_aggregates works for sectors", {
  sut_result <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      fd_sectors = rep(list(c("Residential", "Transport", "Oil fields")), times = nrow(.))
    ) %>%
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    Recca::finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Sector")
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[1]][1,1], 31000)
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[1]][2,1], 40750)
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[2]][1,1], 4200.4)
  expect_equal(sut_result[[Recca::aggregate_cols$net_aggregate_demand]][[2]][2,1], 21714.9805)

  # Testing final demand by sector for the three fd_sectors at the final stage
  # Oil fields
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]][1,1], 2575)
  # Residential
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]][2,1], 31000)
  # Transport
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]][3,1], 40750)

  # Testing final demand by sector for the three fd_sectors at the useful stage
  # Oil fields
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][1,1], 75)
  # Residential
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][2,1], 4200.4)
  # Transport
  expect_equal(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[2]][3,1], 21714.9805)
})


test_that("finaldemand_aggregates works with U_EIOU", {
  sut_result <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      fd_sectors = rep(list(c("Residential", "Transport", "Oil fields")), times = nrow(.))
    ) %>%
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    Recca::finaldemand_aggregates(fd_sectors = "fd_sectors", by = "Sector")

  expect_equal(matsbyname::getrownames_byname(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]]),
               sut_result$fd_sectors[[1]] %>% sort())
})


test_that("region_aggregates() works as expected", {
  mats_GBR <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
  # Add another country, by duplicating GBR
  mats <- dplyr::bind_rows(mats_GBR,
                           mats_GBR %>% dplyr::mutate(Country = "USA"),
                           mats_GBR %>% dplyr::mutate(Country = "FRA"))
  agg_map <- list(EUR = c("GBR", "FRA"), AMR = "USA")
  agg_df <- matsbyname::agg_map_to_agg_table(agg_map,
                                             few_colname = "Continent",
                                             many_colname = IEATools::iea_cols$country)
  mats <- dplyr::left_join(mats, agg_df, by = IEATools::iea_cols$country)

  # Try with an empty data frame
  empty <- region_aggregates(mats[0, ],
                             many_colname = IEATools::iea_cols$country,
                             few_colname = "Continent")
  expect_equal(nrow(empty), 0)
  expect_equal(colnames(empty), colnames(mats))

  # Now try with the data.
  res <- region_aggregates(mats,
                           many_colname = IEATools::iea_cols$country,
                           few_colname = "Continent")
  # Verify column names
  expect_true(Recca::psut_cols$R %in% names(res))
  expect_true(Recca::psut_cols$U_eiou %in% names(res))
  expect_true(Recca::psut_cols$U_feed %in% names(res))
  expect_true(Recca::psut_cols$V %in% names(res))
  expect_true(Recca::psut_cols$Y %in% names(res))
  expect_true(Recca::psut_cols$U %in% names(res))
  expect_true(Recca::psut_cols$r_eiou %in% names(res))
  expect_true(Recca::psut_cols$S_units %in% names(res))
  # Check that the EUR rows are 2x the AMR rows
  eur <- res %>% dplyr::filter(Country == "EUR")
  amr <- res %>% dplyr::filter(Country == "AMR")
  expect_equal(eur$R, matsbyname::hadamardproduct_byname(2, amr$R))
  expect_equal(eur$U, matsbyname::hadamardproduct_byname(2, amr$U))
  expect_equal(eur$U_EIOU, matsbyname::hadamardproduct_byname(2, amr$U_EIOU))
  expect_equal(eur$U_feed, matsbyname::hadamardproduct_byname(2, amr$U_feed))
  expect_equal(eur$V, matsbyname::hadamardproduct_byname(2, amr$V))
  expect_equal(eur$Y, matsbyname::hadamardproduct_byname(2, amr$Y))
  # But some of the matrices should be same
  expect_equal(eur$S_units, amr$S_units)
  expect_equal(eur$r_EIOU, amr$r_EIOU)
})


test_that("despecified_aggregates() works as expected", {
  mats_GBR <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)

  mats_GBR %>%
    despecified_aggregates()

})
