library(dplyr)
library(IEATools)
library(magrittr)
library(matsbyname)
library(matsindf)
library(tidyr)


test_that("primary_aggregates() works as expected", {
  # Define primary industries
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    # Recca::primary_aggregates(p_industries = p_industries, by = "Total", aggregate_primary = "EX_total_agg.ktoe")
    Recca::primary_aggregates(p_industries = p_industries,
                              notation = RCLabels::dash_notation,
                              by = "Total", aggregate_primary = "EX_total_agg.ktoe")
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
    Recca::primary_aggregates(p_industries = p_industries, by = "Product",
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
    Recca::primary_aggregates(p_industries = p_industries, by = "Flow",
                       aggregate_primary = "EX_flow_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_flow_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_flow_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "E" & colnames == "Resources [of Crude]") %>%
                      dplyr::select(matvals) %>% unlist(), rep(50000, 3))
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "E" & colnames == "Resources [of NG]") %>%
                      dplyr::select(matvals) %>% unlist(), rep(43000, 3))
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "X" & colnames == "Resources [of Crude]") %>%
                      dplyr::select(matvals) %>% unlist(), 53500)
  expect_equivalent(primary_flow_aggregates_sut %>% dplyr::filter(Energy.type == "X" & colnames == "Resources [of NG]") %>%
                      dplyr::select(matvals) %>% unlist(), 44720)
})


test_that("primary_aggregates() works with leading pattern for names", {
  # Define primary industries
  p_industries <- c("Resources", "Resources")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    Recca::primary_aggregates(p_industries = p_industries,
                              piece = "noun",
                              notation = RCLabels::of_notation,
                              pattern_type = "leading", by = "Total", aggregate_primary = "EX_total_agg.ktoe")
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$useful) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "E", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Energy.type == "X", Last.stage == IEATools::last_stages$services) %>%
                      dplyr::select("EX_total_agg.ktoe"), 98220)
})


test_that("primary_aggregates() works when R is folded into V", {
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    # Make it wide-by-matrices
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    ) %>%
    Recca::primary_aggregates(p_industries = p_industries,
                              notation = RCLabels::dash_notation,
                              by = "Total",
                              aggregate_primary = "EX_total_agg.ktoe")
  expect_equivalent(primary_total_aggregates_sut %>% dplyr::filter(Last.stage == IEATools::last_stages$final) %>%
                      dplyr::select("EX_total_agg.ktoe"), 93000)
})


test_that("primary_aggregates() works for net and gross", {
  # Define primary industries
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    Recca::primary_aggregates(p_industries = p_industries,
                              notation = RCLabels::of_notation,
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


test_that("primary_aggregates() works with lists", {
  # Define primary industries
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  psut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  # Make a list
  arg_list <- list(R = psut$R[[1]], V = psut$V[[1]], Y = psut$Y[[1]])
  # Calculate primary aggregates
  p_aggs <- Recca::primary_aggregates(.sutdata = arg_list, p_industries = p_industries)
  expect_equal(p_aggs$EX.p, 93000)
})


test_that("finaldemand_aggregates() of SUT data work as expected without units", {
  # Define final demand sectors
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  # Final demand TOTAL aggregates
  final_demand_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Total",
                                  net_aggregate_demand = "EX_total_net_agg.ktoe",
                                  gross_aggregate_demand = "EX_total_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_total_net_agg.ktoe, EX_total_gross_agg.ktoe)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E") %>%
                      dplyr::select("EX_total_gross_agg.ktoe") %>% unlist(),
                    74325)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E") %>%
                      dplyr::select("EX_total_net_agg.ktoe") %>% unlist(),
                    25915.380499999999)
  expect_equivalent(final_demand_total_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Services", Energy.type == "E") %>%
                      dplyr::select("EX_total_net_agg.ktoe") %>% unlist(),
                    5.007179166e+14)

  # Final demand PRODUCT aggregates
  final_demand_product_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Product",
                           net_aggregate_demand = "EX_product_net_agg.ktoe",
                           gross_aggregate_demand = "EX_product_gross_agg.ktoe") %>%
    dplyr::select(Country, Year, Last.stage, Energy.type, EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    tidyr::gather(key = "matnames", value = "matvals", EX_product_net_agg.ktoe, EX_product_gross_agg.ktoe) %>%
    matsindf::expand_to_tidy(drop = 0)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_product_gross_agg.ktoe", rownames == "Crude [from Fields]") %>%
                      dplyr::select(matvals) %>% unlist(),
                    2500)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Final", Energy.type == "E", matnames == "EX_product_net_agg.ktoe", rownames == "Elect [from Grid]") %>%
                      dplyr::select(matvals) %>% unlist(),
                    6000)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_product_gross_agg.ktoe", rownames == "LTH") %>%
                      dplyr::select(matvals) %>% unlist(),
                    20000)
  expect_equivalent(final_demand_product_aggregates_sut %>%
                      dplyr::filter(Last.stage == "Useful", Energy.type == "E", matnames == "EX_product_net_agg.ktoe", rownames == "MD [from Car engines]") %>%
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
    Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Sector",
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


test_that("finaldemand_aggregates() works for sectors", {
  fd_sectors <- c("Residential", "Transport", "Oil fields")
  sut_result <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Sector")
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


test_that("finaldemand_aggregates() works with U_EIOU", {
  fd_sectors <- c("Residential", "Transport", "Oil fields")
  sut_result <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
    Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Sector")

  expect_equal(matsbyname::getrownames_byname(sut_result[[Recca::aggregate_cols$gross_aggregate_demand]][[1]]),
               fd_sectors %>% sort())
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
  # It is important that the result NOT have the Continent column.
  # If Continent is present, it will be NA when joined with other
  # data frames that have successfully aggregated by region.
  expect_equal(colnames(empty), setdiff(colnames(mats), "Continent"))

  # Try when there is no Country column.
  empty_no_country <- mats[0, ] %>%
    dplyr::mutate(
      "{Recca::psut_cols$country}" := NULL
    )
  expect_false(Recca::psut_cols$country %in% names(empty_no_country))
  expect_true("Continent" %in% names(empty_no_country))
  empty_after_region_agg <- empty_no_country |>
    region_aggregates(many_colname = IEATools::iea_cols$country,
                      few_colname = "Continent")
  expect_false("Continent" %in% names(empty_after_region_agg))
  expect_true(Recca::psut_cols$country %in% names(empty_after_region_agg))

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


test_that("region_aggregates() works with WRLD not in aggregation_map", {
  # This test approximates what happens when
  # we have the WRLD country in with other countries
  # but WRLD is not included in the aggregation map.
  mats_GBR <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
  # Add more countries, by duplicating GBR
  mats <- dplyr::bind_rows(mats_GBR,
                           mats_GBR %>% dplyr::mutate(Country = "USA"),
                           mats_GBR %>% dplyr::mutate(Country = "FRA"),
                           mats_GBR %>% dplyr::mutate(Country = "WRLD"))
  # Leave WRLD out of the agg_map.
  agg_map <- list(Europe = c("GBR", "FRA"), NoAmr = "USA")
  agg_df <- matsbyname::agg_map_to_agg_table(agg_map,
                                             few_colname = "Continent",
                                             many_colname = IEATools::iea_cols$country)
  # Results in NA values for Continent column for WRLD country
  mats <- dplyr::left_join(mats, agg_df, by = IEATools::iea_cols$country)
  mats |>
    dplyr::filter(.data[[Recca::psut_cols$country]] == "WRLD") |>
    dplyr::select("Continent") |>
    is.na() |>
    all() |>
    expect_true()

  # Now do the aggregation.
  # Unless we specify drop_na_few = TRUE,
  # we'll get NA rows for WRLD.
  res <- region_aggregates(mats,
                           many_colname = IEATools::iea_cols$country,
                           few_colname = "Continent")
  res |>
    dplyr::filter(is.na(.data[[Recca::psut_cols$country]])) |>
    nrow() |>
    expect_equal(4)

  # Try with eliminating the NA rows
  # by setting drop_na_few = TRUE.
  res2 <- region_aggregates(mats,
                           many_colname = IEATools::iea_cols$country,
                           few_colname = "Continent",
                           drop_na_few = TRUE)
  res2 |>
    dplyr::filter(is.na(.data[[Recca::psut_cols$country]])) |>
    nrow() |>
    expect_equal(0)
})


test_that("region_aggregates() works when no aggregated country is present and drop_na_few = TRUE", {
  mats_GBR <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
  # Add another country, by duplicating GBR
  mats <- dplyr::bind_rows(mats_GBR,
                           mats_GBR |> dplyr::mutate(Country = "USA"),
                           mats_GBR |> dplyr::mutate(Country = "FRA"))
  agg_map <- list(FoSUN = c("SUN", "RUS"))
  agg_df <- matsbyname::agg_map_to_agg_table(agg_map,
                                             few_colname = "Region",
                                             many_colname = IEATools::iea_cols$country)
  # Results in NA values for every entry in the region column
  mats <- dplyr::left_join(mats, agg_df, by = IEATools::iea_cols$country)
  # Now try to aggregate
  res <- region_aggregates(mats,
                           many_colname = IEATools::iea_cols$country,
                           few_colname = "Region",
                           drop_na_few = TRUE)
  expect_equal(nrow(res), 0)
})


test_that("despecified_aggregates() works as expected", {
  mats_GBR <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)

  res <- mats_GBR %>%
    despecified_aggregates()

  # Check that S_units was aggregated
  expect_true(all(res$S_units_aggregated[[1]] == 1))

  # Check a few numbers
  # Aggregated Crude
  expect_equal(res$U_aggregated[[1]][1, 1], 48000)
  # Aggregated Petrol
  expect_equal(res$U_aggregated[[1]][5, 8], 27000)
  # Aggregated Diesel
  expect_equal(res$U_aggregated[[1]][2, 2], 15850)

  expect_equal(rownames(mats_GBR$U[[1]]),
               c("Crude", "Crude [from Dist.]", "Crude [from Fields]", "Diesel", "Diesel [from Dist.]",
                 "Elect", "Elect [from Grid]", "NG", "NG [from Dist.]", "NG [from Wells]",
                 "Petrol", "Petrol [from Dist.]"))
  expect_equal(rownames(res$U_aggregated[[1]]), c("Crude", "Diesel", "Elect", "NG", "Petrol"))
})


test_that("grouped_aggregates() works as expected", {
  mats_GBR <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)

  res <- mats_GBR %>%
    grouped_aggregates(aggregation_map = list(`Oil and oil products` = c("Crude", "Diesel", "Petrol")),
                       pattern_type = "leading", margin = "Product")

  expect_equal(colnames(res$R_aggregated[[1]]), c("NG", "Oil and oil products"))
  expect_equal(rownames(res$U_aggregated[[1]]), c("Elect", "Elect [from Grid]", "NG", "NG [from Dist.]", "NG [from Wells]", "Oil and oil products"))
  expect_equal(colnames(res$V_aggregated[[1]]), c("Elect", "Elect [from Grid]", "NG [from Dist.]", "NG [from Wells]", "Oil and oil products"))
  expect_equal(rownames(res$Y_aggregated[[1]]), c("Elect [from Grid]", "NG [from Dist.]", "Oil and oil products"))

  # Check some values

  # Oil and oil products for Crude dist.
  expect_equal(res$U_aggregated[[1]][6, 1], 48025)
  # Oil and oil products for Transport
  expect_equal(res$Y_aggregated[[1]][3, 2], 40750)
})


test_that("finaldemand_aggregates() works when there are no columns with fd_sectors", {
  U_eiou <- matrix(1, dimnames = list("bogus_row", "bogus_col"))
  Y <- matrix(1, dimnames = list("bogus_row", "bogus_col"))
  res <- finaldemand_aggregates(fd_sectors = "fd_sector", piece = "noun",
                                notation = RCLabels::bracket_notation, pattern_type = "exact",
                                U_eiou = U_eiou, Y = Y, by = "Sector")
  expect_equal(res$EX.fd_net, 0)
  expect_false(is.matrix(res$EX.fd_net))
  expect_equal(res$EX.fd_gross, 0)
  expect_false(is.matrix(res$EX.fd_gross))
})
