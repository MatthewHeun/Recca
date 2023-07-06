test_that("pfu_aggregates() works for primary aggregates", {
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  sep <- Recca::all_stages$last_stage_sep

  # Primary TOTAL aggregates
  pfu_aggs_total <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Total")

  for (ls in c(Recca::all_stages$final, Recca::all_stages$useful, Recca::all_stages$services)) {
    pfu_aggs_total |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
      magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_primary, sep, ls)) |>
      magrittr::extract2(1) |>
      expect_equal(93000)
    pfu_aggs_total |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
      magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_primary, sep, ls)) |>
      magrittr::extract2(1) |>
      expect_equal(93000)
  }
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_primary, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(98220)
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_primary, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(98220)

  # Primary PRODUCT aggregates
  pfu_aggs_product <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Product")

  for (ls in c(Recca::all_stages$final, Recca::all_stages$useful, Recca::all_stages$services)) {
    pfu_aggs_product |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
      magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_primary, sep, ls)) |>
      magrittr::extract2(1) |>
      expect_equal(matrix(c(50000, 43000), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                     matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
    pfu_aggs_product |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
      magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_primary, sep, ls)) |>
      magrittr::extract2(1) |>
      expect_equal(matrix(c(50000, 43000), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                     matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  }
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_primary, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(53500, 44720), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_primary, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(53500, 44720), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))

  # Primary INDUSTRY aggregates
  pfu_aggs_industry <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Industry")

  for (ls in c(Recca::all_stages$final, Recca::all_stages$useful, Recca::all_stages$services)) {
    pfu_aggs_industry |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
      magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_primary, sep, ls)) |>
      magrittr::extract2(1) |>
      expect_equal(matrix(c(50000, 43000), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                     matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
    pfu_aggs_industry |>
      dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
      magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_primary, sep, ls)) |>
      magrittr::extract2(1) |>
      expect_equal(matrix(c(50000, 43000), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                     matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  }
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_primary, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(53500, 44720), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_primary, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(53500, 44720), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
})


test_that("pfu_aggregates() works when last_stage = 'Useful' is the only available option", {
  # This test hits one line of code where we create
  # the outgoing list if last_stage = "Final" is not available.
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  # Primary TOTAL aggregates
  pfu_aggs_total <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    # Eliminate the case when last_stage == "Final"
    dplyr::filter(.data[[Recca::psut_cols$last_stage]] != "Final") |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Total")
  expect_equal(nrow(pfu_aggs_total), 2)
})


test_that("pfu_aggregates() works for final aggregates", {
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  sep <- Recca::all_stages$last_stage_sep

  # Final TOTAL aggregates
  pfu_aggs_total <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Total")

  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_final, sep, Recca::all_stages$final)) |>
    magrittr::extract2(1) |>
    expect_equal(71750)
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_final, sep, Recca::all_stages$final)) |>
    magrittr::extract2(1) |>
    expect_equal(74325)

  # Final PRODUCT aggregates
  pfu_aggs_product <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Product")

  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_final, sep, Recca::all_stages$final)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(14750,
                          6000,
                          25000,
                          26000), ncol = 1, dimnames = list(c("Diesel [from Dist.]",
                                                              "Elect [from Grid]",
                                                              "NG [from Dist.]",
                                                              "Petrol [from Dist.]"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_final, sep, Recca::all_stages$final)) |>
    magrittr::extract2(1) |>
    matsbyname::clean_byname() |>
    expect_equal(matrix(c(2500,
                          14800,
                          6025,
                          25000,
                          26000), ncol = 1, dimnames = list(c("Crude [from Fields]",
                                                              "Diesel [from Dist.]",
                                                              "Elect [from Grid]",
                                                              "NG [from Dist.]",
                                                              "Petrol [from Dist.]"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))

  # Final INDUSTRY aggregates
  pfu_aggs_industry <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Industry")
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_final, sep, Recca::all_stages$final)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(31000, 40750), nrow = 1, dimnames = list("Product", c("Residential", "Transport"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_final, sep, Recca::all_stages$final)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(2575, 31000, 40750), nrow = 1,
                        dimnames = list("Product", c("Oil fields", "Residential", "Transport"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
})


test_that("pfu_aggregates() works for useful aggregates", {
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  sep <- Recca::all_stages$last_stage_sep

  # Useful TOTAL aggregates
  pfu_aggs_total <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Total")

  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_useful, sep, Recca::all_stages$useful)) |>
    magrittr::extract2(1) |>
    expect_equal(25915.3805)
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_useful, sep, Recca::all_stages$useful)) |>
    magrittr::extract2(1) |>
    expect_equal(25990.3805)

  # Useful PRODUCT aggregates
  pfu_aggs_product <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Product")

  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_useful, sep, Recca::all_stages$useful)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(1200,
                          20000,
                          3000.4,
                          1714.9805), ncol = 1, dimnames = list(c("Light",
                                                                  "LTH",
                                                                  "MD [from Car engines]",
                                                                  "MD [from Truck engines]"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_useful, sep, Recca::all_stages$useful)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(50,
                          25,
                          1200,
                          20000,
                          3000.4,
                          1714.9805), ncol = 1, dimnames = list(c("Diesel [from Dist.]",
                                                                "Elect [from Grid]",
                                                                "Light",
                                                                "LTH",
                                                                "MD [from Car engines]",
                                                                "MD [from Truck engines]"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))

  # Useful INDUSTRY aggregates
  pfu_aggs_industry <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Industry")

  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_useful, sep, Recca::all_stages$useful)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(4200.4, 21714.9805), nrow = 1, dimnames = list("Product",
                                                                         c("Residential", "Transport"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_useful, sep, Recca::all_stages$useful)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(75, 4200.4, 21714.9805),
                        nrow = 1, dimnames = list("Product",
                                                  c("Oil fields", "Residential", "Transport"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
})


test_that("pfu_aggregates() works for services aggregates", {
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  sep <- Recca::all_stages$last_stage_sep

  # Services TOTAL aggregates
  pfu_aggs_total <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Total")

  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_services, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(5.00717916629629e14)
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_services, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(5.00717916629629e14)

  # Services PRODUCT aggregations
  pfu_aggs_product <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Product")

  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_services, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(1.42916629629e11,
                          5e14,
                          5e11,
                          7.5e10), ncol = 1, dimnames = list(c("Freight [tonne-km/year]",
                                                               "Illumination [lumen-hrs/yr]",
                                                               "Passenger [passenger-km/yr]",
                                                               "Space heating [m3-K]"),
                                                             "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_services, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(5e1,
                          2.5e1,
                          1.42916629629e11,
                          5e14,
                          5e11,
                          7.5e10), ncol = 1, dimnames = list(c("Diesel [from Dist.]",
                                                               "Elect [from Grid]",
                                                               "Freight [tonne-km/year]",
                                                               "Illumination [lumen-hrs/yr]",
                                                               "Passenger [passenger-km/yr]",
                                                               "Space heating [m3-K]"),
                                                             "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))

  # Services INDUSTRY aggregations
  pfu_aggs_industry <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, fd_sectors = fd_sectors,
                   by = "Industry")

  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$net_aggregate_services, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(5.00075e14, 642916629629), nrow = 1, dimnames = list("Product",
                                                                               c("Residential", "Transport"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E") |>
    magrittr::extract2(paste0(Recca::aggregate_cols$gross_aggregate_services, sep, Recca::all_stages$services)) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(75, 5.00075e14, 642916629629), nrow = 1, dimnames = list("Product",
                                                                               c("Oil fields", "Residential", "Transport"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
})
