test_that("pfu_aggregates() works as expected", {
  # Define primary industries
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  pfu_aggs_total <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, by = "Total")

  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Final") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(93000)
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Useful") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(93000)
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(93000)
  pfu_aggs_total |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(98220)

  # Primary PRODUCT aggregates
  pfu_aggs_product <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, by = "Product")

  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Final") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(50000, 43000), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Useful") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(50000, 43000), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(50000, 43000), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_product |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(53500, 44720), ncol = 1, dimnames = list(c("Crude", "NG"), "Industry")) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))

  # Primary INDUSTRY aggregates
  pfu_aggs_industry <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, by = "Industry")

  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Final") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(50000, 43000), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Useful") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(50000, 43000), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(50000, 43000), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
  pfu_aggs_industry |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(matrix(c(53500, 44720), nrow = 1, dimnames = list("Product", c("Resources [of Crude]", "Resources [of NG]"))) |>
                   matsbyname::setrowtype("Product") |> matsbyname::setcoltype("Industry"))
})


test_that("pfu_aggregates() works when last_stage = 'Useful' is the only available option", {
  # This test hits one line of code where we create
  # the outgoing list if last_stage = "Final" is not available.
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  pfu_aggs_total <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    # Eliminate the case when last_stage == "Final"
    dplyr::filter(.data[[Recca::psut_cols$last_stage]] != "Final") |>
    pfu_aggregates(p_industries = p_industries, by = "Total")
  expect_equal(nrow(pfu_aggs_total), 3)
})
