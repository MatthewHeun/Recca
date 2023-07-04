test_that("pfu_aggregates() works as expected", {
  # Define primary industries
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  pfu_aggs <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates(p_industries = p_industries, by = "Total")
  pfu_aggs |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Final") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(93000)
  pfu_aggs |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Useful") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(93000)
  pfu_aggs |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "E",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(93000)
  # To get this one to work,
  # need to calculate primary aggregates when last_stage == "Services"
  pfu_aggs |>
    dplyr::filter(.data[[Recca::psut_cols$energy_type]] == "X",
                  .data[[Recca::psut_cols$last_stage]] == "Services") |>
    magrittr::extract2(Recca::aggregate_cols$gross_aggregate_primary) |>
    magrittr::extract2(1) |>
    expect_equal(93000)
})
