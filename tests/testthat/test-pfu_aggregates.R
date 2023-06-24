test_that("pfu_aggregates() works as expected", {
  # Define primary industries
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  # Primary TOTAL aggregates
  primary_total_aggregates_sut <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    pfu_aggregates()

})
