test_that("calc_exergy_losses_irrev() works as expected", {
  UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix") |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
    calc_exergy_losses_irrev()
})
