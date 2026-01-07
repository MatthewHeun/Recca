test_that("endogenize_losses() works correctly", {
  pivoted_E <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
                    c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" :=
        RCLabels::make_list(Recca::balance_cols$default_losses_alloc,
                            n = dplyr::n(),
                            lenx = 1)
    )
  # Verify initial balance situation.
  pivoted_E |>
    calc_inter_industry_balance() |>
    verify_inter_industry_balance() |>
    expect_silent()
  pivoted_E |>
    calc_intra_industry_balance() |>
    verify_intra_industry_balance() |>
    expect_warning(regexp = "Industries are not balanced")

  # Endogenize the energy losses.
  endogenized <- pivoted_E |>
    calc_intra_industry_balance() |>
    endogenize_losses(replace_cols = TRUE)

  # Now test that everything remains balanced.
  endogenized |>
    calc_inter_industry_balance() |>
    verify_inter_industry_balance(delete_balance_cols_if_verified = TRUE) |>
    expect_silent()
  endogenized |>
    calc_intra_industry_balance() |>
    verify_intra_industry_balance(delete_balance_cols_if_verified = TRUE) |>
    expect_silent()
  # Test that expected row and column names now exist in the matrices
  for (i in 1:2) {
    expect_true(Recca::balance_cols$waste_heat %in%
                  matsbyname::getcolnames_byname(endogenized$V[[i]]))
    expect_true(Recca::balance_cols$waste_heat %in%
                  matsbyname::getrownames_byname(endogenized$Y[[i]]))
    expect_true(Recca::balance_cols$losses_sector %in%
                  matsbyname::getcolnames_byname(endogenized$Y[[i]]))
  }

  # Try again without calculating the balances first.
  # They will be calculated internally.
  endogenized2 <- pivoted_E |>
    endogenize_losses(replace_cols = TRUE)
  endogenized2 |>
    calc_intra_industry_balance() |>
    verify_intra_industry_balance(delete_balance_cols_if_verified = TRUE) |>
    expect_silent()

  # Check that columns are NOT deleted when NOT requested
  res1 <- pivoted_E |>
    calc_intra_industry_balance() |>
    endogenize_losses()
  expect_true(Recca::balance_cols$intra_industry_balance_colname %in% names(res1))
  expect_true(Recca::balance_cols$losses_alloc_colname %in% names(res1))
  expect_true("V_prime" %in% names(res1))
  expect_true("Y_prime" %in% names(res1))
  # Check that columns are deleted when requested
  res2 <- pivoted_E |>
    calc_intra_industry_balance() |>
    endogenize_losses(replace_cols = TRUE)
  expect_false(Recca::balance_cols$intra_industry_balance_colname %in% names(res2))
  expect_false(Recca::balance_cols$losses_alloc_colname %in% names(res2))
  expect_false("V_prime" %in% names(res2))
  expect_false("Y_prime" %in% names(res2))

  # Check that cleaning works as expected
  # When starting with an already-endogenized ECC,
  # endogenizing again will result in zero rows and columns.
  # Try first without cleaning.
  endogenized3 <- endogenized |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" :=
        RCLabels::make_list(matrix(1, dimnames = list("All industries", "bogus loss product")) |>
                              matsbyname::setrowtype("Industry") |>
                              matsbyname::setcoltype("Product"),
                            n = dplyr::n(),
                            lenx = 1)
    ) |>
    endogenize_losses(loss_sector = "bogus loss sector",
                      replace_cols = TRUE)
  V3 <- endogenized3$V[[1]]
  expect_true("bogus loss product" %in% colnames(V3))
  Y3 <- endogenized3$Y[[2]]
  expect_true("bogus loss product" %in% rownames(Y3))
  expect_true("bogus loss sector" %in% colnames(Y3))

  # Next, try with cleaning.
  # We should get exactly the same data frame either way.
  endogenized4 <- endogenized |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" :=
        RCLabels::make_list(matrix(1, dimnames = list("All industries", "bogus loss product")) |>
                              matsbyname::setrowtype("Industry") |>
                              matsbyname::setcoltype("Product"),
                            n = dplyr::n(),
                            lenx = 1)
    ) |>
    endogenize_losses(loss_sector = "bogus loss sector",
                      replace_cols = TRUE,
                      clean = TRUE)
  expect_equal(endogenized4, endogenized)
})


test_that("endogenize_losses() fails when not all industries are present in losses_alloc_mat", {
  losses_alloc_mat <- matrix(c(1, 1),
                             nrow = 2,
                             ncol = 1,
                             dimnames = list(c("foo", "bar"),
                                             "Waste heat")) |>
    matsbyname::setrowtype("Industry") |>
    matsbyname::setcoltype("Product")
  pivoted_E <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) |>
    dplyr::filter(.data[[IEATools::iea_cols$last_stage]] %in%
                    c(IEATools::last_stages$final, IEATools::last_stages$useful)) |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" :=
        RCLabels::make_list(losses_alloc_mat,
                            n = dplyr::n(),
                            lenx = 1)
    )
  # This should fail, because the industry names
  # (foo and bar) do not match anything in the
  # V matrix.
  pivoted_E |>
    endogenize_losses() |>
    expect_error(regexp = "Industries not same in ")

  # Test when the losses allocation matrix rows
  # do not sum to 1.
  wrong_mat <- Recca::balance_cols$default_losses_alloc_mat
  wrong_mat[1, 1] <- 42
  pivoted_E |>
    dplyr::mutate(
      "{Recca::balance_cols$losses_alloc_colname}" :=
        RCLabels::make_list(wrong_mat,
                            n = dplyr::n(),
                            lenx = 1)
    ) |>
    endogenize_losses() |>
    expect_error("Rows of the losses allocation matrix do not sum to 1")
})

