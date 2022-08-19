test_that("chop_Y() works as expected", {
  p_industries <- c("Resources - Crude", "Resources - NG")
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  psut_mats <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
  # Calculate aggregates
  chopped_Y <- psut_mats %>%
    Recca::chop_Y(p_industries = p_industries, fd_sectors = fd_sectors)
  expect_true(Recca::aggregate_cols$aggregates_df %in% names(chopped_Y))
  chopped_Y_unnested <- psut_mats %>%
    Recca::chop_Y(p_industries = p_industries, fd_sectors = fd_sectors, unnest = TRUE)
  expect_true(Recca::aggregate_cols$product_sector %in% names(chopped_Y_unnested))
  expect_true(Recca::aggregate_cols$aggregate_primary %in% names(chopped_Y_unnested))
  expect_true(Recca::aggregate_cols$net_aggregate_demand %in% names(chopped_Y_unnested))
  expect_true(Recca::aggregate_cols$gross_aggregate_demand %in% names(chopped_Y_unnested))
  expect_true(paste0(Recca::psut_cols$R, "_prime") %in% names(chopped_Y_unnested))
  expect_true(paste0(Recca::psut_cols$U, "_prime") %in% names(chopped_Y_unnested))
  expect_true(paste0(Recca::psut_cols$U_feed, "_prime") %in% names(chopped_Y_unnested))
  expect_true(paste0(Recca::psut_cols$U_eiou, "_prime") %in% names(chopped_Y_unnested))
  expect_true(paste0(Recca::psut_cols$r_eiou, "_prime") %in% names(chopped_Y_unnested))
  expect_true(paste0(Recca::psut_cols$V, "_prime") %in% names(chopped_Y_unnested))
  expect_true(paste0(Recca::psut_cols$Y, "_prime") %in% names(chopped_Y_unnested))
})


test_that("footprint_aggregates() works with Losses", {
  # At one point (27 July 2022) there was a problem with footprint_aggregates()
  # where it provided a NULL value for industries that were in Y but not in fd_sectors.
  # This test triggered that bug.
  p_industries <- c("Resources - Crude", "Resources - NG")
  fd_sectors <- c("Residential", "Transport")
  chop_Y_aggs <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(Last.stage %in% c(IEATools::last_stages$final)) %>%
    # Rename Residential to "Losses" to trigger the error
    dplyr::mutate(
      Y = matsbyname::setcolnames_byname(Y, c("Losses", "Transport"))
    ) %>%
    Recca::chop_Y(p_industries = p_industries,
                  fd_sectors = fd_sectors,
                  unnest = TRUE)
  # Check if any net are NULL.  None should be.
  netfd <- chop_Y_aggs[[Recca::aggregate_cols$net_aggregate_demand]]
  which_null_net <- sapply(netfd, FUN = function(this_entry) {
    is.null(this_entry)
  })
  expect_true(!any(which_null_net))
  # Also check gross
  grossfd <- chop_Y_aggs[[Recca::aggregate_cols$gross_aggregate_demand]]
  which_null_gross <- sapply(grossfd, FUN = function(this_entry) {
    is.null(this_entry)
  })
  expect_true(!any(which_null_gross))
})


test_that("effects_aggregates() works as expected", {
  p_industries <- c("Resources - Crude", "Resources - NG")
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  psut_mats <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix) %>%
    # To avoid the services rows on which NA values are obtained.
    dplyr::slice(1, 3)
  # Calculate aggregates
  effects_aggs_nested <- psut_mats %>%
    Recca::effects_aggregates(p_industries = p_industries, fd_sectors = fd_sectors)
  expect_true(Recca::aggregate_cols$aggregates_df %in% names(effects_aggs_nested))
  effects_aggs_unnested <- psut_mats %>%
    Recca::effects_aggregates(p_industries = p_industries, fd_sectors = fd_sectors, unnest = TRUE)
  expect_true(Recca::aggregate_cols$product_sector %in% names(effects_aggs_unnested))
  expect_true(Recca::aggregate_cols$aggregate_primary %in% names(effects_aggs_unnested))
  expect_true(Recca::aggregate_cols$net_aggregate_demand %in% names(effects_aggs_unnested))
  expect_true(Recca::aggregate_cols$gross_aggregate_demand %in% names(effects_aggs_unnested))
  expect_true(paste0(Recca::psut_cols$R, "_prime") %in% names(effects_aggs_unnested))
  expect_true(paste0(Recca::psut_cols$U, "_prime") %in% names(effects_aggs_unnested))
  expect_true(paste0(Recca::psut_cols$U_feed, "_prime") %in% names(effects_aggs_unnested))
  expect_true(paste0(Recca::psut_cols$U_eiou, "_prime") %in% names(effects_aggs_unnested))
  expect_true(paste0(Recca::psut_cols$r_eiou, "_prime") %in% names(effects_aggs_unnested))
  expect_true(paste0(Recca::psut_cols$V, "_prime") %in% names(effects_aggs_unnested))
  expect_true(paste0(Recca::psut_cols$Y, "_prime") %in% names(effects_aggs_unnested))
})
