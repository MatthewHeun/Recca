
test_that("industry efficiencies are calculated correctly", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    calc_eta_i() %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, eta_i) %>%
    tidyr::gather(key = matnames, value = matvals, eta_i) %>%
    expand_to_tidy() %>%
    dplyr::rename(eta_i = matvals) %>%
    dplyr::mutate(
      # Make expected values
      expected = case_when(
        startsWith(rownames, "Resources - ") ~ Inf,
        Last.stage == "services" & endsWith(rownames, " dist.") ~ NA_real_,
        rownames %in% c("Cars", "Homes", "Rooms", "Trucks") ~ NA_real_,
        TRUE ~ eta_i
      )
    )
  # Check that NAs appear in the right places.
  expect_equal(result$eta_i, result$expected)

  # Test some specific values
  expect_equal(result %>% filter(Last.stage == IEATools::last_stages$final, rownames == "Crude dist.") %>% extract2("eta_i"), 0.98855359)
  expect_equal(result %>% filter(Last.stage == IEATools::last_stages$useful, rownames == "Power plants") %>% extract2("eta_i"), 0.39751553)
  expect_equal(result %>% filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$e, rownames == "Oil fields") %>% extract2("eta_i"), 0.94857713)
  expect_equal(result %>% filter(Last.stage == IEATools::last_stages$services, Energy.type == IEATools::energy_types$x, rownames == "Oil fields") %>% extract2("eta_i"), 0.94860812)
})


test_that("efficiency vectors are named correctly", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    calc_eta_i()

  # Ensure that efficiency column is named correctly.
  for (i in 1:nrow(result)) {
    eta_i <- result$eta_i[[i]]
    expect_equal(colnames(eta_i)[1], "eta_i")
  }
})


test_that("calc_eta_pfu() works correctly", {
  wide <- primary_total_aggregates_sut <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)

  # Define primary industries
  p_industries <- c("Resources [of Crude]", "Resources [of NG]")

  primary_total_aggregates_sut <- wide %>%
    Recca::primary_aggregates(p_industries = p_industries, by = "Total") %>%
    # Get rid of unneeded matrix columns.
    dplyr::mutate(
      "{Recca::psut_cols$R}" := NULL,
      "{Recca::psut_cols$U}" := NULL,
      "{Recca::psut_cols$U_feed}" := NULL,
      "{Recca::psut_cols$U_eiou}" := NULL,
      "{Recca::psut_cols$r_eiou}" := NULL,
      "{Recca::psut_cols$V}" := NULL,
      "{Recca::psut_cols$Y}" := NULL,
      "{Recca::psut_cols$S_units}" := NULL,
      p_industries = NULL
    )

  # Define final demand sectors
  fd_sectors <- c("Residential", "Transport", "Oil fields")

  finaldemand_total_aggregates_sut <- wide %>%
    Recca::finaldemand_aggregates(fd_sectors = fd_sectors, by = "Total") %>%
    # Get rid of unneeded matrix columns.
    dplyr::mutate(
      "{Recca::psut_cols$R}" := NULL,
      "{Recca::psut_cols$U}" := NULL,
      "{Recca::psut_cols$U_feed}" := NULL,
      "{Recca::psut_cols$U_eiou}" := NULL,
      "{Recca::psut_cols$r_eiou}" := NULL,
      "{Recca::psut_cols$V}" := NULL,
      "{Recca::psut_cols$Y}" := NULL,
      "{Recca::psut_cols$S_units}" := NULL,
      fd_sectors = NULL
    )

  etas <- dplyr::full_join(primary_total_aggregates_sut,
                           finaldemand_total_aggregates_sut,
                           by = c(IEATools::iea_cols$country,
                                  IEATools::iea_cols$year,
                                  IEATools::iea_cols$energy_type,
                                  IEATools::iea_cols$last_stage)) %>%
    calc_eta_pfd()

  expect_equal(etas$eta_pfd_gross, c(0.799193548387097, 5384063619.67424, 0.279466456989247, 5097922181.12103))
  expect_equal(etas$eta_pfd_net, c(0.771505376344086, 5384063619.67343, 0.278660005376344, 5097922181.12023))
})


test_that("calc_eta_pfd() works with the output from chop_Y()", {
  psut_mats <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
  p_industries <- c("Resources - Crude", "Resources - NG")
  fd_sectors <- c("Residential", "Transport", "Oil fields")
  chop_Y_aggs <- psut_mats %>%
    Recca::chop_Y(p_industries = p_industries, fd_sectors = fd_sectors, unnest = TRUE)
  etas <- chop_Y_aggs %>%
    calc_eta_pfd()
  # Make sure expected efficiency columns are present
  expect_true(Recca::efficiency_cols$eta_pfd_gross %in% names(etas))
  expect_true(Recca::efficiency_cols$eta_pfd_net %in% names(etas))
})



