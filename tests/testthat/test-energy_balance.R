###########################################################
context("SUT energy balance")
###########################################################

test_that("SUT matrix energy balance works with energy only", {
  expect_silent(
    UKEnergy2000mats %>%
      tidyr::spread(key = matrix.name, value = matrix) %>%
      dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
      verify_SUT_energy_balance()
  )
  # Try with missing R matrix
  expect_silent(
    UKEnergy2000mats %>%
      tidyr::spread(key = matrix.name, value = matrix) %>%
      dplyr::mutate(
        V = matsbyname::sum_byname(R, V),
        R = NULL
      ) %>%
      dplyr::filter(Last.stage %in% c(IEATools::last_stages$final, IEATools::last_stages$useful)) %>%
      verify_SUT_energy_balance()
  )
})


test_that("SUT matrix energy balance fails when a number has changed", {
  mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::filter(Last.stage == IEATools::last_stages$final)
  R <- mats$R[[1]]
  U <- mats$U[[1]]
  V <- mats$V[[1]]
  Y <- mats$Y[[1]]
  expect_equal(verify_SUT_energy_balance(R = R, U = U, V = V, Y = Y),
               list(.SUT_energy_balance = TRUE))
  Y[2, 2] <- 42 # Replace a 0 with a value
  expect_warning(verify_SUT_energy_balance(R = R, U = U, V = V, Y = Y),
                 "Energy not conserved")
})


test_that("SUT matrix energy balance with units works as expected", {
  result <- verify_SUT_energy_balance_with_units(
    UKEnergy2000mats %>%
      tidyr::spread(key = matrix.name, value = matrix),
    tol = 1e-3)
  expect_true(all(result$.SUT_prod_energy_balance %>% as.logical()))
  expect_true(all(result$.SUT_ind_energy_balance %>% as.logical()))

  # Now try when R is gone.
  result_noR <- verify_SUT_energy_balance_with_units(
    UKEnergy2000mats %>%
      tidyr::spread(key = matrix.name, value = matrix) %>%
      dplyr::mutate(
        V = matsbyname::sum_byname(R, V),
        R = NULL
      ),
    tol = 1e-3)
  expect_true(all(result_noR$.SUT_prod_energy_balance %>% as.logical()))
  expect_true(all(result_noR$.SUT_ind_energy_balance %>% as.logical()))
})


test_that("all SUT industries are producing energy", {
  UKspread <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  expect_silent(verify_SUT_industry_production(UKspread))
  result <- verify_SUT_industry_production(UKspread)
  expect_true(all(result[[".industry_production_OK"]] %>% as.logical()))

  # Try it when something doesn't produce energy
  R <- UKspread$R[[1]]
  U <- UKspread$U[[1]]
  V <- UKspread$V[[1]]
  expect_equal(V["Crude dist.", "Crude - Dist."], 47500)
  V["Crude dist.", "Crude - Dist."] <- 0 # Zero out production of Crude - Dist. from Crude dist.
  expect_warning(result <- verify_SUT_industry_production(R = R, U = U, V = V),
                 "There are some industries that consume but do not produce energy.")
  expect_false(result$.industry_production_OK)
  expect_equal(expected = "Crude dist.", result$.problem_industries)

  # Try when there is no R matrix
  UKspread_noR <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    )
  expect_silent(verify_SUT_industry_production(UKspread_noR))
  result_noR <- verify_SUT_industry_production(UKspread_noR)
  expect_true(all(result_noR[[".industry_production_OK"]] %>% as.logical()))
})


test_that("SUT energy balance works with single matrices", {
  UKspread <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  R <- UKspread$R[[1]]
  U <- UKspread$U[[1]]
  V <- UKspread$V[[1]]
  Y <- UKspread$Y[[1]]
  expect_silent(verify_SUT_energy_balance(R = R, U = U, V = V, Y = Y))
})


test_that("IEA energy balance works correctly", {
  # Make sure that it works.
  expect_silent(
    UKEnergy2000tidy %>%
      dplyr::group_by(Country, Year, Energy.type, Last.stage) %>%
      verify_IEATable_energy_balance(energy = "E.dot")
  )

  # Introduce something to make the energy balance fail.
  Unbalanced <- UKEnergy2000tidy
  # Change from 5e4 to 1e4
  Unbalanced$E.dot[[1]] <- 1e4
  # Now try energy balance. It should fail.
  expect_error(Unbalanced %>%
                 dplyr::group_by(Country, Year, Energy.type, Last.stage) %>%
                 verify_IEATable_energy_balance(energy = "E.dot"),
                 "Energy not balanced in verify_IEATable_energy_balance.")

})

