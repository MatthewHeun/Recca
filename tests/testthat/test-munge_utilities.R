
test_that("add_matnames works correctly with a prefixed Flow", {
  # Add an exports row to UKEnergy2000tidy
  new_row <- list(Country = "ZA", Year = 2018, LedgerSide = "Supply",
                  FlowAggregationPoint = "Total primary energy supply", EnergyType = "Edot",
                  LastStage = "Final", Flow = "Exports", Product = "Crude", Edot = -42,
                  Unit = "ktoe")
  with_export <- UKEnergy2000tidy %>%
    dplyr::bind_rows(new_row)
  UVY <- with_export %>% IEATools::add_psut_matnames(matnames = "UVY")
  n_rows <- nrow(UVY)
  # This piece of exports data should be put in the Y matrix, not in the U matrix.
  expect_equal(UVY[["UVY"]][[n_rows]], "Y")
})


test_that("add_matnames identifies resource industries correctly", {
  WithR <- UKEnergy2000tidy %>%
    dplyr::group_by(Country, Year, EnergyType, LastStage) %>%
    IEATools::add_psut_matnames() %>%
    dplyr::filter(matnames == "R") %>%
    as.data.frame()
  # We expect that every flow from a "Resources [of *]" industry will end up in the R matrix.
  Expected <- UKEnergy2000tidy %>%
    dplyr::filter(startsWith_any_of(Flow, "Resources [of ")) %>%
    dplyr::mutate(matnames = "R")

  expect_equal(WithR, Expected)
  # Check that rowname is correct for resource rows.
  WithRmeta <- WithR %>% IEATools::add_row_col_meta()
  # Ensure that the rowname is correct
  expect_equal(WithRmeta$rownames, WithRmeta$Flow)
  # Ensure that the colname is correct
  expect_equal(WithRmeta$colnames, WithRmeta$Product)
  # Ensure that the rowtype is correct
  expect_equal(WithRmeta$rowtypes, rep("Industry", 8))
  # Ensure that the coltype is correct
  expect_equal(WithRmeta$coltypes, rep("Product", 8))
})


test_that("verify_cols_missing works when either strings or names are provided", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  # Try with strings
  newcols <- c("a", "b")
  expect_error(matsindf::verify_cols_missing(df, newcols),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
  # Try with names
  newcolnames <- lapply(newcols, as.name)
  expect_error(matsindf::verify_cols_missing(df, newcolnames),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
})


test_that("verify_cols_missing works with a single value", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  expect_silent(matsindf::verify_cols_missing(df, as.name("c")))
  expect_error(matsindf::verify_cols_missing(df, as.name("a")),
               Hmisc::escapeRegex("column(s) 'a' is (are) already column names in data frame 'df'"))
})


test_that("S_units_from_tidy works as expected", {

  S_units_expanded <- UKEnergy2000tidy %>%
    dplyr::group_by(Country, Year, EnergyType, LastStage) %>%
    S_units_from_tidy() %>%
    tidyr::gather(key = "matnames", value = "matvals", S_units) %>%
    matsindf::expand_to_tidy()
  S_units_expanded %>%
    dplyr::filter(EnergyType == IEATools::energy_types$e, LastStage == IEATools::last_stages$final, rownames == "Crude", colnames == "ktoe") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(1)
  S_units_expanded %>%
    dplyr::filter(EnergyType ==IEATools::energy_types$e, LastStage == IEATools::last_stages$services, rownames == "Crude", colnames == "lumen-hrs/yr") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0)
  S_units_expanded %>%
    dplyr::filter(EnergyType == IEATools::energy_types$e, LastStage == IEATools::last_stages$services, rownames == "Freight [tonne-km/year]", colnames == "tonne-km/yr") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(1)
  S_units_expanded %>%
    dplyr::filter(EnergyType == IEATools::energy_types$x, LastStage == IEATools::last_stages$services, rownames == "MD [from Car engines]", colnames == "passenger-km/yr") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0)
  S_units_expanded %>%
    dplyr::filter(EnergyType == IEATools::energy_types$x, LastStage == IEATools::last_stages$services, rownames == "Space heating [m3-K]", colnames == "tonne-km/yr") %>%
    dplyr::select(matvals) %>%
    unlist()|>
    unname() |>
    expect_equal(0)
})
