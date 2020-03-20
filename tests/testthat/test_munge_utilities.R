###########################################################
context("Data prep utilities")
###########################################################

test_that("add_matnames works correctly with a prefixed Flow", {
  # Add an exports row to UKEnergy2000tidy
  new_row <- list(Country = "ZA", Year = 2018, Ledger.side = "Supply",
                  Flow.aggregation.point = "Total primary energy supply", Energy.type = "E.dot",
                  Last.stage = "Final", Flow = "Exports", Product = "Crude", E.dot = -42,
                  Unit = "ktoe")
  with_export <- UKEnergy2000tidy %>%
    bind_rows(new_row)
  UVY <- with_export %>% add_matnames_iea(matname = "UVY")
  n_rows <- nrow(UVY)
  # This piece of exports data should be put in the Y matrix, not in the U matrix.
  expect_equal(UVY[["UVY"]][[n_rows]], "Y")
})


test_that("add_matnames identifies resource industries correctly", {
  # Run the add_matnames_iea function with use_R = TRUE
  WithR <- UKEnergy2000tidy %>%
    dplyr::group_by(Country, Year, Energy.type, Last.stage) %>%
    IEATools::add_psut_matnames() %>%
    # add_matnames_iea(use_R = TRUE) %>%
    dplyr::filter(matnames == "R")
  # We expect that every flow from a "Resources - *" industry will end up in the R matrix.
  Expected <- UKEnergy2000tidy %>%
    dplyr::filter(startsWith_any_of(Flow, "Resources - ")) %>%
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
  expect_error(verify_cols_missing(df, newcols),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
  # Try with names
  newcolnames <- lapply(newcols, as.name)
  expect_error(verify_cols_missing(df, newcolnames),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
})


test_that("verify_cols_missing works with a single value", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  expect_silent(verify_cols_missing(df, as.name("c")))
  expect_error(verify_cols_missing(df, as.name("a")),
               Hmisc::escapeRegex("column(s) 'a' is (are) already column names in data frame 'df'"))
})


test_that("S_units_from_tidy works as expected", {

  S_units_expanded <- UKEnergy2000tidy %>%
    group_by(Country, Year, Energy.type, Last.stage) %>%
    S_units_from_tidy() %>%
    gather(key = "matnames", value = "matvals", S_units) %>%
    expand_to_tidy()
  expect_equivalent(S_units_expanded %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "final", rownames == "Crude", colnames == "ktoe") %>% select(matvals) %>% unlist(),
                    1)
  expect_equivalent(S_units_expanded %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", rownames == "Crude", colnames == "lumen-hrs/yr") %>% select(matvals) %>% unlist(),
                    0)
  expect_equivalent(S_units_expanded %>%
                      filter(Energy.type == "E.ktoe", Last.stage == "services", rownames == "Freight [tonne-km/year]", colnames == "tonne-km/yr") %>% select(matvals) %>% unlist(),
                    1)
  expect_equivalent(S_units_expanded %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", rownames == "MD - Car engines", colnames == "passenger-km/yr") %>% select(matvals) %>% unlist(),
                    0)
  expect_equivalent(S_units_expanded %>%
                      filter(Energy.type == "X.ktoe", Last.stage == "services", rownames == "Space heating [m3-K]", colnames == "tonne-km/yr") %>% select(matvals) %>% unlist(),
                    0)
})
