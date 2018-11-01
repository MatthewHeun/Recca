library(Hmisc)

###########################################################
context("Data prep utilities")
###########################################################

test_that("add_matnames and add_row_col_meta works as expected", {
  Recca:::test_against_file(UKEnergy2000tidy %>% add_matnames_iea() %>% add_row_col_meta(),
                            "expected_row_col_meta.rds",
                            update = FALSE)
})

test_that("add_matnames works correctly with a prefixed Flow", {
  # Add an exports row to UKEnergy2000tidy
  new_row <- list(Country = "ZA", Year = 2018, Ledger.side = "Supply",
                  Flow.aggregation.point = "Total primary energy supply", Energy.type = "E.ktoe",
                  Last.stage = "final", Flow = "Exports", Product = "Crude", EX.ktoe = -42,
                  Unit = "ktoe")
  with_export <- UKEnergy2000tidy %>%
    bind_rows(new_row)
  UVY <- with_export %>% add_matnames_iea(matname = "UVY")
  n_rows <- nrow(UVY)
  # This piece of exports data should be put in the Y matrix, not in the U matrix.
  expect_equal(UVY[["UVY"]][[n_rows]], "Y")
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
  Recca:::test_against_file(S_units_from_tidy(UKEnergy2000tidy %>% group_by(Country, Year, Energy.type, Last.stage)),
                            "expected_S_units.rds",
                            update = FALSE)
})
