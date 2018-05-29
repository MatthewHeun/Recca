library(Hmisc)

###########################################################
context("Data prep utilities")
###########################################################

test_that("add_matnames and add_row_col_meta works as expected", {
  expec_path <- file.path("tests", "expectations")

  if (is_testing()) {
    # testthat sets the working directory to the folder containing this file.
    # We want the ability to use these tests interactively, too,
    # when the working directory will be the top level of this project.
    # So change the working directory if we're testing.
    # Save the current working directory, to be restored later
    currwd <- getwd()
    # Move the working directory up two levels, to the top level of this project.
    setwd(file.path("..", ".."))
  }

  expect_known_value(UKEnergy2000tidy %>% add_matnames_iea() %>% add_row_col_meta(),
                     file.path(expec_path, "expected_row_col_meta.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
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
  expec_path <- file.path("tests", "expectations")

  if (is_testing()) {
    # testthat sets the working directory to the folder containing this file.
    # We want the ability to use these tests interactively, too,
    # when the working directory will be the top level of this project.
    # So change the working directory if we're testing.
    # Save the current working directory, to be restored later
    currwd <- getwd()
    # Move the working directory up two levels, to the top level of this project.
    setwd(file.path("..", ".."))
  }

  expect_known_value(S_units_from_tidy(UKEnergy2000tidy %>% group_by(Country, Year, Energy.type, Last.stage)),
                     file.path(expec_path, "expected_S_units.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})
