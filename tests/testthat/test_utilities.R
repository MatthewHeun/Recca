library(Hmisc)

###########################################################
context("Utilities")
###########################################################

test_that("error messages about column names works as expected", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  newcols <- c("c", "d", "a", "b")
  expect_error(verify_cols_missing(df, newcols),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))

  expect_silent(verify_cols_missing(df, c("d")))

  # Try with a list in newcols
  expect_silent(verify_cols_missing(df, list("d", "e")))
  expect_error(verify_cols_missing(df, list("a", "c")),
               Hmisc::escapeRegex("column(s) 'a' is (are) already column names in data frame 'df'"))
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

test_that("any_start_with works properly", {
  expect_true(any_start_with(x = c("a", "b", "c"), target = "b"))
  expect_true(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Production"))
  expect_false(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Offshore"))
  expect_false(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Crude"))
  expect_equal(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = c("Production", "Offshore")),
               c(TRUE, FALSE))
  # Does it also work with lists?
  expect_equal(any_start_with(x = list("Production - Crude", "Production - NG", "Bogus"), target = c("Production", "Offshore")),
               c(TRUE, FALSE))
})

test_that("starts_with_any_of works properly", {
  expect_true(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = "suffix"))
  expect_equal(starts_with_any_of(x = c("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
  # Does it also work with lists?
  expect_equal(starts_with_any_of(x = list("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("primary_industries works correctly", {
  mats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  expected <- c("Resources - Crude", "Resources - NG")
  expect_equal(primary_industries(mats)[["p_industries"]],
               list(expected, expected, expected, expected))
  # Try with individual matrices
  for (i in 1:nrow(mats)) {
    expect_equal(primary_industries(U = mats$U[[i]], V = mats$V[[i]]) %>% set_names(NULL) %>% unlist(), expected)
  }
})

