library(dplyr)
library(Hmisc)
library(magrittr)
library(tidyr)

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

test_that("resource_industries works correctly", {
  mats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  expected <- c("Resources - Crude", "Resources - NG")
  expect_equal(resource_industries(mats)[["r_industries"]],
               list(expected, expected, expected, expected))
  # Try with individual matrices
  for (i in 1:nrow(mats)) {
    expect_equal(resource_industries(U = mats$U[[i]], V = mats$V[[i]]) %>% set_names(NULL) %>% unlist(), expected)
  }
})

test_that("separate_RV works correctly", {
  # These tests will need to be re-evaluated after I implement R matrices in the
  # UKEnergy2000Mats data frame.
  expected <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    mutate(
      R = V %>% select_rows_byname(retain_pattern = make_pattern("Resources - ", pattern_type = "leading")),
      V = V %>% select_rows_byname(remove_pattern = make_pattern("Resources - ", pattern_type = "leading"))
    )

  mats <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    # Rename the V matrix, because it includes the R matrix.
    # At some point, this rename step will be unnecessary because UKEnergy2000mats will be created with R separate from V
    rename(
      V_plus_R = V
    ) %>%
    separate_RV()

  # Make sure that we get the expected values for R and V matrices
  for (i in 1:4) {
    expect_true(equal_byname(mats$R[[i]], expected$R[[i]]))
    expect_true(equal_byname(mats$V[[i]], expected$V[[i]]))
  }
})

test_that("products_unit_homogeneous works correctly", {
  result <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    products_unit_homogeneous() %>%
    extract2("products_unit_homogeneous") %>%
    unlist()
  expect_true(all(result))

  # Now make an S_units matrix that should fail.
  su <- matrix(c(1, 1, 0, 1), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("m", "kg")))
  expect_false(products_unit_homogeneous(S_units_colname = su)[[1]])
})

test_that("inputs_unit_homogeneous works correctly", {
  result <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    inputs_unit_homogeneous() %>%
    extract2("inputs_unit_homogeneous") %>%
    unlist()
  # We expect to have FALSE when services are the Last.stage.
  expected <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    mutate(
      expected = case_when(
        Last.stage == "services" ~ FALSE,
        Last.stage != "services" ~ TRUE,
        TRUE ~ NA
        )
    ) %>%
    extract2("expected")
  # Perform the test.
  expect_equal(result, expected)
})

test_that("output_unit_homogeneous works correctly", {
  result <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    outputs_unit_homogeneous() %>%
    extract2("outputs_unit_homogeneous") %>%
    unlist()
  expect_true(all(result))

  # Now make a version that we expect to fail
  V <- matrix(c(1, 1,
                1, 1), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("i1", "i2"), c("p1", "p2")))
  S_units <- matrix(c(1, 0,
                      0, 1), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2"), c("m", "kg")))
  result2 <- outputs_unit_homogeneous(V_colname = V, S_units_colname = S_units)
  expect_false(result2 %>% unlist())
})











