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

test_that("ensure_DF works as expected", {
  m1 <- matrix(c(1, 1), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))
  m2 <- matrix(c(42, 42), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))
  df <- data.frame(v1 = c(1,2), v2 = c(3,4))

  expect_error(ensure_DF(df, m1 = m1, m2 = m2), "All ... arguments were matrices, but .DF was not missing or NULL in ensure_DF")
  expect_error(ensure_DF(U = "U", V = "V"), "All ... arguments were character, but .DF was not a data frame in ensure_DF")
  expect_error(ensure_DF(df, a = 5), "Unexpected inputs to ensure_DF")
  expect_error(ensure_DF(NULL, m1 = m1, m2 = m2, m3 = 5), "Unexpected inputs to ensure_DF")

  expectedDF <- data.frame(m1 = I(list(m1)), m2 = I(list(m2)))
  expect_equal(ensure_DF(NULL, m1 = m1, m2 = m2), expectedDF)
  expect_equal(ensure_DF(m1 = m1, m2 = m2), expectedDF)
})

