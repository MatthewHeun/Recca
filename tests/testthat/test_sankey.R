# Tests for make_sankey()

library(Recca)
library(testthat)
library(tidyr)

###########################################################
context("Sankey")
###########################################################

test_that("make_sankey works as expected", {
  sutmats <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix")
  # First, try with some missing values.
  single <- sutmats[1, ]
  expect_false(is.na(make_sankey(U = single$U[[1]], V = single$V[[1]], Y = single$Y[[1]])))
  expect_true(is.na(make_sankey(U = NA_real_, V = single$V[[1]], Y = single$Y[[1]])))
  expect_true(is.na(make_sankey(U = single$U[[1]], V = NA_real_, Y = single$Y[[1]])))
  expect_true(is.na(make_sankey(U = single$U[[1]], V = single$V[[1]], Y = NA_real_)))
  expect_true(is.na(make_sankey(R = NA_real_, U = single$U[[1]], V = single$V[[1]], Y = single$Y[[1]])))

  # Now try to make some real sankeys
  for (i in 1:nrow(sutmats)) {
    s <- make_sankey(U = sutmats$U[[i]], V = sutmats$V[[i]], Y = sutmats$Y[[i]])
    expect_false(is.null(s))
  }
  sankeys <-  sutmats %>%
    make_sankey()
  for (i in nrow(sankeys)) {
    expect_false(is.null(sankeys$Sankey[[i]]))
  }
})
