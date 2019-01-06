# Tests for make_sankey()

library(Recca)
library(testthat)
library(tidy)

###########################################################
context("Sankey")
###########################################################

test_that("make_sankey works as expected", {
  sutmats <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix")
  for (i in 1:nrow(sutmats)) {
    s <- make_sankey(U = sutmats$U[[i]], V = sutmats$V[[i]], Y = sutmats$Y[[i]])
    expect_true(!is.null(s))
  }
  sankeys <-  sutmats %>%
    make_sankey()
  for (i in nrow(sankeys)) {
    expect_true(!is.null(sankeys$Sankey[[i]]))
  }
})
