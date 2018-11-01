library(magrittr)
library(testthat)
library(tidyr)


###########################################################
context("IO calculations")
###########################################################

test_that("calculating y, q, f, g, W, A, and L works as expected", {
  # Calculate y, q, f, g, and W from UKEnergy2000mats
  yqfgW <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW()
  Recca:::test_against_file(yqfgW, "expected_yqfgW.rds", update = FALSE)

  # Calculate Z, D, C, and A matrices from yqfgW
  A <- yqfgW %>% calc_A()
  Recca:::test_against_file(A, "expected_A.rds", update = FALSE)

  # Calculate L matrices (L_ixp and L_pxp)
  L <- A %>% calc_L()
  Recca:::test_against_file(L, "expected_L.rds", update = FALSE)
})


test_that("calculating IO matrices works as expected", {
  # Calculate all IO matrices
  io_mats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_io_mats()
  Recca:::test_against_file(io_mats, "expected_iomats.rds", update = FALSE)
})


test_that("calculating IO matrices works as expected", {
  # Make bogus U, V, Y, and S_units matrices
  U <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
    setrowtype("products") %>% setcoltype("industries")
  V <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>%
    setrowtype("industries") %>% setcoltype("products")
  Y <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("s1", "s2"))) %>%
    setrowtype("products") %>% setcoltype("industries")
  S_units <- matrix(c(1, 0,
                      0, 1), byrow = TRUE,
                    nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("m", "K")))

  yqfgW <- calc_yqfgW(U_colname = U, V_colname = V, Y_colname = Y, S_units = S_units)
  # Because the units are not homogeneous, we should receive NA values for f and g vectors.
  f <- yqfgW$f
  expect_true(is.na(f[1, 1]))
  expect_true(is.na(f[2, 1]))
  g <- yqfgW$g
  expect_true(is.na(g[1, 1]))
  expect_true(is.na(g[2, 1]))
})


