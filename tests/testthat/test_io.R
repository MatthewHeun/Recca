
###########################################################
context("IO calculations")
###########################################################

test_that("calculating y, q, f, g, W, A, and L works as expected", {
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

  # Calculate y, q, f, g, and W from UKEnergy2000mats
  yqfgW <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_yqfgW()
  expect_known_value(yqfgW, file.path(expec_path, "expected_yqfgW.rds"), update = FALSE)

  # Calculate Z, D, C, and A matrices from yqfgW
  A <- yqfgW %>% calc_A()
  expect_known_value(A, file.path(expec_path, "expected_A.rds"), update = FALSE)

  # Calculate L matrices (L_ixp and L_pxp)
  L <- A %>% calc_L()
  expect_known_value(L, file.path(expec_path, "expected_L.rds"), update = TRUE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})


test_that("calculating IO matrices works as expected", {
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

  # Calculate all IO matrices
  io_mats <- UKEnergy2000mats %>%
    spread(key = matrix.name, value = matrix) %>%
    calc_io_mats()
  expect_known_value(io_mats, file.path(expec_path, "expected_iomats.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
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


