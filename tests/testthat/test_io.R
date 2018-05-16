
###########################################################
context("IO calculations")
###########################################################

test_that("calculating y, q, g, W, A, and L works as expected", {
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

  # Calculate y, q, g, and W from UKEnergy2000mats
  yqgW <- UKEnergy2000mats %>% calc_yqgW()
  expect_known_value(yqgW, file.path(expec_path, "expected_yqgW.rds"), update = FALSE)

  # Calculate Z, D, C, and A matrices from yqgW
  A <- yqgW %>% calc_A()
  expect_known_value(A, file.path(expec_path, "expected_A.rds"), update = FALSE)

  # Calculate L matrices (L_ixp and L_pxp)
  L <- A %>% calc_L()
  expect_known_value(L, file.path(expec_path, "expected_L.rds"), update = FALSE)

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
  io_mats <- UKEnergy2000mats %>% calc_io_mats()
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
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2")))
  V <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("i1", "i2"), c("p1", "p2")))
  Y <- matrix(c(1, 2,
                3, 4), byrow = TRUE,
              nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("s1", "s2")))
  S_units <- matrix(c(1, 0,
                      0, 1), byrow = TRUE,
                    nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("m", "K")))

  expect_error(calc_yqgW(U_colname = U, V_colname = V, Y_colname = Y, S_units = S_units),
               "Outputs from each industry not unit homogeneous. Offending industries: i1, i2")
})


