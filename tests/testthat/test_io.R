
###########################################################
context("IO calculations")
###########################################################

test_that("calculating y, q, g, and W works as expected", {
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
  yqgW <- UKEnergy2000mats %>%
    calc_yqgW(keep_cols = c("Country", "Year", "Energy.type", "Last.stage", "U", "V", "Y", "r_EIOU", "S_units"))
  expect_known_value(yqgW, file.path(expec_path, "yqgW.rds"), update = FALSE)


  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})

