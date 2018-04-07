
###########################################################
context("Utilities")
###########################################################

test_that("saving and loading expectations works as expected", {
  # testthat sets the working directory to the folder containing this file.
  # However, the functions use_expected_data and load_expected_data
  # make different (default) assumptions about the location of the expectations directory.
  # Make everything work by setting the working directory to the
  # top level directory for this project.

  # Save the current working directory, to be restored later
  cwd <- getwd()
  # Move the working directory up two levels, the top level of this project.
  setwd(file.path("..", ".."))


  afortesting <- 5
  use_expected_data(afortesting)
  expect_equal(afortesting, load_expected_data("afortesting"))
  path <- file.path("tests", "expectations", "expected_afortesting.rds")
  if (file.exists(path)) {
    file.remove(path) %>% invisible()
  }
  expect_false(file.exists(path))


  # Restore the previous working directory.
  setwd(cwd)
})
