#' Save an object for testing
#'
#' When developing tests,
#' it is convenient to perform an \code{R} calculation in a \code{test_that} function
#' and compare its result against an external calculation (from Excel or otherwise)
#' to verify correctness.
#' When correctness is verified,
#' you may want to
#' (a) capture the \code{R} object that represents the
#' result of the calculation and
#' (b) save it for later use with correctness tests in the \code{testthat} framework.
#' This function streamlines that process and works like
#' \code{\link[devtools]{use_data}}.
#' A call to \code{use_expected_data()} saves \code{obj} in a file named
#' "\code{expected_obj.rda}" file, where "\code{obj}" is replaced by the object's name in the environment.
#' Because the \code{testthat.R} file loads all previously-saved expectation objects,
#' they will be available for testing.
#'
#' @param obj an object to be used for testing
#' @param dir the directory in which to save expectation objects.
#'        Default is file.path("tests", "expectations")
#'
#' @return This function is called for its side effect of saving data into the \code{tests/expectations} directory.
#'
#' @export
#'
#' @examples
#' a <- 5
#' use_expected_data(a)
use_expected_data <- function(obj, dir = file.path("tests", "expectations")){
  # Get the name of the object as a string.
  # From https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function
  name <- deparse(substitute(obj))
  # Build the new name for the object as well as the file name and file path.
  newname <- paste0("expected_", name)
  filename <- paste0(newname, ".rda")
  filepath <- file.path("tests", "expectations", filename)
  # Save obj under its new file name
  assign(newname, obj)
  # Remove obj, as it is now saved under its new name
  rm(obj)
  # Save the newname to disk
  save(list = newname, file = filepath)
}


load_expectation <- function(expectation_name){

}
