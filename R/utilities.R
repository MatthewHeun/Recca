#
# This file contains utilities useful for unit testing.
#


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
#' @param prefix the prefix to the file name.  Default is \code{expected_}.
#'
#' @return This function is called for its side effect of saving data into the \code{tests/expectations} directory.
#'
#' @export
#'
#' @examples
#' afortesting <- 5
#' use_expected_data(afortesting)
use_expected_data <- function(obj,
                              dir = file.path("tests", "expectations"),
                              prefix = "expected_",
                              overwrite = FALSE){
  # Get the name of the object as a string.
  # From https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function
  name <- deparse(substitute(obj))
  path <- file.path(dir, paste0(prefix, name, ".rds"))
  if (!overwrite & file.exists(path)) {
    stop(paste("file", path, "already exists. Use 'overwrite = TRUE' to save anyway."))
  }
  # Save obj to disk under its new file name
  saveRDS(obj, file = file.path(dir, paste0(prefix, name, ".rds")))
}


#' Load expected results
#'
#' Loads an expected result for a test.
#' Use \link[Recca]{use_expected_data} to save the expectation to disk.
#'
#' The default directory for expectatations is \code{tests/expectations}.
#'
#' @param expectation_name the name of the object for which you want to load an expected result.
#'        Do not include the "\code{expected_}" prefix.
#' @param dir the directory in which to look for expected results.
#'        Default is \code{file.path("tests", "expectations")}
#' @param prefix the prefix to the file name.  Default is "\code{expected_}".
#'
#' @return the R object contained in the file named \code{<prefix><expectation_name>.rds}
#'         loaded from the directory at \code{path}.
#'
#' @export
#'
#' @examples
#' afortesting <- 5
#' use_expected_data(afortesting)
#' load_expected_data("afortesting")
load_expected_data <- function(expectation_name,
                               dir = file.path("tests", "expectations"),
                               prefix = "expected_"){
  # Load the expected result and return it
  readRDS(file.path(dir, paste0(prefix, expectation_name, ".rds")))
}
