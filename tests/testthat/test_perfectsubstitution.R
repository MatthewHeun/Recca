library(dplyr)
library(Hmisc)
library(magrittr)
library(matsindf)
library(Recca)
library(testthat)
library(tidyr)

###########################################################
context("Perfect substitution")
###########################################################

prep_perfect_subsitution <- function(){
  PerfectSubtidy <- read.csv(system.file("extdata", "PerfectSubstitutionraw", "PerfectSubstitutionraw.csv",
                                           package = "Recca", mustWork = TRUE),
                               stringsAsFactors = FALSE)

  # Create S_units matrices from the PerfectSubtidy data frame
  S_units <- PerfectSubtidy %>%
    group_by(Country, Year, Energy.type, Last.stage) %>%
    S_units_from_tidy()

  PerfectSubmats <- PerfectSubtidy %>%
    # Add a column indicating the matrix in which this entry belongs (U, V, or Y).
    add_matnames_iea() %>%
    # Add metadata columns for row names, column names, row types, and column types.
    add_row_col_meta() %>%
    # Eliminate columns we no longer need
    select(-Ledger.side, -Flow.aggregation.point, -Flow, -Product) %>%
    mutate(
      # Ensure that all energy values are positive, as required for analysis.
      EX.ktoe = abs(EX.ktoe)
    ) %>%

    # Collapse to matrices
    group_by(Country, Year, Energy.type, Last.stage, matname) %>%
    collapse_to_matrices(matnames = "matname", matvals = "EX.ktoe",
                         rownames = "rowname", colnames = "colname",
                         rowtypes = "rowtype", coltypes = "coltype") %>%
    rename(matrix.name = matname, matrix = EX.ktoe) %>%
    spread(key = matrix.name, value = matrix) %>%

    mutate(
      # Create full U matrix
      U = sum_byname(U_excl_EIOU, U_EIOU),
      r_EIOU = elementquotient_byname(U_EIOU, U),
      r_EIOU = replaceNaN_byname(r_EIOU, val = 0)
    ) %>%
    select(-U_EIOU, -U_excl_EIOU) %>%
    # Add S_units matrices
    left_join(S_units, by = c("Country", "Year", "Energy.type", "Last.stage"))

  return(list(tidy = PerfectSubtidy, mats = PerfectSubmats))
}

test_that("calculation of B matrix works", {
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

  perfectsublist <- prep_perfect_subsitution()
  perfectsub_tidy <- perfectsublist$tidy
  perfectsub_mats <- perfectsublist$mats

  io_mats <- perfectsub_mats %>% calc_io_mats()
  B <- io_mats$B[[1]]

  expect_known_value(B, file.path(expec_path, "expected_B.rds"), update = FALSE)

  if (is_testing()) {
    # Restore the previous working directory.
    setwd(currwd)
  }
})

