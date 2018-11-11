#
# This script reads the raw perfect substitution data and prepares it for use in the package.
#
# Workflow:
#
# (1) If any changes are made in PerfectSubstitutionraw.csv,
# be sure to build the package before running this script.
# Building the package will put the PerfectSubstitutionraw.csv file in the correct location
# to be picked up by the System.file function.
#
# (2) Run this script so that use_data puts the *.rda files in the correct location.
#
# (3) Build again to make sure all *.rda data files are collected into the package.

library(magrittr)
library(dplyr)
library(tidyr)
library(matsbyname)
library(matsindf)
library(devtools)

# Load the raw data from the .csv file
PerfectSubtidy <- read.csv(system.file("extdata", "PerfectSubstitutionraw", "PerfectSubstitutionraw.csv",
                                       package = "Recca", mustWork = TRUE),
                           stringsAsFactors = FALSE)

use_data(PerfectSubtidy, overwrite = TRUE)

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
  left_join(S_units, by = c("Country", "Year", "Energy.type", "Last.stage")) %>%
  gather(key = matrix.name, value = matrix, U, V, Y, r_EIOU, S_units)


use_data(PerfectSubmats, overwrite = TRUE)
