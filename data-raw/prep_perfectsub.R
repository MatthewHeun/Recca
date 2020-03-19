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

# Load the raw data from the .Excel file
PerfectSubtidy <- openxlsx::read.xlsx(system.file("extdata", "PerfectSubstitutionraw", "UTEI_Sankey_Simple_ECC_Perfect_Sub.xlsx",
                                                    package = "Recca", mustWork = TRUE), sheet = "PerfectSubstitutionRaw")

usethis::use_data(PerfectSubtidy, overwrite = TRUE)

# Create S_units matrices from the PerfectSubtidy data frame
S_units <- PerfectSubtidy %>%
  dplyr::group_by(Country, Year, Energy.type, Last.stage) %>%
  S_units_from_tidy()

PerfectSubmats <- PerfectSubtidy %>%
  # Add a column indicating the matrix in which this entry belongs (U, V, or Y).
  add_matnames_iea() %>%
  # Add metadata columns for row names, column names, row types, and column types.
  IEATools::add_row_col_meta() %>%
  # Eliminate columns we no longer need
  dplyr::select(-Ledger.side, -Flow.aggregation.point, -Flow, -Product) %>%
  dplyr::mutate(
    # Ensure that all energy values are positive, as required for analysis.
    EX.ktoe = abs(EX.ktoe)
  ) %>%

  # Collapse to matrices
  dplyr::group_by(Country, Year, Energy.type, Last.stage, matname) %>% View
  matsindf::collapse_to_matrices(matnames = "matnames", matvals = "E.dot",
                                 rownames = "rownames", colnames = "colnames",
                                 rowtypes = "rowtypes", coltypes = "coltypes") %>%
  dplyr::rename(matrix.name = matname, matrix = EX.ktoe) %>%
  tidyr::spread(key = matrix.name, value = matrix) %>%

  dplyr::mutate(
    # Create full U matrix
    U = sum_byname(U_excl_EIOU, U_EIOU),
    r_EIOU = quotient_byname(U_EIOU, U),
    r_EIOU = replaceNaN_byname(r_EIOU, val = 0)
  ) %>%
  dplyr::select(-U_EIOU, -U_excl_EIOU) %>%
  # Add S_units matrices
  dplyr::left_join(S_units, by = c("Country", "Year", "Energy.type", "Last.stage")) %>%
  tidyr::gather(key = matrix.name, value = matrix, U, V, Y, r_EIOU, S_units)


usethis::use_data(PerfectSubmats, overwrite = TRUE)
