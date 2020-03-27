#
# This script reads the raw UK2000 data and prepares it for use in the package.
#
# Workflow:
#
# (1) If any changes are made in SuperSimpleEconomy_2018-11-13.xlsx,
# be sure to build the package before running this script.
# Building the package will put the SuperSimpleEconomy_2018-11-13.xlsx file in the correct location
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

# Load the raw data from the Excel file
UKEnergy2000tidy <- openxlsx::read.xlsx(system.file("extdata", "UKEnergy2000raw", "SuperSimpleEconomy_2018-11-13.xlsx",
                                                    package = "Recca", mustWork = TRUE), sheet = "UKEnergy2000raw") %>%
  dplyr::mutate(
    Flow = replace_html_codes(Flow)
  )
usethis::use_data(UKEnergy2000tidy, overwrite = TRUE)

# Create S_units matrices from the UKEnergy2000tidy data frame
S_units <- UKEnergy2000tidy %>%
  dplyr::group_by(Country, Year, Energy.type, Last.stage) %>%
  S_units_from_tidy()

UKEnergy2000mats <- UKEnergy2000tidy %>%
  # Add a column indicating the matrix in which this entry belongs (U, V, or Y).
  IEATools::add_psut_matnames() %>%
  # Add metadata columns for row names, column names, row types, and column types.
  IEATools::add_row_col_meta() %>%
  # Eliminate columns we no longer need
  dplyr::select(-Ledger.side,
                -Flow.aggregation.point,
                -Flow,
                -Product) %>%
  dplyr::mutate(
    # Ensure that all energy values are positive, as required for analysis.
    E.dot = abs(E.dot)
  ) %>%

  # Collapse to matrices
  dplyr::group_by(Country, Year, Energy.type, Last.stage, matnames) %>%
  matsindf::collapse_to_matrices(matnames = "matnames", matvals = "E.dot",
                                 rownames = "rownames", colnames = "colnames",
                                 rowtypes = "rowtypes", coltypes = "coltypes") %>%
  dplyr::rename(matrix.name = matnames, matrix = E.dot) %>%
  tidyr::spread(key = matrix.name, value = matrix) %>%

  dplyr::mutate(
    # Create full U matrix
    U = matsbyname::sum_byname(U_excl_EIOU, U_EIOU),
    r_EIOU = matsbyname::quotient_byname(U_EIOU, U),
    r_EIOU = matsbyname::replaceNaN_byname(r_EIOU, val = 0)
  ) %>%
  dplyr::select(-U_EIOU, -U_excl_EIOU) %>%
  # Add S_units matrices
  dplyr::left_join(S_units, by = c("Country", "Year", "Energy.type", "Last.stage")) %>%
  tidyr::gather(key = matrix.name, value = matrix, R, U, V, Y, r_EIOU, S_units)

usethis::use_data(UKEnergy2000mats, overwrite = TRUE)
