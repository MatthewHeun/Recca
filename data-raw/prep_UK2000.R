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
    U = matsbyname::sum_byname(U_feed, U_EIOU),
    r_EIOU = matsbyname::quotient_byname(U_EIOU, U),
    r_EIOU = matsbyname::replaceNaN_byname(r_EIOU, val = 0)
  ) %>%
  # Add S_units matrices
  dplyr::left_join(S_units, by = c("Country", "Year", "Energy.type", "Last.stage")) %>%
  tidyr::gather(key = matrix.name, value = matrix, R, U, U_EIOU, U_feed, V, Y, r_EIOU, S_units)

usethis::use_data(UKEnergy2000mats, overwrite = TRUE)


# Create a phi vector for later use in testing.
phi_vec <- tibble::tribble(~rownames, ~matvals,
                           "Crude", 1.06,
                           "Crude [from Dist.]", 1.06,
                           "Crude [from Fields]", 1.06,
                           "Diesel", 1.06,
                           "Diesel [from Dist.]", 1.06,
                           "Petrol", 1.06,
                           "Petrol [from Dist.]", 1.06,
                           "NG", 1.04,
                           "NG -[from Dist.]", 1.04,
                           "NG [from Wells]", 1.04,
                           "Elect", 1.0,
                           "Elect [from Grid]", 1.0,
                           "MD [from Truck engines]", 1.0,
                           "MD [from Car engines]", 1.0,
                           "Light", 0.956,
                           # Same as LTH.20.C
                           "LTH", 0.05117) %>%
  dplyr::mutate(
    matnames = "phi",
    colnames = "phi",
    rowtypes = "Product",
    coltypes = "phi"
  ) %>%
  matsindf::collapse_to_matrices() %>%
  magrittr::extract2("matvals") %>%
  magrittr::extract2(1)

usethis::use_data(phi_vec, overwrite = TRUE)





