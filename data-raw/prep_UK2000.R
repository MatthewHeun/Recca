#
# This script reads the raw UK2000 data and prepares it for use in the package.
#

library(magrittr)
library(dplyr)
library(tidyr)
library(matsindf)
library(devtools)


# Pull in functions to assist with cleaning the data
source(file.path("data-raw", "data_prep_utilities.R"))

# Load S_unit matrix
S_units_temp <- read.csv(system.file("extdata", "UKEnergy2000raw", "S_units_tidy.csv",
                                     package = "Recca", mustWork = TRUE),
                         stringsAsFactors = FALSE) %>%
  collapse_to_matrices(matnames = "matname", values = "value",
                       rownames = "rowname", colnames = "colname",
                       rowtypes = "rowtype", coltypes = "coltype")
S_units <- S_units_temp$value[[1]]
rm(S_units_temp)

# Load the raw data from the .csv file
UKEnergy2000tidy <- read.csv(system.file("extdata", "UKEnergy2000raw", "UKEnergy2000raw.csv",
                                         package = "Recca", mustWork = TRUE),
                             stringsAsFactors = FALSE)
use_data(UKEnergy2000tidy, overwrite = TRUE)

# Add metadata columns
UKEnergy2000mats <- UKEnergy2000tidy %>%
  # Add a column indicating the matrix in which this entry belongs (U, V, or Y).
  add_UKEnergy2000_matnames() %>%
  # Add columns for row names, column names, row types, and column types.
  add_UKEnergy2000_row_col_meta() %>%
  mutate(
    # Eliminate columns we no longer need
    Ledger.side = NULL,
    Flow.aggregation.point = NULL,
    Flow = NULL,
    Product = NULL,
    # Ensure that all energy values are positive, as required for analysis.
    EX.ktoe = abs(EX.ktoe)
  ) %>%

  # Collapse to matrices
  group_by(Country, Year, Energy.type, Last.stage, matname) %>%
  collapse_to_matrices(matnames = "matname", values = "EX.ktoe",
                       rownames = "rowname", colnames = "colname",
                       rowtypes = "rowtype", coltypes = "coltype") %>%
  rename(matrix.name = matname, matrix = EX.ktoe) %>%
  spread(key = matrix.name, value = matrix) %>%

  mutate(
    # Create full U matrix
    U = sum_byname(U, U_EIOU),
    r_EIOU = elementquotient_byname(U_EIOU, U),
    r_EIOU = replaceNaNWith0(r_EIOU),
    # Add unit information
    S_units = make_list(S_units, 4)
  ) %>%
  select(-U_EIOU)

use_data(UKEnergy2000mats, overwrite = TRUE)
