#
# This script reads the raw UK2000 data and prepares it for use in the package.
#

library(magrittr)
library(dplyr)
library(matsindf)
library(devtools)


# Pull in functions to assist with cleaning the data
source(file.path("data-raw", "data_prep_utilities.R"))

# Load the raw data from the .csv file
UKEnergy2000raw <- read.csv(system.file("extdata", "UKEnergy2000raw", "UKEnergy2000raw.csv",
                                        package = "Recca", mustWork = TRUE),
                            stringsAsFactors = FALSE)

# Add metadata columns
UKEnergy2000mats <- UKEnergy2000raw %>%
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
  rename(matrix.name = matname, matrix = EX.ktoe)

use_data(UKEnergy2000mats, overwrite = TRUE)


