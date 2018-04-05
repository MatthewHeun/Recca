#
# This script reads the raw UK2000 data and prepares it for use in the package.
#

# Pull in functions to assist with cleaning the data
source(file.path("data-raw", "data_prep_utilities.R"))

# Load the raw data from the .csv file
UK2000path <- system.file("extdata", "UK2000raw", "UK2000raw.csv", package = "Recca", mustWork = TRUE)
UK2000 <- read.csv(UK2000path)
