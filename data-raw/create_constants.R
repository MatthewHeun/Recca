# This script creates constants and saves them in the right locations.
# If there are any changes to these constants,
# source this script before building the package.

library(magrittr)
library(IEATools)


#
# Give the column names of data frames with PSUT data
#

psut_cols <- list(resources = "R",
                  R = "R",
                  use = "U",
                  U = "U",
                  U_feed = "U_feed",
                  U_eiou = "U_EIOU",
                  r_eiou = "r_EIOU",
                  make = "V",
                  V = "V",
                  final_demand = "Y",
                  Y = "Y",
                  s_units = "S_units",
                  matvals = "matvals")
usethis::use_data(psut_cols, overwrite = TRUE)



#
# Give the column names of Sankey data
#

sankey_cols <- list(sankey = "Sankey")
usethis::use_data(sankey_cols, overwrite = TRUE)

