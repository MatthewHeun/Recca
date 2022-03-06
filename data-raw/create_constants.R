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
                  phi = "phi",
                  s_units = "S_units",
                  matnames = "matnames",
                  matvals = "matvals",
                  sector = "Sector",
                  energy_type = "Energy.type")
usethis::use_data(psut_cols, overwrite = TRUE)



#
# Default names for columns in aggregate data frames
#

aggregate_cols <- list(aggregate_primary = "EX.p",
                       net_aggregate_demand = "EX.d_net",
                       gross_aggregate_demand = "EX.d_gross")
usethis::use_data(aggregate_cols, overwrite = TRUE)


#
# Give the column names of Sankey data
#

sankey_cols <- list(sankey = "Sankey")
usethis::use_data(sankey_cols, overwrite = TRUE)


#
# Give the column names of industry information
#

industry_cols <- list(p_industry_prefixes = "p_industry_prefixes",
                      p_industries_complete = "p_industries_complete")
usethis::use_data(industry_cols, overwrite = TRUE)


#
# Energy types
#

energy_types <- list(energy_type = "Energy.type", # Column name
                     e = "E", # Energy
                     x = "X") # Exergy
usethis::use_data(energy_types, overwrite = TRUE)
