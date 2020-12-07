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
# Default names for columns in aggregate data frames
#
aggregate_cols <- list(aggregate_primary = "EX_p.ktoe",
                       net_aggregate_demand = "EX_fd_net.ktoe",
                       gross_aggregate_demand = "EX_fd_gross.ktoe",
                       aggregate_primary_iea = "EX_p_IEA.ktoe",
                       aggregate_net_finaldemand_iea = "EX_fd_net_IEA.ktoe",
                       aggregate_gross_finaldemand_iea = "EX_fd_gross_IEA.ktoe")
usethis::use_data(aggregate_cols, overwrite = TRUE)


#
# Give the column names of Sankey data
#
sankey_cols <- list(sankey = "Sankey")
usethis::use_data(sankey_cols, overwrite = TRUE)

