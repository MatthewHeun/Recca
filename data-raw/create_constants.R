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
                  V_feed = "V_feed",
                  V_eiou = "V_EIOU",
                  final_demand = "Y",
                  Y = "Y",
                  phi = "phi",
                  S_units = "S_units",
                  Y_fu_details = "Y_fu_details",
                  U_eiou_fu_details = "U_EIOU_fu_details",
                  includes_neu = "IncludesNEU",
                  matnames = "matnames",
                  matvals = "matvals",
                  sector = "Sector",
                  country = "Country",
                  year = "Year",
                  method = "Method",
                  energy_type = "EnergyType",
                  last_stage = "LastStage")
usethis::use_data(psut_cols, overwrite = TRUE)


#
# Default names for columns in aggregate data frames
#

aggregate_cols <- list(aggregate_primary = "EXp",
                       net_aggregate_primary = "EXpnet",
                       gross_aggregate_primary = "EXpgross",

                       aggregate_final = "EXf",
                       net_aggregate_final = "EXfnet",
                       gross_aggregate_final = "EXfgross",

                       aggregate_useful = "EXu",
                       net_aggregate_useful = "EXunet",
                       gross_aggregate_useful = "EXugross",

                       aggregate_services = "EXs",
                       net_aggregate_services = "EXsnet",
                       gross_aggregate_services = "EXsgross",

                       aggregate_demand = "EXfd",
                       net_aggregate_demand = "EXfdnet",
                       gross_aggregate_demand = "EXfdgross",

                       region = "Region",
                       aggregated_suffix = "aggregated",
                       product_sector = "ProductIndustrySector",
                       chop_df = "ChoppedECCs")
usethis::use_data(aggregate_cols, overwrite = TRUE)


#
# Default names for columns in efficiency data frames
#

efficiency_cols <- list(eta_i = "eta_i",
                        eta_pfd_gross = "etapfdgross",
                        eta_pfd_net = "etapfdnet",
                        eta_pfd = "etapfd",
                        eta_pf = "etapf",
                        eta_fu = "etafu",
                        eta_pu = "etapu",
                        eta_ps = "etaps",
                        eta_fs = "etafs",
                        eta_us = "etaus",
                        efficiency_name_suffix = "_name",
                        gross_net = "GrossNet",
                        gross = "Gross",
                        net = "Net")
usethis::use_data(efficiency_cols, overwrite = TRUE)


#
# Allocation and efficiency columns
#
alloc_cols <- list(C_Y = "C_Y",
                   C_eiou = "C_EIOU")
usethis::use_data(alloc_cols, overwrite = TRUE)


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

energy_types <- list(energy_type = "EnergyType", # Column name
                     e = "E", # Energy
                     x = "X") # Exergy
usethis::use_data(energy_types, overwrite = TRUE)


#
# High pressure matter Eu.products
#
hpm_eu.products <- list(hpa = "HPA",
                        hpl = "HPL",
                        hpng = "HPNG")
usethis::use_data(hpm_eu.products, overwrite = TRUE)


#
# High temperature heat (HTH) Eu.products
#
hth_eu.products <- list(hth_400_c = "HTH.400.C",
                        hth_600_c = "HTH.600.C",
                        hth_850_c = "HTH.850.C",
                        hth_960_c = "HTH.960.C",
                        hth_1000_c = "HTH.1000.C",
                        hth_1300_c = "HTH.1300.C",
                        hth_1600_c = "HTH.1600.C")
usethis::use_data(hth_eu.products, overwrite = TRUE)


#
# Medium temperature heat (MTH) Eu.products
#
mth_eu.products <- list(mth_100_c = "MTH.100.C",
                        mth_200_c = "MTH.200.C")
usethis::use_data(mth_eu.products, overwrite = TRUE)


#
# Low temperature heat (LTH) Eu.products
#
lth_eu.products <- list(lth_20_c = "LTH.20.C",
                        lth_60_c = "LTH.60.C")
usethis::use_data(lth_eu.products, overwrite = TRUE)


#
# Cooling Eu.products
#
cooling_eu.products <- list(`ltc_-10_c` = "LTC.-10.C",
                            ltc_20_c = "LTC.20.C")
usethis::use_data(cooling_eu.products, overwrite = TRUE)


#
# Propulsion Eu.products
#
propulsion_eu.products <- list(mp = "MP",
                               rop = "RoP",
                               rap = "RaP")
usethis::use_data(propulsion_eu.products, overwrite = TRUE)


#
# Mechanical Eu.products
#
mechanical_eu.products <- list(md = "MD",
                               ke = "KE",
                               mw = "MW",
                               mf = "MF")
usethis::use_data(mechanical_eu.products, overwrite = TRUE)


#
# Information processing (IP) Eu.products
#
ip_eu.products <- list(ip = "IP")
usethis::use_data(ip_eu.products, overwrite = TRUE)


#
# Light (L) Eu.products
#
l_eu.products <- list(L = "L")
usethis::use_data(l_eu.products, overwrite = TRUE)


#
# Non-energy use (NEU) Eu.products
#
neu_eu.products <- list(neu = "NEU")
usethis::use_data(neu_eu.products, overwrite = TRUE)


#
# Eu.product Aggregation information
#

euproduct_aggregation_map <-
  list(`High pressure matter` = Recca::hpm_eu.products %>% unlist() %>% unname(),
       `High temperature heat` = Recca::hth_eu.products %>% unlist() %>% unname(),
       `Medium temperature heat` = Recca::mth_eu.products %>% unlist() %>% unname(),
       `Low temperature heat` = Recca::lth_eu.products %>% unlist() %>% unname(),
       `Cooling` = Recca::cooling_eu.products %>% unlist() %>% unname(),
       `Propulsion` = Recca::propulsion_eu.products %>% unlist() %>% unname(),
       `Mechanical` = Recca::mechanical_eu.products %>% unlist() %>% unname(),
       `Information processing` = Recca::ip_eu.products %>% unlist() %>% unname(),
       `Light` = Recca::l_eu.products %>% unlist() %>% unname(),
       `Non-energy use` = Recca::neu_eu.products %>% unlist() %>% unname())
usethis::use_data(euproduct_aggregation_map, overwrite = TRUE)


#
# Names of columns containing product and industry names
#

prod_ind_names_colnames <- list(product_names = "Product.names",
                                industry_names = "Industry.names")
usethis::use_data(prod_ind_names_colnames, overwrite = TRUE)


#
# Row and column types
#

row_col_types <- list(product_type = "Product",
                      industry_type = "Industry")
usethis::use_data(row_col_types, overwrite = TRUE)


#
# All stages in energy conversion chains
#

all_stages <- list(primary = "Primary",
                   final = "Final",
                   useful = "Useful",
                   services = "Services",
                   last_stage_sep = "___ls")
usethis::use_data(all_stages, overwrite = TRUE)
