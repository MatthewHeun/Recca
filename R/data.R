#' Physical Supply-Use Table (PSUT) data frame column names
#'
#' A string list containing named names of columns in PSUT data frames.
#' Items in the list provide default values for column name function arguments
#' throughout the `Recca` package.
#'
#' Note that some of the values are repeated,
#' thereby providing synonyms.
#' E.g., both `resources` and `R` point to the "R" column name.
#'
#' @format A string list with `r length(psut_cols)` entries.
#' \describe{
#' \item{resources,R}{The name of a column in a wide-by-matrices data frame containing resource (`R`) matrices.}
#' \item{U_feed}{The name of a column in a wide-by-matrices data frame containing use (`U`) matrices that exclude energy industry own use.}
#' \item{U_eiou}{The name of a column in a wide-by-matrices data frame containing use (`U`) matrices that contain exclusively energy industry own use.}
#' \item{U}{The name of a column in a wide data-by-matrices frame containing use (`U`) matrices that are the sum of `U_feed` and `U_eiou` matrices.}
#' \item{r_eiou}{The name of a column in a wide-by-matrices data frame containing the ratio of `U_eiou` and `U` matrices.}
#' \item{make,V}{The name of a column in a wide-by-matrices data frame containing make (`V`) matrices.}
#' \item{final_demand,Y}{The name of a column in a wide-by-matrices data frame containing final demand (`Y`) matrices.}
#' \item{phi}{The name of a column in a wide-by-matrices data frame containing exergy-to-energy-ratio (**phi**) matrices.}
#' \item{S_units}{The name of a column in a wide-by-matrices data frame containing unit summation (`S_units`) matrices.}
#' \item{Y_fu_details}{The name of a column in a wide-by-matrices data frame containing details of the final-to-useful extension for final demand.}
#' \item{U_eiou_fu_details}{The name of a column in a wide-by-matrices data frame containing details of the final-to-useful extension for energy industry own use.}
#' \item{IncludesNEU}{The name of a column containing `TRUE` or `FALSE` for whether non-energy use is include in the energy conversion chain for the associated row.}
#' \item{matnames}{The name of a column in a tidy data frame containing matrix names.}
#' \item{matvals}{The name of a column in a tidy data frame containing matrices.}
#' \item{sector}{The name of a column in a tidy data frame containing sector names.}
#' \item{country}{The name of a column in a tidy data frame containing names of countries.}
#' \item{year}{The name of a column in a tidy data frame containing years.}
#' \item{method}{The name of a column in a tidy data frame containing the method for accounting for the primary equivalent of renewable electricity.}
#' \item{energy_type}{The name of a column in a tidy data frame containing names of energy types.}
#' \item{last_stage}{The name of a column in a tidy data frame last stages of the energy conversion chain.}
#' }
#'
#' @examples
#' psut_cols
"psut_cols"



#' Aggregate data frame column names
#'
#' A string list containing named names of columns in aggregate data frames.
#' Items in the list provide default values for column name function arguments
#' to aggregation functions throughout the `Recca` package.
#'
#' @format A string list with `r length(aggregate_cols)` entries.
#' \describe{
#' \item{aggregate_primary}{The name of a column in a wide-by-matrices data frame containing aggregates of primary energy.}
#' \item{net_aggregate_primary}{The name of a column in a wide-by-matrices data frame containing aggregates of net primary energy. Net and gross aggregates will be identical at the primary stage.}
#' \item{gross_aggregate_primary}{The name of a column in a wide-by-matrices data frame containing aggregates of gross primary energy. Net and gross aggregates will be identical at the primary stage.}
#'
#' \item{aggregate_final}{The name of a column in a wide-by-matrices data frame containing aggregates of final energy.}
#' \item{net_aggregate_final}{The name of a column in a wide-by-matrices data frame containing aggregates of net final energy, not including energy industry own use.}
#' \item{gross_aggregate_final}{The name of a column in a wide-by-matrices data frame containing aggregates of gross final energy, including energy industry own use.}
#'
#' \item{aggregate_useful}{The name of a column in a wide-by-matrices data frame containing aggregates of useful energy.}
#' \item{net_aggregate_useful}{The name of a column in a wide-by-matrices data frame containing aggregates of net useful energy, not including energy industry own use.}
#' \item{gross_aggregate_useful}{The name of a column in a wide-by-matrices data frame containing aggregates of gross useful energy, including energy industry own use.}
#'
#' \item{aggregate_services}{The name of a column in a wide-by-matrices data frame containing aggregates of energy services.}
#' \item{net_aggregate_services}{The name of a column in a wide-by-matrices data frame containing aggregates of net energy services, not including energy industry own use.}
#' \item{gross_aggregate_services}{The name of a column in a wide-by-matrices data frame containing aggregates of gross energy services, including energy industry own use.}
#'
#' \item{aggregate_demand}{The name of a column in a wide-by-matrices data frame containing aggregates of final demand energy (including energy industry own use), regardless of whether the last stage is final, useful, or services.}
#' \item{net_aggregate_demand}{The name of a column in a wide-by-matrices data frame containing aggregates of net final demand energy (excluding energy industry own use), regardless of whether the last stage is final, useful, or services.}
#' \item{gross_aggregate_demand}{The name of a column in a wide-by-matrices data frame containing aggregates of gross final demand energy (including energy industry own use), regardless of whether the last stage is final, useful, or services.}
#'
#' \item{region}{The name of a column in a wide-by-matrices data frame containing regions.}
#' \item{aggregated_suffix}{The suffix for column names containing aggregated matrices.}
#' \item{product_sector}{The name of a column containing names of products, industries, or sectors. Default is "Product.Industry.Sector".}
#' \item{chop_df}{The name of a column containing a nested data frame of chopped energy conversion chains. Default is "Chopped.ECCs".}
#' }
#'
#' @examples
#' aggregate_cols
"aggregate_cols"



#' Efficiency data frame column names
#'
#' A string list containing named names of columns in efficiency data frames.
#' Items in the list provide default values for column name function arguments
#' to efficiency functions throughout the `Recca` package.
#'
#' @format A string list with `r length(efficiency_cols)` entries.
#' \describe{
#' \item{eta_i}{The name of a column in a wide-by-matrices data frame containing industry efficiencies.}
#' \item{eta_pfd_gross}{The name of a column in a wide-by-matrices data frame containing  efficiencies between the primary and gross final demand stages.}
#' \item{eta_pfd_net}{The name of a column in a wide-by-matrices data frame containing efficiencies between the primary and net final demand stages.}
#' \item{eta_pfd}{The name of a column in a wide-by-matrices data frame containing efficiencies between the primary and final demand stages, regardless of net or gross.}
#' \item{eta_pf}{The string name for primary-to-final efficiency.}
#' \item{eta_fu}{The string name for final-to-useful efficiency.}
#' \item{eta_pu}{The string name for primary-to-useful efficiency.}
#' \item{eta_ps}{The string name for primary-to-services efficiency.}
#' \item{eta_fs}{The string name for final-to-services efficiency.}
#' \item{eta_us}{The string name for useful-to-services efficiency.}
#' \item{efficiency_name_suffix}{The suffix for names of columns containing efficiency names.}
#' \item{gross_net}{The name of a column that contains "Gross" or "Net" for the type of efficiency.}
#' \item{gross}{The entry in the `gross_net` column that identifies a gross efficiency.}
#' \item{net}{The entry in the `gross_net` column that identifies a net efficiency.}
#' }
#'
#' @examples
#' efficiency_cols
"efficiency_cols"


#' Balance column names
#'
#' A string list containing named names of columns in balance data frames.
#' Items in the list provide default values for column name function arguments
#' to balance functions throughout the `Recca` package.
#'
#' @format A string list with `r length(balance_cols)` entries.
#' \describe{
#' \item{inter_industry_balance_colname}{The name of a column that contains inter-industry balance vectors.}
#' \item{inter_industry_balanced_colname}{The name of a column that tells whether inter-industry balance vectors are all **0**.}
#' \item{between_industry_balance_colname}{The name of a column that contains inter-industry balance vectors.}
#' \item{between_industry_balanced_colname}{The name of a column that contains inter-industry balance vectors are all **0**.}
#' \item{intra_industry_balance_colname}{The name of a column that contains intra-industry balance vectors.}
#' \item{intra_industry_balanced_colname}{The name of a column that contains intra-industry balance vectors are all **0**.}
#' \item{across_industry_balance_colname}{The name of a column that contains intra-industry balance vectors.}
#' \item{across_industry_balanced_colname}{The name of a column that contains intra-industry balance vectors are all **0**.}
#' \item{waste_heat}{The name of a column in **V** and a row in **Y** that contains waste heat calculated from intra-industry balances.}
#' \item{losses_sector}{The name of a column in **Y** that contains losses calculated from intra-industry balances.}
#' }
#'
#' @examples
#' balance_cols
"balance_cols"


#' Columns in a data frame that contains final-to-useful allocations
#'
#' A string list containing named names of columns in PSUT data frames.
#' Items in the list provide default values for column name function arguments
#' throughout the `Recca` package.
#'
#' @format A string list with `r length(alloc_cols)` entries.
#' \describe{
#' \item{C_Y}{The name of a column in a wide-by-matrices data frame containing allocations to final-to-useful machines used in final demand. "C_Y"}
#' \item{C_eiou}{The name of a column in a wide-by-matrices data frame containing allocations to final-to-useful machines used in final demand. "C_EIOU"}
#' }
#'
#' @examples
#' alloc_cols
"alloc_cols"



#' Sankey diagram data frame column names
#'
#' A string list containing named names of columns in Sankey data frames.
#' Items in the list provide default values for column name function arguments
#' throughout the `Recca` package.
#'
#' @format A string list with `r length(sankey_cols)` entries.
#' \describe{
#' \item{sankey}{The name of a column in a wide-by-matrices data frame containing Sankey diagrams.}
#' }
#'
#' @examples
#' sankey_cols
"sankey_cols"



#' Primary industry column names
#'
#' A string list containing named names of columns in SUT data frames.
#' Items in the list provide default values for column name function arguments
#' throughout the `Recca` package.
#'
#' @format A string list with `r length(industry_cols)` entries.
#' \describe{
#' \item{p_industries_prefixes}{The name of a column in a wide-by-matrices data frame containing prefixes for names of primary industries.}
#' \item{p_industries_complete}{The name of a column in a wide-by-matrices data frame containing complete names of primary industries.}
#' }
#'
#' @examples
#' industry_cols
"industry_cols"




#' Energy consumption in the UK in 2000
#'
#' A dataset containing approximations to
#' some of the energy flows in the UK in the year 2000.
#' These data first appeared as the example in
#' Figures 3, 4, 6, 7, 10, 11, B.1, and B.2 of
#' M.K. Heun, A. Owen, and P. E. Brockway.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Applied Energy, 226:1134–1162, Sep 2018.
#'
#' \code{UKEnergy2000tidy} gives each non-zero entry in \code{UKEnergy2000mats}
#' as a single column in a data frame.
#' These data are in tidy format.
#'
#' @format A data frame with 186 rows and 9 variables:
#' \describe{
#'   \item{Country}{country, GB (Great Britain, only one country)}
#'   \item{Year}{year, 2000 (only one year)}
#'   \item{LedgerSide}{Supply or Consumption}
#'   \item{FlowAggregationPoint}{tells where each row should be aggregated}
#'   \item{EnergyType}{E.ktoe (for energy) or X.ktoe (for exergy)}
#'   \item{LastStage}{tells the final stage of the energy conversion chain: final, useful, or services}
#'   \item{Flow}{the Industry or Sector involved in this flow}
#'   \item{Product}{the energy product involved in this flow}
#'   \item{Edot}{value of the energy, exergy, or service flow in ktoe}
#'   \item{Unit}{unit in which quantity is expressed}
#' }
#' @source \doi{10.1016/j.apenergy.2018.05.109}
"UKEnergy2000tidy"



#' Energy consumption in the UK in 2000
#'
#' A dataset containing approximations to
#' some of the energy flows in the UK in the year 2000.
#' These data first appeared as the example in
#' Figures 3, 4, 6, 7, 10, 11, B.1, and B.2 of
#' M.K. Heun, A. Owen, and P. E. Brockway.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Applied Energy, 226:1134–1162, Sep 2018.
#'
#' \code{UKEnergy2000mats} gives the use (\code{U}), make (\code{V}), and final demand (\code{Y}) matrices
#' associated with \code{UKEnergy2000tidy}.
#' These data are in \pkg{matsindf} format.
#'
#' @format A data frame with 12 rows and 6 variables:
#' \describe{
#'   \item{Country}{country, GB (Great Britain, only one country)}
#'   \item{Year}{year, 2000 (only one year)}
#'   \item{EnergyType}{E.ktoe (for energy) or X.ktoe (for exergy)}
#'   \item{LastStage}{tells the final stage of the energy conversion chain: final, useful, or services}
#'   \item{matrix.name}{gives the name of the matrix}
#'   \item{matrix}{gives use (U), make (V), final demand (Y), r_EIOU, and S_units matrices}
#' }
#' @source \doi{10.1016/j.apenergy.2018.05.109}
"UKEnergy2000mats"



#' Example energy conversion chain to demonstrate perfect substitution
#'
#' A dataset containing an example energy conversion chain
#' for the purpose of demonstrating changes to industry inputs
#' where the inputs are perfect substitutes.
#'
#' \code{PerfectSubtidy} gives each non-zero entry in \code{PerfectSubmats}
#' as a single column in a data frame.
#' These data are in tidy format.
#'
#' @format A data frame with 20 rows and 10 variables:
#' \describe{
#'   \item{Country}{country, (Example, only one country)}
#'   \item{Year}{year, 2000 (only one year as an example)}
#'   \item{LedgerSide}{Supply or Consumption}
#'   \item{FlowAggregationPoint}{tells where each row should be aggregated}
#'   \item{EnergyType}{E (for energy) or X (for exergy)}
#'   \item{LastStage}{tells the final stage of the energy conversion chain: services is the only entry here}
#'   \item{Flow}{the Industry or Sector involved in this flow}
#'   \item{Product}{the energy product involved in this flow}
#'   \item{Edot}{value of the energy, exergy, or service flow in ktoe}
#'   \item{Unit}{unit in which quantity is expressed}
#' }
"PerfectSubtidy"



#' Example energy conversion chain to demonstrate perfect substitution
#'
#' A dataset containing an example energy conversion chain
#' for the purpose of demonstrating changes to industry inputs
#' where the inputs are perfect substitutes.
#'
#' \code{PerfectSubmats} gives the use (\code{U}), make (\code{V}), and final demand (\code{Y}) matrices
#' as a single column in a data frame.
#' These data are in \pkg{matsindf} format.
#'
#' @format A data frame with 20 rows and 10 variables:
#' \describe{
#'   \item{Country}{country, (Example, only one country)}
#'   \item{Year}{year, 2000 (only one year as an example)}
#'   \item{EnergyType}{E.ktoe (for energy) or X.ktoe (for exergy)}
#'   \item{LastStage}{tells the final stage of the energy conversion chain: services is the only entry here}
#'   \item{matrix.name}{gives the name of the matrix}
#'   \item{matrix}{gives use (U), make (V), final demand (Y), r_EIOU, and S_units matrices}
#' }
"PerfectSubmats"



#' A vector of phi (exergy-to-energy ratios) values
#'
#' Converting from energy to exergy requires vectors of phi values.
#' This object is a vector to assist that conversion process
#' for the `UKEnergy2000mats` energy conversion chain bundled with this package.
#'
#' @format A matrix with energy products in rows and phi values in a single column.
"phi_vec"



#' A list of energy types
#'
#' A list of energy type options in the "EnergyType" column.
#'
#' @format A list with `r length(energy_types)` entries.
#' \describe{
#'   \item{energy_type}{The name of the energy type column, "EnergyType".}
#'   \item{e}{energy}
#'   \item{x}{exergy}
#' }
"energy_types"



#' A list of High pressure matter (HPM) useful work products
#'
#' A list of High pressure matter (HPM) useful work products.
#'
#' @format A list with `r length(hpm_eu.products)` entries.
#' \describe{
#'   \item{HPA}{The useful work product (Eu.product) "High pressure air"}
#'   \item{HPL}{The useful work product (Eu.product) "High pressure liquids"}
#'   \item{HPNG}{The useful work product (Eu.product) "High pressure natural gas"}
#' }
"hpm_eu.products"



#' A list of High temperature heat (HTH) useful work products
#'
#' A list of High temperature heat (HTH) useful work products
#'
#' @format A list with `r length(hth_eu.products)` entries.
#' \describe{
#'   \item{HTH.400.C}{The useful work product (Eu.product) "HTH.400.C"}
#'   \item{HTH.600.C}{The useful work product (Eu.product) "HTH.600.C"}
#'   \item{HTH.850.C}{The useful work product (Eu.product) "HTH.850.C"}
#'   \item{HTH.960.C}{The useful work product (Eu.product) "HTH.960.C"}
#'   \item{HTH.1000.C}{The useful work product (Eu.product) "HTH.1000.C"}
#'   \item{HTH.1300.C}{The useful work product (Eu.product) "HTH.1300.C"}
#'   \item{HTH.1600.C}{The useful work product (Eu.product) "HTH.1600.C"}
#' }
"hth_eu.products"



#' A list of Medium temperature heat (MTH) useful work products
#'
#' A list of Medium temperature heat (MTH) useful work products
#'
#' @format A list with `r length(mth_eu.products)` entries.
#' \describe{
#'   \item{MTH.100.C}{The useful work product (Eu.product) "MTH.100.C"}
#'   \item{MTH.200.C}{The useful work product (Eu.product) "MTH.200.C"}
#' }
"mth_eu.products"



#' A list of Low temperature heat (LTH) useful work products
#'
#' A list of Low temperature heat (LTH) useful work products
#'
#' @format A list with `r length(lth_eu.products)` entries.
#' \describe{
#'   \item{LTH.20.C}{The useful work product (Eu.product) "LTH.20.C"}
#'   \item{LTH.60.C}{The useful work product (Eu.product) "LTH.60.C"}
#' }
"lth_eu.products"



#' A list of Cooling useful work products
#'
#' A list of Cooling useful work products
#'
#' @format A list with `r length(cooling_eu.products)` entries.
#' \describe{
#'   \item{LTC.-10.C}{The useful work product (Eu.product) "LTC.-10.C"}
#'   \item{LTC.20.C}{The useful work product (Eu.product) "LTC.20.C"}
#' }
"cooling_eu.products"



#' A list of Propulsion useful work products
#'
#' A list of Propulsion useful work products
#'
#' @format A list with `r length(propulsion_eu.products)` entries.
#' \describe{
#'   \item{MP}{The useful work product (Eu.product) Marine propulsion}
#'   \item{RoP}{The useful work product (Eu.product) Road propulsion}
#'   \item{RaP}{The useful work product (Eu.product) Rail propulsion}
#' }
"propulsion_eu.products"



#' A list of Mechanical useful work products
#'
#' A list of Mechanical useful work products
#'
#' @format A list with `r length(mechanical_eu.products)` entries.
#' \describe{
#'   \item{MD}{The useful work product (Eu.product) Mechanical drive}
#'   \item{KE}{The useful work product (Eu.product) Kinetic energy}
#'   \item{MW}{The useful work product (Eu.product) Mechanical work}
#'   \item{MF}{The useful work product (Eu.product) Material fracture}
#' }
"mechanical_eu.products"



#' A list of Information processing useful work products
#'
#' A list of Information processing useful work products
#'
#' @format A list with `r length(ip_eu.products)` entries.
#' \describe{
#'   \item{IP}{The useful work product (Eu.product) Information processing}
#' }
"ip_eu.products"



#' A list of Light useful work products
#'
#' A list of Light useful work products
#'
#' @format A list with `r length(l_eu.products)` entries.
#' \describe{
#'   \item{L}{The useful work product (Eu.product) Light}
#' }
"l_eu.products"



#' A list of Non-energy use useful work products
#'
#' A list of Non-energy use useful work products
#'
#' @format A list with `r length(neu_eu.products)` entries.
#' \describe{
#'   \item{NEU}{The useful work product (Eu.product) Non-energy use}
#' }
"neu_eu.products"



#' A list of Name - Eu.product list combinations
#'
#' A list of Name - Eu.product list combinations for use in aggregating individual
#' useful work products in to groups
#'
#' @format A list with `r length(euproduct_aggregation_map)` entries.
"euproduct_aggregation_map"



#' Names of data frame columns containing Product and Industry matrix row and column names
#'
#' A list of data frame column names.
#' The data frame columns contain vectors of unique Product and Industry column names
#' from **R**, **U**, **V**, and **Y** matrices.
#'
#' @format A list with `r length(prod_ind_names_colnames)` entries.
#' \describe{
#'   \item{product_names}{The name of a data frame column that contains Products. Default is "Product.names".}
#'   \item{industry_names}{The name of a data frame column that contains Industries. Default is "Industry.names".}
#' }
"prod_ind_names_colnames"


#' Names of row and column types in PSUT matrices
#'
#' A list of row and column types.
#'
#' @format A list with `r length(prod_ind_names_colnames)` entries.
#' \describe{
#'   \item{product_type}{The name of the product row and column type. Default is "Product".}
#'   \item{industry_type}{The name of the industry row and column type. Default is "Industry".}
#'   \item{unit_type}{The name of the unit row and column type. Default is "Unit".}
#' }
"row_col_types"


#' All energy conversion chain stages
#'
#' A string list containing options for the all stages of energy conversion chain analysis.
#'
#' @format A string list with `r length(all_stages)`
#' \describe{
#' \item{primary}{The string identifier for the Primary stage of the energy conversion chain.}
#' \item{final}{The string identifier for the Final stage of the energy conversion chain.}
#' \item{useful}{The string identifier for the Useful stage of the energy conversion chain.}
#' \item{services}{The string identifier for the Services stage of the energy conversion chain.}
#' \item{last_stage_sep}{A string that separates last-stage identifiers in variable names.}
#' }
#' @examples
#' all_stages
"all_stages"


