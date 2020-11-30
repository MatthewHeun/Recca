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
#' \item{s_units}{The name of a column in a wide-by-matrices data frame containing unit summation (`S_units`) matrices.}
#' \item{matvals}{The name of a column in a tidy data frame containing matrices.}
#' }
#'
#' @examples
#' psut_cols
"psut_cols"



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
#'   \item{Ledger.side}{Supply or Consumption}
#'   \item{Flow.aggregation.point}{tells where each row should be aggregated}
#'   \item{Energy.type}{E.ktoe (for energy) or X.ktoe (for exergy)}
#'   \item{Last.stage}{tells the final stage of the energy conversion chain: final, useful, or services}
#'   \item{Flow}{the Industry or Sector involved in this flow}
#'   \item{Product}{the energy product involved in this flow}
#'   \item{E.dot}{value of the energy, exergy, or service flow in ktoe}
#'   \item{Unit}{unit in which quantity is expressed}
#' }
#' @source \url{https://doi.org/10.1016/j.apenergy.2018.05.109}
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
#'   \item{Energy.type}{E.ktoe (for energy) or X.ktoe (for exergy)}
#'   \item{Last.stage}{tells the final stage of the energy conversion chain: final, useful, or services}
#'   \item{matrix.name}{gives the name of the matrix}
#'   \item{matrix}{gives use (U), make (V), final demand (Y), r_EIOU, and S_units matrices}
#' }
#' @source \url{https://doi.org/10.1016/j.apenergy.2018.05.109}
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
#'   \item{Ledger.side}{Supply or Consumption}
#'   \item{Flow.aggregation.point}{tells where each row should be aggregated}
#'   \item{Energy.type}{E.ktoe (for energy) or X.ktoe (for exergy)}
#'   \item{Last.stage}{tells the final stage of the energy conversion chain: services is the only entry here}
#'   \item{Flow}{the Industry or Sector involved in this flow}
#'   \item{Product}{the energy product involved in this flow}
#'   \item{E.dot}{value of the energy, exergy, or service flow in ktoe}
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
#'   \item{Energy.type}{E.ktoe (for energy) or X.ktoe (for exergy)}
#'   \item{Last.stage}{tells the final stage of the energy conversion chain: services is the only entry here}
#'   \item{matrix.name}{gives the name of the matrix}
#'   \item{matrix}{gives use (U), make (V), final demand (Y), r_EIOU, and S_units matrices}
#' }
"PerfectSubmats"
