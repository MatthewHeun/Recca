#' Energy consumption in the UK in 2000
#'
#' A dataset containing approximations to
#' some of the energy flows in the UK in the year 2000.
#' These data first appeared as the example in
#' Figures 3, 4, 6, 7, 10, 11, B.1, and B.2 of
#' M.K. Heun, A. Owen, and P.E. Brockway.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Sustainability Research Institute Paper 111,
#' University of Leeds, School of Earth and Environment,
#' Sustainability Research Institute,
#' Leeds, England,
#' 13 November 2017.
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
#'   \item{EX.ktoe}{value of the energy, exergy, or service flow in ktoe}
#'   \item{Unit}{unit in which quantity is expressed}
#' }
#' @source \url{http://www.see.leeds.ac.uk/fileadmin/Documents/research/sri/workingpapers/sri-wp111.pdf}
"UKEnergy2000tidy"



#' Energy consumption in the UK in 2000
#'
#' A dataset containing approximations to
#' some of the energy flows in the UK in the year 2000.
#' These data first appeared as the example in
#' Figures 3, 4, 6, 7, 10, 11, B.1, and B.2 of
#' M.K. Heun, A. Owen, and P.E. Brockway.
#' A physical supply-use table framework for energy analysis on the energy conversion chain.
#' Sustainability Research Institute Paper 111,
#' University of Leeds, School of Earth and Environment,
#' Sustainability Research Institute,
#' Leeds, England,
#' 13 November 2017.
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
#' @source \url{http://www.see.leeds.ac.uk/fileadmin/Documents/research/sri/workingpapers/sri-wp111.pdf}
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
#'   \item{EX.ktoe}{value of the energy, exergy, or service flow in ktoe}
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
