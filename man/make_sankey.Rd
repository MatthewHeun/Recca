% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sankey.R
\name{make_sankey}
\alias{make_sankey}
\title{Make a Sankey diagram}
\usage{
make_sankey(
  .sutmats = NULL,
  R = Recca::psut_cols$R,
  U = Recca::psut_cols$U,
  V = Recca::psut_cols$V,
  Y = Recca::psut_cols$Y,
  simplify_edges = TRUE,
  colour_string = NULL,
  name_col = "name",
  colour_col = "colour",
  sankey = Recca::sankey_cols$sankey,
  ...
)
}
\arguments{
\item{.sutmats}{An optional wide-by-matrices data frame}

\item{R, U, V, Y}{See \code{Recca::psut_cols}.}

\item{simplify_edges}{A boolean which tells whether edges should be simplified.
Applies to every row of \code{.sutmats} if \code{.sutmats} is specified.}

\item{colour_string}{An optional Javascript string that defines colours
for the Sankey diagram,
appropriate for use by \code{\link[networkD3:sankeyNetwork]{networkD3::sankeyNetwork()}}.
Default is \code{NULL}, meaning that the default
color palette should be used, namely
\code{networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20);")}.
Can be a data frame with \code{name_col} and \code{colour_col} columns
in which case \code{\link[=create_sankey_colour_string]{create_sankey_colour_string()}}
is called internally
with \code{colour_string}, \code{name_col}, and \code{colour_col}
as the arguments.}

\item{name_col, colour_col}{The names of columns in \code{colour_df} for
names of nodes and flows (\code{name_col}) and
colours of nodes and flows (\code{colour_col}).
Defaults are "name" and "colour", respectively.}

\item{sankey}{See \code{Recca::sankey_cols}.}

\item{...}{Arguments passed to \code{\link[networkD3:sankeyNetwork]{networkD3::sankeyNetwork()}},
mostly for formatting purposes.}
}
\value{
a Sankey diagram
}
\description{
A Sankey diagram is a flow diagram in which the width of the lines is proportional
to the rate of energy flow.
Sankey diagrams are a helpful way to visualize energy flows in an energy conversion chain (ECC).
This function takes a matrix description of an ECC and produces a Sankey diagram.
}
\details{
At present, this function uses \code{\link[networkD3:sankeyNetwork]{networkD3::sankeyNetwork()}} to draw the Sankey diagram.

If any of \code{R}, \code{U}, \code{V}, or \code{Y} is \code{NA}, \code{NA} is returned.

Note that there appears to be a colour bug
in \code{\link[networkD3:sankeyNetwork]{networkD3::sankeyNetwork()}}
when a node name ends in a ".".
Colours for those nodes does not render correctly.
}
\examples{
library(dplyr)
library(magrittr)
library(networkD3)
library(tidyr)
# Default colours are likely to appear nearly random.
UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = "matrix.name",
                     values_from = "matrix") |>
  make_sankey() |>
  extract2("Sankey") |>
  extract2(1)
# Create your own colour palette.
colour_df <- tibble::tribble(~name, ~colour,
                             "Resources [of Crude]", "gray",
                             "Crude dist.",          "gray",
                             "Oil fields",           "gray",
                             "Oil refineries",       "gray",
                             "Diesel dist.",         "gray",
                             "Power plants",         "gray",
                             "Elect. grid",          "gray",
                             "Resources [of NG]",    "gray",
                             "NG dist.",             "gray",
                             "Gas wells & proc.",    "gray",
                             "Petrol dist.",         "gray",
                             "Transport",            "gray",
                             "Residential",          "gray",
                             "Waste",                "gray",
                             "Crude",                "black",
                             "Crude [from Dist.]",   "black",
                             "Crude [from Fields]",  "black",
                             "Diesel",               "brown",
                             "Diesel [from Dist.]",  "brown",
                             "Diesel [from Fields]", "brown",
                             "Elect",                "yellow",
                             "Elect [from Grid]",    "yellow",
                             "NG",                   "lightblue",
                             "NG [from Dist.]",      "lightblue",
                             "NG [from Wells]",      "lightblue",
                             "Petrol",               "orange",
                             "Petrol [from Dist.]",  "orange")
colour_df
UKEnergy2000mats |>
  tidyr::pivot_wider(names_from = "matrix.name",
                     values_from = "matrix") |>
  make_sankey(colour_string = colour_df,
              # Arguments are passed to networkD3::sankeyNetwork()
              fontSize = 10,
              fontFamily = "Helvetica",
              units = "ktoe",
              width = 400, # pixels
              height = 400 # pixels
  ) |>
  extract2("Sankey") |>
  extract2(1)
}
