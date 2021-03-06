% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/node_edge_list.R
\name{add_node_ids}
\alias{add_node_ids}
\title{Add node ID numbers to an edge list}
\usage{
add_node_ids(
  edge_list,
  from = "From",
  to = "To",
  node_id = "node_id",
  first_node = 0
)
}
\arguments{
\item{edge_list}{the edge list to which node ID numbers are to be added}

\item{from}{the name of the column containing source nodes. (Default is "\code{From}".)}

\item{to}{the name of the column containing destination nodes. (Default is "\code{To}".)}

\item{node_id}{the root of the column name for node IDs. (Default is "\code{node_ID}".)  See details.}

\item{first_node}{the ID number of the first node. (Default is \code{0}.)}
}
\value{
\code{edge_list} with two additional columns containing \code{From} and \code{To} node ID numbers.
}
\description{
Edge lists can contain identification numbers (integers) for each node.
Because each row in the edge list data frame contains a "\code{From}" node
and a "\code{To}" node, two columns of node IDs are added, one for "\code{From}"
and one for "\code{To}".
}
\details{
The column names for the "\code{From}" and "\code{To}" nodes are created by \code{paste}-ing
the value of the \code{from} and \code{to} arguments with the value of the \code{node_id} argument.
}
\examples{
library(matsbyname)
library(tidyr)
sutmats <- UKEnergy2000mats \%>\% spread(key = matrix.name, value = matrix)
# Suppress adding node IDs
elDF <- edge_list(sutmats, node_id = NULL)$`Edge list`[[1]]
add_node_ids(elDF)
}
