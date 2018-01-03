\name{Recca-package}
\alias{Recca-package}
\alias{Recca}
\docType{package}
\title{
\packageTitle{Recca}
}
\description{
\packageDescription{Recca}
}
\details{
    An energy conversion chain (ECC) is a set of energy carriers,
    energy transformation devices, and
    energy services
    within spatial and temporal boundaries of interest.
    An economy-spanning ECC is comprised of
    primary energy carriers (raw energy, such as fossil fuels),
    final energy carriers (energy sold to consumers,
                           such as electricity and refined petroleum products), and
    useful energy carriers (energy created in machines owned by consumers,
                            such as mechanical drive and heat), and
    energy services (such as transport, space heating, and illumination).
    ECC analyses query economy-spanning ECCs
    for features, themes, and metrics
    that elucidate historical trends and allow projection of future scenarios.
    \code{\link[Recca:Recca-package]{Recca}}
    provides functions that
    aggregate energy aggregates at all stages of the ECC
    (primary, final, useful, and services),
    calculate physical supply use table (PSUT) matrices to describe the input-output
    structure of an ECC, and
    otherwise assist the evaluation of an ECC.
}
\author{
\packageAuthor{Recca}

\strong{Maintainer}: \packageMaintainer{Recca}
}
\references{
M.K. Heun, A. Owen, and P.E. Brockway.
A physical supply-use table framework for energy analysis on the energy conversion chain.
Sustainability Research Institute Paper 111,
University of Leeds,
School of Earth and Environment,
Sustainability Research Institute,
Leeds, England, 13 November 2017.
\url{http://www.see.leeds.ac.uk/fileadmin/Documents/research/sri/workingpapers/sri-wp111.pdf}
}
\keyword{
array
}
\seealso{
The
\code{\link[Recca:Recca-package]{Recca}}
package relies upon several other packages, including
\code{\link[qgraph:qgraph-package]{qgraph}},
\code{\link[igraph:igraph-package]{igraph}},
\code{\link[matsindf:matsindf-package]{matsindf}},
and
\code{\link[byname:byname-package]{byname}}.
}
\examples{
library(magrittr)
library(dplyr)
}
