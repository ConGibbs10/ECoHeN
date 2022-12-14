% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_irregularities.R
\name{check_irregularities}
\alias{check_irregularities}
\title{Verify that G meets conditions for ECoHeN.}
\usage{
check_irregularities(G, node_type)
}
\arguments{
\item{G}{igraph representation of graph.}

\item{node_type}{name of vertex attribute containing the node types.}
}
\value{
The heterogeneous graph, possibly edited to meet expectations for method.
}
\description{
Verify that G meets conditions for ECoHeN.
}
\keyword{internal}
