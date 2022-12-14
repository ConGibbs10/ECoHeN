% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_arguments.R
\name{check_arguments}
\alias{check_arguments}
\title{Verify that ECoHeN arguments meet expectations.}
\usage{
check_arguments(G, node_type, alpha, loco_samples)
}
\arguments{
\item{G}{igraph representation of graph.}

\item{node_type}{Name of vertex attribute containing the node types.}

\item{alpha}{Numeric between 0 and 1 representing significance level.}

\item{loco_samples}{Logical indicating whether or not to consider only locally optimal seed sets.}
}
\value{
Possibly, G.
}
\description{
Verify that ECoHeN arguments meet expectations.
}
\keyword{internal}
