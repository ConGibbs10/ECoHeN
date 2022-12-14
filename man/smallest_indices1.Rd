% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{smallest_indices1}
\alias{smallest_indices1}
\title{Identifies the smallest indices in a vector.}
\usage{
smallest_indices1(x, N)
}
\arguments{
\item{Vector}{of doubles.}

\item{Integer}{for the number of indices to identify.}
}
\description{
Identifies the smallest indices in a vector.
}
\details{
Identifies the indices of the N smallest values in a double vector x,
breaking ties uniformly at random. Uses a full sort.
}
\keyword{internal}
