% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DataLoad.R
\name{CentroidCollect}
\alias{CentroidCollect}
\title{Wrapper for centroid calculation}
\usage{
CentroidCollect(CoordTable, MouseLabels, Overwrite = TRUE)
}
\arguments{
\item{CoordTable}{DataTable with coordinates.}

\item{MouseLabels}{A list with string vectors for labels.}

\item{Overwrite}{A bool indicating if output should be overwritten if it exists already (default = TRUE).}
}
\value{
Add centroid coordinates
}
\description{
This function is a wrapper for the \code{AddCentroid} and allows computation of 
centroids for multiple object groups provided by a list.
}
