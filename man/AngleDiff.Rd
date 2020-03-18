% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Angles.R
\name{AngleDiff}
\alias{AngleDiff}
\title{Calculate difference between two angles (radians)}
\usage{
AngleDiff(CoordTable, Angle1, Angle2, OutputName)
}
\arguments{
\item{CoordTable}{A table including coordinates of labels.}

\item{Angle1}{A double indicating the angle in radians.}

\item{Angle2}{A double indicating the angle in radians.}

\item{OutputName}{A string indicating for the output angle difference.}
}
\value{
Modifies existing DataTable.
}
\description{
This function calculates the difference between two angles from a reference 
to objects from an object table and adds the resulting distances to the DataTable as columns.
}