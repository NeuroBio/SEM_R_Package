% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Source_StatsPlotsandTesting.R
\name{TerritoryHeatMap}
\alias{TerritoryHeatMap}
\title{Territory Heat Map}
\usage{
TerritoryHeatMap(P, index = 1, trait, max = NA)
}
\arguments{
\item{P}{a list of parameters}

\item{index}{which column to plot}

\item{trait}{a matrix of SEM data from a Basic sim (individual data)}
}
\description{
Creates a heat map showing the magnitude of a trait in each territory for a given timestep.  Requires individual data.
}
\keyword{stats-plotting}
