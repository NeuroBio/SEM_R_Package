% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Source_Parameters_and_Simulation.R
\name{TestRequirement}
\alias{TestRequirement}
\title{Test Requirement}
\usage{
TestRequirement(test, dependancy1 = 0, dependancy2 = FALSE)
}
\arguments{
\item{test}{a SaveTrait}

\item{dependancy1}{a value for trait noise}

\item{dependancy2}{a secon dependancy that requires teh trait}
}
\description{
If a Save parameter is set to NA, checks whether they should be set to TRUE or FALSE.
}
\keyword{error-check}
