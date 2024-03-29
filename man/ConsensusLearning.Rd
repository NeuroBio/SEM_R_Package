% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Source_Learning.R
\name{ConsensusLearning}
\alias{ConsensusLearning}
\title{Consensus Learning}
\usage{
ConsensusLearning(P, population, learners, vacancy)
}
\arguments{
\item{P}{a list of parameters}

\item{population}{the population of birds}

\item{learners}{the indicies of birds that will attempt to learn}

\item{vacancy}{the indicies of dead birds}
}
\description{
Allows birds to sample multiple tutors to create a consensus song as a template to decide what to learn.  Birds then learn.
}
\keyword{song-learning}
