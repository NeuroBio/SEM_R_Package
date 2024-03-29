% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Source_Learning.R
\name{DropSyllables}
\alias{DropSyllables}
\title{Drop Syllables}
\usage{
DropSyllables(chanceFor, tutorSongs, learnerSongs)
}
\arguments{
\item{chanceFor}{the learners' chance to forget}

\item{tutorSongs}{a matrix of tutor syllable vectors}

\item{learnerSongs}{a matrix of learner syllable vectors}
}
\description{
Tests whether a learner forgets a syllable that he knows, but that his tutor did not sing.
}
\keyword{song-learning}
