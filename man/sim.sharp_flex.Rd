% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perturbations.R
\name{sim.sharp_flex}
\alias{sim.sharp_flex}
\title{Signal perturbation: sharp flex}
\usage{
sim.sharp_flex(baseline, range, range_by_time = TRUE, strength)
}
\arguments{
\item{baseline}{baseline, as generated by function \code{sim.baseline}, possibly perturbed by other functions.}

\item{range}{indices of affected values. Either a start and end index or the whole range. If length of \code{range} is 2 and second value is infinity, the second value is converted to highest possible value. If \code{range} is \code{NULL}, maximum range (1 to max) is taken.}

\item{range_by_time}{Boolean, indicates whether values in \code{range} correspond to value of \code{time} of signal, rather than row indices. This is recommended, since time values for fluorescence signals with non-uniform flow rate are also not uniform.}

\item{strength}{factor by which to stretch the selected values.}
}
\value{
Modified \code{baseline} input.
}
\description{
Distort a baseline signal by stretching or contracting the signal values (independently of time), effectively changing their variance, uniformly.
}
\seealso{
See functions beginning with \code{sim.sample.} for examples of usage.
}
