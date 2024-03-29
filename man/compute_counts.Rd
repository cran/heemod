% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/part_surv.R, R/strategy_eval.R
\name{compute_counts.eval_part_surv}
\alias{compute_counts.eval_part_surv}
\alias{compute_counts}
\alias{compute_counts.eval_matrix}
\title{Compute Count of Individual in Each State per Cycle}
\usage{
\method{compute_counts}{eval_part_surv}(x, init, inflow, ...)

compute_counts(x, ...)

\method{compute_counts}{eval_matrix}(x, init, inflow, ...)
}
\arguments{
\item{x}{An \code{eval_matrix} or
\code{eval_part_surv} object.}

\item{init}{numeric vector, same length as number of
model states. Number of individuals in each model state
at the beginning.}

\item{inflow}{numeric vector, similar to \code{init}.
Number of new individuals in each state per cycle.}
}
\value{
A \code{cycle_counts} object.
}
\description{
Given an initial number of individual and an evaluated
transition matrix, returns the number of individual per
state per cycle.
}
\details{
Use the \code{method} argument to specify if transitions
are supposed to happen at the beginning or the end of
each cycle. Alternatively linear interpolation between
cycles can be performed.
}
\keyword{internal}
