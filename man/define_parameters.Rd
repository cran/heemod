% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameters.R
\name{define_parameters}
\alias{define_parameters}
\alias{modify.uneval_parameters}
\title{Define Markov Model Parameters}
\usage{
define_parameters(...)

\method{modify}{uneval_parameters}(.OBJECT, ..., BEFORE)
}
\arguments{
\item{...}{Name-value pairs of expressions definig 
parameters.}

\item{.OBJECT}{An object of class
\code{uneval_parameters}.}

\item{BEFORE}{character, length 1. Name of parameters 
before which new parameters are to be added.}
}
\value{
An object of class \code{uneval_parameters} 
  (actually a named list of \code{lazy} expressions).
}
\description{
Define parameters called to compute the transition matrix
or state values for a Markov model. Parameters can be 
time dependent by using the \code{markov_cycle} 
parameter.
}
\details{
Parameters are defined sequencially, parameters defined 
earlier can be called in later expressions.

Vector length should not be explicitely set, but should 
instead be stated relatively to \code{markov_cycle} 
(whose length depends on the number of simulation 
cycles). Alternatively, \code{dplyr} functions such as 
\code{n()} or \code{row_numbers()} can be used.

This function relies heavily on the \code{dplyr} package.
Parameter definitions should thus mimic the use of
functions such as \code{mutate}.

Variable names are searched first in the parameter 
definition (only parameters defined earlier are visible) 
then in the environment where \code{define_parameters} 
was called.

For the \code{modify} function, existing parameters are 
modified, new parameters are added at the end by default 
if \code{BEFORE} is not specified. Parameter order 
matters since only parameters defined earlier can be 
referenced in later expressions.
}
\examples{

# parameter 'age' depends on time:
# simulating a cohort starting at 60 yo

define_parameters(
  age_start = 60,
  age = age_start + markov_cycle
)

# other uses of markov_cycle are possible

define_parameters(
  top_time = ifelse(markov_cycle < 10, 1, 0)
)
  
# more elaborate: risk function

define_parameters(
  rate = 1 - exp(- markov_time * .5)
)

\dontrun{
# dont explicitely state lengths
define_parameters(
  var = seq(1, 15, 2)
)
}

# instead rely on markov_cycle or dplyr 
# functions such as n() or row_number()

define_parameters(
 var = seq(from = 1, length.out = n(), by = 3),
 var2 = seq(1, length(markov_cycle), 2)
)

param <- define_parameters(
  age_start = 60,
  age = age_start + markov_cycle
)

# adding new parameters

modify(
  param,
  const = 4.4,
  age_2 = age ^ 2
)

# modify existing parameters

modify(
  param,
  age_start = 40
)

# specify new parameter position

modify(
  param,
  var = 3.14,
  BEFORE = "age"
)

}
