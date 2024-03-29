% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resamp_eval.R
\name{run_psa}
\alias{run_psa}
\title{Run Probabilistic Uncertainty Analysis}
\usage{
run_psa(model, psa, N, keep = FALSE)
}
\arguments{
\item{model}{The result of \code{\link[=run_model]{run_model()}}.}

\item{psa}{Resampling distribution for parameters defined
by \code{\link[=define_psa]{define_psa()}}.}

\item{N}{> 0. Number of simulation to run.}

\item{keep}{logical; if TRUE, all models will be returned}
}
\value{
A list with the following elements
\itemize{
\item psa:  a \code{data.frame} with one row per model.
\item run_model: a \code{data.frame} with mean cost and utility for each strategy
\item model: the initial model object
\item N: the number of simulations ran
\item resamp_par: the resampled parameters
\item full: if \code{keep} is TRUE, a list of each model objects created at each iteration
}
}
\description{
Run Probabilistic Uncertainty Analysis
}
\examples{
# example for run_psa

mod1 <- define_strategy(
  transition = define_transition(
    .5, .5,
    .1, .9
  ),
  define_state(
    cost = cost_init + age * 5,
    ly = 1
  ),
  define_state(
    cost = cost_init + age,
    ly = 0
  )
)

mod2 <- define_strategy(
  transition = define_transition(
    p_trans, C,
    .1, .9
  ),
  define_state(
    cost = 789 * age / 10,
    ly = 1
  ),
  define_state(
    cost = 456 * age / 10,
    ly = 0
  )
  
)

res2 <- run_model(
  mod1, mod2,
  parameters = define_parameters(
    age_init = 60,
    cost_init = 1000,
    age = age_init + model_time,
    p_trans = .7
  ),
  init = 1:0,
  cycles = 10,
  cost = cost,
  effect = ly
)

rsp <- define_psa(
  age_init ~ normal(60, 10),
  cost_init ~ normal(1000, 100),
  p_trans ~ binomial(.7, 100),
  correlation = matrix(c(
    1,  .4, 0,
    .4, 1,  0,
    0,  0,  1
  ), byrow = TRUE, ncol = 3)
)


# with run_model result
# (only 10 resample for speed)
ndt1 <- run_psa(res2, psa = rsp, N = 10)
}
