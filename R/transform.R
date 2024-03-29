#' Convenience Functions to Compute Probabilities
#' 
#' These convenience functions make it easier to compute 
#' transition probabilities from incidence rates, OR, RR, or
#' probabilities estimated on a different timeframe.
#' 
#' @name probability
#' @param p Probability.
#' @param r Rate.
#' @param or Odds ratio.
#' @param rr Relative risk.
#' @param to Compute probability for that timeframe.
#' @param from Timeframe of the original probability.
#' @param per Number of person-time corresponding to the
#'   rate.
#' @param ... For deprecated functions.
#'   
#' @return A probability.
#'   
#' @example inst/examples/example_transform.R
NULL

#' @export
#' @rdname probability
rescale_prob <- function(p, to = 1, from = 1) {
  stopifnot(
    p >= 0,
    p <= 1,
    to > 0,
    from > 0
  )
  r <- - log(1 - p) / from
  rate_to_prob(r, to = to)
}

#' @export
#' @rdname probability
prob_to_prob <- function(...) {
  warning("'prob_to_prob' is deprecated, use 'rescale_prob()' instead.")
  rescale_prob(...)
}

#' @export
#' @rdname probability
rate_to_prob <- function(r, to = 1, per = 1) {
  stopifnot(
    r > 0,
    to > 0,
    per > 0
  )
  r <- r / per
  1 - exp(- r * to)
}

#' @export
#' @rdname probability
or_to_prob <- function(or, p) {
  stopifnot(
    or >= 0,
    p >= 0,
    p <= 1
  )
  
  res <- plogis(qlogis(p) + log(or))
  
  stopifnot(
    res >= 0,
    res <= 1
  )
  
  res
}

#' @export
#' @rdname probability
rr_to_prob <- function(rr, p) {
  res <- rr * p
  stopifnot(
    rr >= 0,
    p >= 0,
    p <= 1,
    res >= 0,
    res <= 1
  )
  res
}

#' Rescale Discount Rate
#' 
#' Rescale a discount rate between two time frames.
#' 
#' Continuous discounting is assumed, i.e. when converting a
#' long-term discount rate into a short-term rate, we assume
#' that a partial gain from one short term is 
#' multiplicatively discounted in all following short terms.
#' At the same time, we assume the short-term rate is
#' time-invariant.
#' 
#' @param x Discount rate to rescale.
#' @param from Original time period.
#' @param to Final time period.
#'   
#' @return Rate rescaled under the assumption of compound 
#'   discounting.
#'   
#' @export
#' 
#' @examples
#'   ## 1% monthly interest rate to annual
#'   rescale_discount_rate(0.01, 1, 12)
#'   ## 3% annual discount rate to (approximately) weekly 
#'   rescale_discount_rate(0.03, 52, 1)
rescale_discount_rate <- function(x, from, to) {
  ((1 + x) ^ (to / from)) -1
}

#' Combine Probabilities
#' 
#' Given several independent probabilities of an event, 
#' return the final probability of the event.
#' 
#' This function is only correct if the probabilities are
#' independent!
#' 
#' @param ... Probability vectors.
#'   
#' @return A probability vector.
#' @export
#' 
#' @examples
#' 
#' (p1 <- runif(5))
#' (p2 <- runif(5))
#' combine_probs(p1, p2)
#' 
combine_probs <- function(...) {
  combine_probs_(list(...))
}

combine_probs_ <- function(x) {
  1 - Reduce("*", lapply(x, function(x) {1 - x}))
}
