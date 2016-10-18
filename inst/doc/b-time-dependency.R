## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
define_parameters(
  mr = exp(- state_cycle * lambda),
  age = 50 + markov_cycle
)

define_state(
  cost = 100 - state_cycle,
  effect = 10
)

f <- function(x) abs(sin(x))

define_transition(
  C,  f(state_cycle),
  .1, .9
)

