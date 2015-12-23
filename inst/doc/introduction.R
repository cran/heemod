## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
mat_trans <- define_matrix(
  .9, .1,
  .2, .8
)
mat_trans

## ------------------------------------------------------------------------
state_A <- define_state(
  cost = 1234,
  utility = 0.85
)
state_A

state_B <- define_state(
  cost = 4321,
  utility = 0.50
)
state_B

## ------------------------------------------------------------------------
state_list <- define_state_list(
  state_A,
  state_B
)
state_list

## ------------------------------------------------------------------------
mod_1 <- define_model(
  transition_matrix = mat_trans,
  states = state_list
)
mod_1

## ------------------------------------------------------------------------
res_mod_1 <- run_model(
  mod_1,
  cycles = 5
)
res_mod_1

## ------------------------------------------------------------------------
summary(res_mod_1)

