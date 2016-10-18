## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
mat_trans <- define_transition(
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
strat_1 <- define_strategy(
  transition_matrix = mat_trans,
  state_A,
  state_B
)
strat_1

## ------------------------------------------------------------------------
res_mod_1 <- run_model(
  strat_1,
  cycles = 10,
  cost = cost,
  effect = utility
)
res_mod_1

## ---- fig.width = 6, fig.height=4, fig.align='center'--------------------
plot(res_mod_1)

## ---- fig.align='center', fig.height=4, fig.width=6, message=FALSE-------
library(ggplot2)

plot(res_mod_1) +
  xlab("Time") +
  ylab("N") +
  theme_minimal() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )

## ------------------------------------------------------------------------
get_counts(res_mod_1)
get_values(res_mod_1)

## ------------------------------------------------------------------------
rate_to_prob(r = 162, per = 1000, to = 5)

