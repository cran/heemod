## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)
library(ggplot2)

## ------------------------------------------------------------------------
mat_mono <-
  define_transition(
    .721, .202, .067, .010,
    0,    .581, .407, .012,
    0,    0,    .750, .250,
    0,    0,    0,    1
  )
mat_mono

## ------------------------------------------------------------------------
rr <- .509

mat_comb <-
  define_transition(
    C, .202*rr, .067*rr, .010*rr,
    0, C,       .407*rr, .012*rr,
    0, 0,       C,       .250*rr,
    0, 0,       0,       1
  )
mat_comb

## ---- fig.width = 6, fig.height=6, fig.align='center'--------------------
plot(mat_mono)

## ---- fig.width = 6, fig.height=6, fig.align='center'--------------------
plot(mat_comb)

## ------------------------------------------------------------------------
cost_zido <- 2278
cost_lami <- 2086

## ------------------------------------------------------------------------
A_mono <- define_state(
    cost_health = 2756,
    cost_drugs = cost_zido,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
A_mono

## ------------------------------------------------------------------------
B_mono <- define_state(
    cost_health = 3052,
    cost_drugs = cost_zido,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
C_mono <- define_state(
    cost_health = 9007,
    cost_drugs = cost_zido,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
D_mono <- define_state(
    cost_health = 0,
    cost_drugs = 0,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 0
  )

## ------------------------------------------------------------------------
A_comb <- define_state(
    cost_health = 2756,
    cost_drugs = cost_zido + cost_lami,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
B_comb <- define_state(
    cost_health = 3052,
    cost_drugs = cost_zido + cost_lami,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
C_comb <- define_state(
    cost_health = 9007,
    cost_drugs = cost_zido + cost_lami,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 1
  )
D_comb <- define_state(
    cost_health = 0,
    cost_drugs = 0,
    cost_total = discount(cost_health + cost_drugs, .06),
    life_year = 0
  )

## ------------------------------------------------------------------------
mod_mono <- define_strategy(
  transition_matrix = mat_mono,
  A_mono,
  B_mono,
  C_mono,
  D_mono
)
mod_mono

## ------------------------------------------------------------------------
mod_comb <- define_strategy(
  transition_matrix = mat_comb,
  A_comb,
  B_comb,
  C_comb,
  D_comb
)

## ------------------------------------------------------------------------
res_mod <- run_model(
  mono = mod_mono,
  comb = mod_comb,
  cycles = 50,
  cost = cost_total,
  effect = life_year
)

## ------------------------------------------------------------------------
summary(res_mod)

## ---- fig.align='center', fig.width=6, message=FALSE---------------------
plot(res_mod, type = "counts", panel = "by_model") +
  xlab("Time") +
  theme_minimal() +
  scale_color_brewer(
    name = "State",
    palette = "Set1"
  )

## ---- fig.align='center', fig.width=6, message=FALSE---------------------
plot(res_mod, type = "counts", panel = "by_state") +
  xlab("Time") +
  theme_minimal() +
  scale_color_brewer(
    name = "Model",
    palette = "Set1"
  )

