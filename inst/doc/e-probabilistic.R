## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
param <- define_parameters(
  rr = .509,
  
  p_AA_mono = .721,
  p_AB_mono = .202,
  p_AC_mono = .067,
  p_AD_mono = .010,
  
  p_BC_mono = .407,
  p_BD_mono = .012,
  
  p_CD_mono = .250,
  
  
  p_AB_comb = p_AB_mono * rr,
  p_AC_comb = p_AC_mono * rr,
  p_AD_comb = p_AD_mono * rr,
  
  p_BC_comb = p_BC_mono * rr,
  p_BD_comb = p_BD_mono * rr,
  
  p_CD_comb = p_CD_mono * rr,
  
  p_AA_comb = 1 - (p_AB_comb + p_AC_comb + p_AD_comb),
  
  
  cost_zido = 2278,
  cost_lami = 2086,
  
  cost_A = 2756,
  cost_B = 3052,
  cost_C = 9007
)

## ------------------------------------------------------------------------
mat_trans_mono <- define_transition(
  p_AA_mono, p_AB_mono, p_AC_mono, p_AD_mono,
  0,         C,         p_BC_mono, p_BD_mono,
  0,         0,         C,         p_CD_mono,
  0,         0,         0,         1
)
mat_trans_comb <- define_transition(
  p_AA_comb, p_AB_comb, p_AC_comb, p_AD_comb,
  0,         C,         p_BC_comb, p_BD_comb,
  0,         0,         C,         p_CD_comb,
  0,         0,         0,         1
)

## ------------------------------------------------------------------------
A_mono <- define_state(
  cost_health = cost_A,
  cost_drugs = cost_zido,
  cost_total = discount(cost_health + cost_drugs, .06),
  life_year = 1
)
B_mono <- define_state(
  cost_health = cost_B,
  cost_drugs = cost_zido,
  cost_total = discount(cost_health + cost_drugs, .06),
  life_year = 1
)
C_mono <- define_state(
  cost_health = cost_C,
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

A_comb <- define_state(
  cost_health = cost_A,
  cost_drugs = cost_zido + cost_lami,
  cost_total = discount(cost_health + cost_drugs, .06),
  life_year = 1
)
B_comb <- define_state(
  cost_health = cost_B,
  cost_drugs = cost_zido + cost_lami,
  cost_total = discount(cost_health + cost_drugs, .06),
  life_year = 1
)
C_comb <- define_state(
  cost_health = cost_C,
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
strat_mono <- define_strategy(
  transition = mat_trans_mono,
  A_mono,
  B_mono,
  C_mono,
  D_mono
)

strat_comb <- define_strategy(
  transition = mat_trans_comb,
  A_comb,
  B_comb,
  C_comb,
  D_comb
)

res_mod <- run_model(
  mono = strat_mono,
  comb = strat_comb,
  parameters = param,
  cycles = 50,
  cost = cost_total,
  effect = life_year
)

## ------------------------------------------------------------------------
rsp <- define_psa(
  rr ~ lognormal(mean = .509, sdlog = .173),
  
  cost_A ~ make_gamma(mean = 2756, sd = sqrt(2756)),
  cost_B ~ make_gamma(mean = 3052, sd = sqrt(3052)),
  cost_C ~ make_gamma(mean = 9007, sd = sqrt(9007)),
  
  p_CD_base ~ prop(prob = .25, size = 40),
  
  p_AA_base + p_AB_base + p_AC_base + p_AD_base ~ multinom(721, 202, 67, 10)
)

## ------------------------------------------------------------------------
pm <- run_psa(
  model = res_mod,
  resample = rsp,
  N = 100
)

## ------------------------------------------------------------------------
summary(pm, 
        threshold = c(1000, 5000, 6000, 1e4))

## ---- fig.width = 6, fig.height=4, fig.align='center'--------------------
plot(pm, type = "ce")

## ---- fig.width = 6, fig.align='center'----------------------------------
plot(pm, type = "ac", max_wtp = 10000, log_scale = FALSE)

## ---- fig.width = 6, fig.height = 4, fig.align='center'------------------
plot(pm, type = "cov")

## ---- fig.align='center', fig.height=4, fig.width=6, message=FALSE-------
library(ggplot2)

plot(pm, type = "ce") +
  xlab("Life-years gained") +
  ylab("Additional cost") +
  scale_color_brewer(
    name = "Strategy",
    palette = "Set1"
  ) +
  theme_minimal()

