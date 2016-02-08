## ---- echo=FALSE, include=FALSE------------------------------------------
library(heemod)

## ------------------------------------------------------------------------
death_prob <- data.frame(
  age = rep(seq(35, 85, 10), each = 2),
  sex = rep(0:1, 6),
  mr = c(
    1.51e-3, .99e-3, 3.93e-3,
    2.6e-3, 10.9e-3, 6.7e-3,
    31.6e-3, 19.3e-3, 80.1e-3,
    53.5e-3, 187.9e-3, 154.8e-3
  )
)
death_prob

## ------------------------------------------------------------------------
# a function to return age-related mortality rate
# given age and sex
mr_func <- function(age, sex) {
  age  <- floor(age/10-.5)*10+5
  age <- ifelse(age > 85, 85, age)
  merge(data.frame(age = age, sex = sex), death_prob)$mr
}

param_standard <- define_parameters(
    age_init = 60,
    sex = 0,
    # age increases with cycles
    age = age_init + markov_cycle,
    
    # operative mortality rates
    omrPTHR = .02,
    omrRTHR = .02,
    
    # re-revision mortality rate
    rrr = .04,
    
    # parameters for calculating primary revision rate
    cons = -5.49094,
    ageC = -.0367,
    maleC = .768536,
    lambda = exp(cons + ageC * age_init + maleC * sex),
    gamma = 1.45367786,
    
    rrNP1 = .260677,
    
    # revision probability of primary procedure
    standardRR = 1 - exp(lambda * ((markov_cycle - 1) ^ gamma -
                                     markov_cycle ^ gamma)),
    
    # age-related mortality rate
    mr = mr_func(age, sex)
)
param_standard

## ------------------------------------------------------------------------
param_np1 <- modify(
  param_standard,
  standardRR = 1 - exp(
    lambda *
      rrNP1 *
      (
        (markov_cycle - 1) ^ gamma - 
          markov_cycle ^ gamma)
  )
)
param_np1

## ------------------------------------------------------------------------
mat_trans <- define_matrix(
    state_names = c(
      "PrimaryTHR",
      "SuccessP",
      "RevisionTHR",
      "SuccessR",
      "Death"
    ),
    0, C, 0,          0, omrPTHR,
    0, C, standardRR, 0, mr,
    0, 0, 0,          C, omrRTHR+mr,
    0, 0, rrr,        C, mr,
    0, 0, 0,          0, 1
)
mat_trans

## ---- fig.width = 6, fig.height=6, fig.align='center'--------------------
plot(mat_trans)

## ------------------------------------------------------------------------
mod_standard <- define_model(
  parameters = param_standard,
  transition_matrix = mat_trans,
  PrimaryTHR = define_state(
    utility = 0,
    cost = 0
  ),
  SuccessP = define_state(
    utility = discount(.85, .015),
    cost = 0
  ),
  RevisionTHR = define_state(
    utility = discount(.30, .015),
    cost = discount(5294, .06)
  ),
  SuccessR = define_state(
    utility = discount(.75, .015),
    cost = 0
  ),
  Death = define_state(
    utility = 0,
    cost = 0
  ),
  starting_values = c(cost = 394)
)
mod_standard

mod_np1 <- define_model(
  parameters = param_np1,
  transition_matrix = mat_trans,
  PrimaryTHR = define_state(
    utility = 0,
    cost = 0
  ),
  SuccessP = define_state(
    utility = discount(.85, .015),
    cost = 0
  ),
  RevisionTHR = define_state(
    utility = discount(.30, .015),
    cost = discount(5294, .06)
  ),
  SuccessR = define_state(
    utility = discount(.75, .015),
    cost = 0
  ),
  Death = define_state(
    utility = 0,
    cost = 0
  ),
  starting_values = c(cost = 579)
)

## ------------------------------------------------------------------------
res_mod <- run_models(
  standard = mod_standard,
  np1 = mod_np1,
  cycles = 60,
  cost = cost,
  effect = utility
)
res_mod

## ------------------------------------------------------------------------
summary(res_mod)

## ---- fig.width = 6, fig.align='center'----------------------------------
plot(res_mod, model = "standard", type = "counts")

## ---- fig.width = 6, fig.align='center'----------------------------------
plot(res_mod, model = "np1", type = "counts")

## ---- fig.width = 4, fig.height=4, fig.align='center'--------------------
plot(res_mod, type = "ce")

