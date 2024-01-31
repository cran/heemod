## ----setup, include=FALSE-----------------------------------------------------
library(heemod)
library(flexsurv)

## -----------------------------------------------------------------------------
surv_dist <- define_surv_dist("gamma", shape = 2, rate = 0.1)
psa <- define_psa(surv_dist ~ resample_surv(n = 500))

## -----------------------------------------------------------------------------
psa2 <- define_psa(surv_dist ~ resample_surv(n = 50))
plot(surv_dist, psa = psa)
plot(surv_dist, psa = psa2)

## -----------------------------------------------------------------------------
fit_cov <- flexsurv::flexsurvreg(survival::Surv(rectime, censrec) ~ group,
                            data = bc,
                            dist = "exp")|>
  define_surv_fit()

psa <-  define_psa(fit_cov ~ resample_surv())

plot(fit_cov, times = 1:1000, psa = psa, Nrep = 10)

## -----------------------------------------------------------------------------
fitcov_poor   <- set_covariates(fit_cov, group = "Poor")
fitcov_medium <- set_covariates(fit_cov, group = "Medium")
fit_w <- flexsurvreg(
  formula = Surv(futime, fustat) ~ 1,
  data = ovarian, dist = "weibull"
) |> 
  define_surv_fit()

fit_cov |> 
  set_covariates(group = "Good") |> 
  apply_hr(hr = 2) |> 
  mix(
    fitcov_medium,
    weights = c(0.25, 0.75)
  ) |>
  add_hazards(
    fit_w
  ) |>
  join(
    fitcov_poor,
    at = 500
  ) |>
plot(psa = psa, 1:1000, Nrep = 10)

