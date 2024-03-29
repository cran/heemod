#' Project Beyond a Survival Distribution with Another
#' 
#' Project survival from a survival distribution using one
#' or more survival distributions using the specified cut points.
#' 
#' @param ... Survival distributions to be used in the
#'   projection.
#' @param dots Used to work around non-standard evaluation.
#' @param at A vector of times corresponding to the cut
#'   point(s) to be used.
#'   
#' @return A `surv_projection` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_surv_dist(distribution = "exp", rate = .5)
#' dist2 <- define_surv_dist(distribution = "gompertz", rate = .5, shape = 1)
#' join_dist <- join(dist1, dist2, at=20)
join <- function(..., at) {
  dots <- exprs(...) %>% 
    detect_dplyr_pipe()
  join_(dots, at)
}

#' @export
#' @rdname join
join_ <- function(dots, at) {
  stopifnot(
    all(at > 0),
    all(is.finite(at)),
    !is.unsorted(at, strictly=T),
    length(at) == length(dots) - 1
  )
  
  # Restructure so that first distribution is "alone"
  # and subsequent distributions are put in a list along
  # with their cut point.
  dist_list <- list()
  for (i in seq_along(dots)) {
    if (i==1) {
      dist_list[[i]] <- dots[[i]]
    } else {
      dist_list[[i]] <- list(
        dist = dots[[i]],
        at = at[i-1]
      )
    }
  }
  
  # Use recursion to deal with distributions in pairs
  Reduce(project_fn, dist_list)
}

#' Project Beyond a Survival Distribution with Another
#' (pairwise)
#' 
#' Project survival from a survival distribution using
#' another survival distribution at the specified cutpoint. 
#' Used by project to reduce the list of distributions.
#' 
#' @param dist1 Survival distribution to project from.
#' @param dist2_list A list containing distribution to
#'   project with and the time at which projection begins.
#' @return A `surv_projection` object.
#' @keywords internal
project_fn <- function(dist1, dist2_list) {
  structure(
    list(
      dist1 = dist1,
      dist2 = dist2_list$dist,
      at = dist2_list$at
    ),
    class = c("surv_projection", "surv_object")
  )
}

#' Mix Two or More Survival Distributions
#' 
#' Mix a set of survival distributions using the specified
#' weights.
#' 
#' @param ... Survival distributions to be used in the
#'   projection.
#' @param dots Used to work around non-standard evaluation.
#' @param weights A vector of weights used in pooling.
#'   
#' @return A `surv_pooled` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_surv_dist(distribution = "exp", rate = .5)
#' dist2 <- define_surv_dist(distribution = "gompertz", rate = .5, shape = 1)
#' pooled_dist <- mix(dist1, dist2, weights = c(0.25, 0.75))
#' 
mix <- function(..., weights = 1) {
  dots <- exprs(...)%>% 
    detect_dplyr_pipe()
  
  mix_(dots, weights)
}

#' @export
#' @rdname mix
mix_ <- function(dots, weights = 1) {
  
  stopifnot(
    all(weights > 0),
    all(is.finite(weights)),
    length(weights) == length(dots)
  )
  
  structure(
    list(
      dists = dots,
      weights =  weights
    ),
    class = c("surv_pooled", "surv_object")
  )
}

#' Apply a Hazard Ratio
#' 
#' Proportional reduce or increase the hazard rate of a
#' distribution.
#' 
#' @param dist A survival distribution.
#' @param hr A hazard ratio to be applied.
#' @param log_hr If `TRUE`, the hazard ratio is exponentiated
#'   before being applied.
#'   
#' @return A `surv_ph` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_surv_dist(distribution = "exp", rate = .25)
#' ph_dist <- apply_hr(dist1, 0.5)
#' 
apply_hr <- function(dist, hr, log_hr = FALSE) {
  dist <- enexpr(dist) %>% 
    detect_dplyr_pipe()
  #stopifnot(
   # length(hr) == 1#,
    # is.finite(hr),
    # log_hr | hr > 0
  #)
  hr <- enexpr(hr)
  if(log_hr) hr <- rlang::call2(exp, hr)
  #if(hr == 1) return(dist)
  # if(inherits(eval_tidy(dist), "surv_ph")){
  #   dist <- eval_tidy(dist)
  #   dist$hr <- dist$hr * hr
  #   if(dist$hr == 1) return(dist$dist)
  #   return(dist)
  # }
  structure(
    list(
      dist = dist,
      hr = hr
    ),
    class = c("surv_ph", "surv_object")
  )
}

#' Apply an Acceleration Factor
#' 
#' Proportionally increase or reduce the time to event of a
#' survival distribution.
#' 
#' @param dist A survival distribution.
#' @param af An acceleration factor to be applied.
#' @param log_af If `TRUE`, the acceleration factor is
#'   exponentiated before being applied.
#'   
#' @return A `surv_aft` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_surv_dist(distribution = "exp", rate = .25)
#' aft_dist <- apply_af(dist1, 1.5)
apply_af <- function(dist, af, log_af = FALSE) {
  dist <- enexpr(dist) %>% 
    detect_dplyr_pipe()
  # stopifnot(
  #   length(af) == 1,
  #   is.finite(af),
  #   log_af | af > 0
  # )
  af <- enexpr(af)
  if(log_af) af <- rlang::call2(exp, af)
  #if(af == 1) return(dist)
  # if(inherits(new_dist, "surv_aft")){
  #   dist <- new_dist
  #   dist$af <- dist$af * af
  #   if(dist$af == 1) return(dist$dist)
  #   return(dist)
  # }
  
  structure(
    list(
      dist = dist,
      af = af
    ),
    class = c("surv_aft", "surv_object")
  )
}

#' Apply an Odds Ratio
#' 
#' Proportionally increase or reduce the odds of an event of
#' a survival distribution.
#' 
#' @param dist A survival distribution.
#' @param or An odds ratio to be applied.
#' @param log_or If `TRUE`, the odds ratio is exponentiated
#'   before being applied.
#'   
#' @return A `surv_po` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_surv_dist(distribution = "exp", rate = .25)
#' po_dist <- apply_or(dist1, 1.2)
apply_or = function(dist, or, log_or = FALSE) {
  dist <- enexpr(dist) %>% 
    detect_dplyr_pipe()
  
  # stopifnot(
  #   length(or) == 1,
  #   is.finite(or),
  #   log_or | or > 0
  # )
  or <- enexpr(or)
  if(log_or) or <- rlang::call2(exp, or)
 # if(or == 1) return(dist)
  # if(inherits(eval_tidy(dist), "surv_po")){
  #   dist <- eval_tidy(dist)
  #   dist$or <- dist$or * or
  #   if(dist$or == 1) return(dist$dist)
  #   return(dist)
  # }
  
  structure(
    list(
      dist = dist,
      or = or
    ),
    class = c("surv_po", "surv_object")
  )
}

#' Apply a time shift to a survival distribution
#' 
#' 
#' @param dist A survival distribution.
#' @param shift A time shift to be applied.
#'   
#' @return A `surv_shift` object.
#' 
#' @details A positive shift moves the fit backwards in time.   That is,
#'   a shift of 4 will cause time 5 to be evaluated as time 1, and so on.
#'   If `shift == 0`, `dist` is returned unchanged.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_surv_dist(distribution = "gamma", rate = 0.25, shape = 3)
#' shift_dist <- apply_shift(dist1, 4)
#' compute_surv(dist1, 1:10)
#' compute_surv(shift_dist, 1:10)
apply_shift = function(dist, shift) {
  dist <- enexpr(dist)%>% 
    detect_dplyr_pipe()
  
  # stopifnot(
  #   length(shift) == 1,
  #   is.finite(shift)
  # )
  
  shift <- enexpr(shift)
  if(shift == 0) return(dist)
  # if(inherits(eval_tidy(dist), "surv_shift")){
  #   dist <- eval_tidy(dist)
  #     dist$shift <- dist$shift + shift
  #     if(dist$shift == 0) return(dist$dist)
  #     else return(dist)
  # }  
  structure(
      list(
        dist = dist,
        shift = shift
      ),
      class = c("surv_shift", "surv_object")
    )
}

#' Add Hazards
#' 
#' Get a survival distribution reflecting the independent
#' hazards from two or more survival distributions.
#' 
#' @param ... Survival distributions to be used in the
#'   projection.
#' @param dots Used to work around non-standard evaluation.
#'   
#' @return A `surv_add_haz` object.
#' @export
#' 
#' @examples
#' 
#' dist1 <- define_surv_dist(distribution = "exp", rate = .125)
#' dist2 <- define_surv_dist(distribution = "weibull", shape = 1.2, scale = 50)
#' combined_dist <- add_hazards(dist1, dist2)
#' 
add_hazards <- function(...) {
  dots <- exprs(...)%>% 
    detect_dplyr_pipe()
  
  add_hazards_(dots)
}

#' @export
#' @rdname add_hazards
add_hazards_ <- function(dots) {
  
  structure(
    list(
      dists = dots
    ),
    class = c("surv_add_haz", "surv_object")
  )
}

#' Set Covariates of a Survival Distribution
#' 
#' Set the covariate levels of a survival model to be 
#' represented in survival projections.
#' 
#' @param dist a survfit or flexsurvreg object
#' @param ... Covariate values representing the group for 
#'   which survival probabilities will be generated when 
#'   evaluated.
#' @param covariates Used to work around non-standard
#'   evaluation.
#' @param data A an optional data frame representing 
#'   multiple sets of covariate values for which survival 
#'   probabilities will be generated. Can be used to 
#'   generate aggregate survival for a heterogeneous set of 
#'   subjects.
#'   
#' @return A `surv_model` object.
#' @export
#' 
#' @examples
#' 
#' fs1 <- flexsurv::flexsurvreg(
#'   survival::Surv(rectime, censrec)~group,
#'   data=flexsurv::bc,
#'   dist = "llogis"
#' )
#' good_model <- set_covariates(fs1, group = "Good")
#' cohort <- data.frame(group=c("Good", "Good", "Medium", "Poor"))
#' mixed_model <- set_covariates(fs1, data = cohort)
#' 
set_covariates <- function(dist, ..., data = NULL) {
  covariates <- data.frame(...)
  dist <- enexpr(dist)%>% 
    detect_dplyr_pipe()
  set_covariates_(dist, covariates, data)
}

#' @export
#' @rdname set_covariates
set_covariates_ <- function(dist, covariates, data = NULL) {
  data <- rbind(
    covariates,
    data
  )
  
  structure(
    list(
      dist = dist,
      covar = data
    ),
    class = c("surv_model", "surv_object")
  )
}


#' Plot general survival models
#'
#' @param x a survival object of class `surv_aft`, `surv_add_haz`,
#'   `surv_ph`, `surv_po`, `surv_model`, `surv_pooled`, or `surv_projection`.
#' @param times Times at which to evaluate and plot the survival object.
#' @param type either `surv` (the default) or `prob`, depending on whether
#'   you want to plot survival from the start or conditional probabilities.
#' @param psa a `define_psa` object
#' @param Nrep The number of replications to estimate the variability of `x`
#' @param join_opts A list of 3 graphical parameters for points at which different
#' survival functions are joined: join_col, join_pch and join_size. 
#' @param ... additional arguments to pass to `compute_surv` functions.
#'   
#' @details The function currently only highlights join points that are at
#'   the top level; that is, for objects with class `surv_projection`.
#'   To avoid plotting the join points, set join_size to a negative number.  
#'   
#'
#' @return a [ggplot2::ggplot()] object.
#' @example inst/examples/example_plot.surv_object.R
#' @export
#'
plot.surv_object <- function(x, times = seq.int(0, 30), type = c("surv", "prob"), 
                             psa, Nrep = 100,
                             join_opts = list(join_col = "red", 
                                              join_pch = 20,
                                              join_size = 3),
                           ...){
  type <- match.arg(type)
  res <- data.frame(times = times,
                     baseline = compute_surv_(x, times, ..., type = type))
  if (!missing(psa)){
    res <- merge(res,
      compute_surv_ci(substitute(x), times, type, psa, Nrep),
      by = "times")
  } else if (inherits(x, "surv_fit") & type == "surv"){
    return(plot(eval_tidy(x)))
  }
  y_ax_label <- c(surv = "survival", prob = "probability")[type]
  
  this_plot <- 
    ggplot2::ggplot(res, ggplot2::aes(x = times, y = .data$baseline)) + 
    ggplot2::geom_line() + 
    ggplot2::scale_x_continuous(name = "time") + 
    ggplot2::scale_y_continuous(name = y_ax_label, limits = c(0,1))
    if (!missing(psa)){
    this_plot <- this_plot + ggplot2::geom_ribbon(alpha=0.25, colour = NA, show.legend = FALSE,
                                         aes(ymin = .data$`2.5%`, ymax = .data$`97.5%`))
    }
  if("at" %in% names(x))
    this_plot <- this_plot +
    ggplot2::geom_point(data = dplyr::filter(res, times == x$at),
                        ggplot2::aes(x = times, y = .data$baseline),
                        pch = join_opts$join_pch, size = join_opts$join_size, 
                        col = join_opts$join_col) 
  
  this_plot
  
}

#' Summarize surv_shift objects
#'
#' @param object a `surv_shift` object 
#' @param summary_type "standard" or "plot" - "standard"
#'   for the usual summary of a `survfit` object,
#'   "plot" for a fuller version
#' @param ... other arguments
#' 
#' @return A summary.
#' @export
#'
summary.surv_shift <- 
  function(object, summary_type = c("plot", "standard"), ...){
    summary_type <- match.arg(summary_type)
    res <- summary(object$dist, ...)
    if(inherits(res, "summary.survfit")){
      if(summary_type == "plot"){
        res <- data.frame(res[c("time", "surv", "upper", "lower")])
        names(res) <- c("time", "est", "lcl", "ucl")
      }
    }
    if(length(res) == 1) res <- res[[1]]
    res$time <- res$time + object$shift
    res
    }
