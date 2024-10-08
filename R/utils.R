#' Get count number
#' 
#' @param x transition matrix
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @return A count matrix
#'   
#' @keywords internal
get_mat_total <- function(x, init) {
  mod1 <- x * init
  diag(mod1) <- diag(mod1) - init
  return(mod1)
}


#' Get count matrix and difference between two cycles
#' 
#' @param x transition matrix
#' @param init numeric vector, same length as number of 
#'   model states. Number of individuals in each model state
#'   at the beginning.
#' @param inflow numeric vector, similar to `init`.
#'   Number of new individuals in each state per cycle.
#'   
#' @return A length 2 list of matrix : the count matrix for each cycle and the diff matrix 
#'   showing the difference of counts between two cycles.
#'   
#' @keywords internal
get_counts_diff <- function(x, init, inflow) {
  inflow <- as.matrix(inflow)
  lapply(seq(1, length(x) + 1), function(i){
    if (i == length(x) + 1) return(list(init, NULL))
    init <- init + unlist(inflow[i, ], use.names = FALSE)
    mat <- get_mat_total(x[[i]], init)
    res <- list(init, mat)
    init <<- colSums(mat) + init
    return(res)
  })
}

#' Check whole Numbers
#' 
#' @param x numeric.
#' @param tol the smallest positive floating-point number x 
#'   such that 1 + x != 1.
#'   
#' @return A logical scalar.
#'   
#' @keywords internal
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

#' Discount a Quantity Over Time. Should be a scalar if time is 
#' specified, a vector otherwise
#' 
#' @param x numeric. A quantity to discount.
#' @param r discount rate.
#' @param first logical. Should discounting start at the
#'   first value?
#' @param period Number of cycle per unit of discount rate.
#' @param linear logical. Should the discount rate vary linearly along the 
#' whole period?
#' @param time The cycle number.
#'
#' @details If the unit of discount rate is the year and a cycle duration is 1 
#' month, period should be 12.
#' 
#' @return A numeric vector of the same length as `x`.
#' @export
#' 
#' @examples
#' 
#' discount(rep(10, 5), .02)
#' discount(rep(10, 5), .02, first = FALSE)
#'  
#' discount(1000, .05, time = 10)
#' discount(1000, .05, period = 2, time = 1:10)
#' discount(1000, .05, period = 2, time = 1:10, linear = TRUE)
#' 
#' @keywords internal
discount <- function(x, r, first = FALSE, period = 1, linear = FALSE, time) {
  if (length(r) > 1) r <- r[1]
  stopifnot(
    r >= 0,
    r <= 1,
    period > 0
  )
  fun <- if(linear) function(x) x else trunc
  dr <- if (missing(time)){
    fun((seq_along(x) - (1 - isTRUE(first))) / period)
  } else {
    fun((time - as.numeric(!isTRUE(first)))/period)
  }
  x / (1 + r) ^ dr
}

#' Check if All the Elements of a List Are the Same
#' 
#' @param x a list.
#'   
#' @return A logical scalar.
#'   
#' @keywords internal
list_all_same <- function(x) {
  length(x) == 0 |
    all(unlist(
      Map(function(y) identical(y, x[[1]]), x)
    ))
}

#' Returns "s" if x > 1
#'
#' @param x integer.
#'
#' @return `"s"` or `""`.
#'   
#' @keywords internal
plur <- function(x) {
  if (x > 1) "s" else ""
}
#' @rdname plur
plur_y <- function(x) {
  if (x > 1) "ies" else "y"
}

#' Check Names
#' 
#' Throws an error if any of the names are reserved.
#' 
#' Reserved names are `model_time` and anything
#' starting with `.`.
#' 
#' @param x A character vector of names.
#'   
#' @return Nothing, just throws an error if a reserved name
#'   is encountered.
#'   
#' @keywords internal
check_names <- function(x) {
  if (is.null(x)) {
    stop("Names must exist.")
  }
  if (anyNA(x)) {
    stop("Missing names are not allowed.")
  }
  if (any("" %in% x)) {
    stop("Empty string names are not allowed.")
  }
  if (any("model_time" %in% x)) {
    stop("'model_time' is a reserved name.")
  }
  if (any("markov_model" %in% x)) {
    stop("'markov_model' is a reserved name.")
  }
  if (any("state_cycle" %in% x)) {
    stop("'state_cycle' is a reserved name.")
  }
  if (any("state_time" %in% x)) {
    stop("'state_time' is a reserved name.")
  }
  if (any("C" %in% x)) {
    stop("'C' is a reserved name.")
  }
  if (any("strategy" %in% x)) {
    stop("'strategy' is a reserved name.")
  }
  if (any(grepl("^\\.", x))) {
    stop("Names starting with '.' are reserved.")
  }
}

#' Make Syntactically Valid Names
#' 
#' Compared to [make.names()] this function also 
#' converts characters to lower case and replaces `.`
#' by `_`.
#' 
#' @param x A character vector.
#'   
#' @return A character vector.
#'   
#' @keywords internal
make_names <- function(x) {
  gsub("\\.+", "_", make.names(tolower(x)))
}

#' Check Strategy Index
#' 
#' @param x A result from [run_model()].
#' @param i A strategy index, character or numeric.
#' @param allow_multiple logical. Allow multiple strategy
#'   index?
#'   
#' @return Strategy names.
#'   
#' @keywords internal
check_strategy_index <- function(x, i, allow_multiple = FALSE) {
  
  if(length(i) != 1 & ! allow_multiple) {
    stop("Strategy index must have length 1.")
  }
  
  if (! (is.character(i) | is.numeric(i))) {
    stop("Strategy index must be either numeric or character.")
  }
  
  if (is.numeric(i) & (any(i > get_strategy_count(x)) | any(i < 1))) {
    stop(sprintf("Strategy index out of range [%i - %i].",
                 1, get_strategy_count(x)))
  }
  
  if (is.character(i) & any(! i %in% get_strategy_names(x))) {
    stop(sprintf(
      "Strategy index is not the name of a strategy (%s).",
      paste(get_strategy_names(x), collapse = " - ")
    ))
  }
  
  res <- get_strategy_names(x)
  names(res) <- res
  
  res[i]
}

#' Weighted Summary
#' 
#' Compute a weighted summary of a numeric vector.
#' 
#' If `weights` is `NULL` an unweighted summary is
#' returned.
#' 
#' @param x A numeric vector.
#' @param weights A vector of weights, same length as 
#'   `x`.
#'   
#' @return A vector with values \code{Min., 1st Qu., Median,
#'   Mean, 3rd Qu., Max.}.
#'   
#' @keywords internal
wtd_summary <- function(x, weights = NULL) {
  if (is.null(weights)) {
    res <- summary(x)
    
  } else if (all(is.na(x))) {
    res <- rep(NA, 6)
    
  } else {
    w_mean <- wtd_mean(x, weights = weights)
    w_q <- wtd_quantile(x, weights = weights,
                        probs = c(0, .25, .5, .75, 1))
    res <- c(w_q[1], w_q[2], w_q[3], w_mean, w_q[4], w_q[5])
  }
  
  setNames(res, c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max."))
}

wtd_quantile <- function(x, weights = rep(1L, length(x)),
                         probs = seq(0, 1, .25)) {
  i <- order(x)
  quant <- cumsum(weights[i]) - weights[i] / 2
  quant <- (quant - quant[1]) / (quant[length(quant)] - quant[1])
  
  stats::approx(x = quant, y = x[i], xout = probs,
                method = "linear")$y
}

wtd_mean <- function(x, weights = rep(1L, length(x))) {
  sum(x * weights) / sum(weights)
}

#' Safely Convert From Characters to Numbers
#' 
#' These function return an error if a conversion fails.
#' 
#' @name safe_conversion
#' @param x A character vector.
#' @param f A conversion function.
#'   
#' @return A converted vector.
#'   
#' @keywords internal
safe_convert <- function(x, f) {
  na1 <- is.na(x)
  res <- suppressWarnings(f(x))
  na2 <- is.na(res)
  
  if (any(pb <- na1 != na2)) {
    stop(sprintf(
      "Failed to convert values: %s.",
      paste(x[pb], collapse = ", ")
    ))
  }
  
  res
}

#' @rdname safe_conversion
as_numeric_safe <- function(x) {
  safe_convert(x, as.numeric)
}

#' @rdname safe_conversion
as_integer_safe <- function(x) {
  res_int <- safe_convert(x, as.integer)
  res_num <- safe_convert(x, as.numeric)
  
  if (! isTRUE(all.equal(res_int, res_num))) {
    stop(sprintf(
      "Floating point values coerced to integer: %s.",
      paste(
        res_num[abs(res_int - res_num) > sqrt(.Machine$double.eps)],
        collapse = ", "
      )
    ))
  }
  res_int
}

#' Convert Data Frame Factor Variables to Character
#' 
#' @param x A data frame.
#'   
#' @return A data frame.
#'   
#' @keywords internal
clean_factors <- function(x) {
  if (any(unlist(lapply(x, is.factor)))){
    for (i in seq_along(x)) {
      if (is.factor(x[[i]])) {
        x[[i]] <- as.character(x[[i]])
      }
    }
  }
  x
}

to_text_dots <- function(x, name = TRUE) {
  n <- names(x)
  ex <- if (is.atomic(x)) {
    format(x)
  } else {
    unlist(lapply(
      x,
      function(y) if (!rlang::is_call(y) && any(is.na(y))) NA else
      as_label(y)
    ))
  }
  
  if (name) {
    stopifnot(
      length(n) == length(ex)
    )
    paste(n, ex, sep = " = ")
  } else {
    ex
  }
}

interleave <- function(...) {
  .dots <- list(...)
  id <- unlist(lapply(.dots, seq_along))
  c(...)[order(id)]
}

#' Insert Elements in Vector
#' 
#' Insert a vector in another vector.
#' 
#' To insert an element at the beginning use a `pos` 
#' value of 0.
#' 
#' Duplicated positions are not allowed.
#' 
#' @param x A vector (or a list).
#' @param pos Integer. Insert after which elements?
#' @param what Vector of elements to insert.
#'   
#' @return A vector.
#'   
#' @examples
#' 
#' heemod:::insert(letters, c(0, 5, 26), c("xxx", "yyy"))
#' 
#' @keywords internal
insert <- function(x, pos, what) {
  
  stopifnot(
    all(pos >= 0),
    all(pos <= length(x)),
    ! any(duplicated(pos))
  )
  
  res <- c(x, rep(what, length(pos)))
  
  id  <- c(
    seq_along(x),
    rep(pos, each = length(what)) +
      seq(0, .9, length.out = length(what))
  )
  res[order(id)]
}

get_tm_pos <- function(row, col, n) {
  (row - 1) * n + col
}

pretty_names <- function(x) {
  if (is_matrix <- inherits(x, "matrix")) {
    n <- colnames(x)
  } else {
    n <- names(x)
  }
  
  names(n) <- n
  
  ref <- tibble::tibble(
    from = c(".cost", ".effect",
             ".dcost", ".deffect",
             ".icer", ".dref",
             ".model_names"),
    to = c("Cost", "Effect",
           "Cost Diff.", "Effect Diff.",
           "ICER", "Ref.",
           "Strategy")
  ) %>% 
    dplyr::filter(.data$from %in% n)
  
  n[ref$from] <- ref$to
  
  if (is_matrix) {
    colnames(x) <- n
  } else (
    names(x) <- n
  )
  
  x
}

to_dots <- function(x) {
  UseMethod("to_dots")
}

#' @export
to_dots.default <- function(x) {
  as_quosures(lapply(
    x, function(x) x
  ))
}

#' @export
to_dots.list <- function(x) {
  f <- function(x) {
    if (inherits(x, "character") || inherits(x, "factor")) {
      as_quosure(as.character(x), env = globalenv())
    } else {
      x
    }
  }
  
  as_quosures(
    lapply(x, f)
  )
}

# transforms factors to characters in a df
clean_factors <- function(x) {
  for (n in names(x)) {
    if (inherits(x[[n]], "factor")) {
      x[[n]] <- as.character(x[[n]])
    }
  }
  x
}

# formula operations

is_one_sided <- function(x) {
  length(x) == 2
}

lhs <- function(x) {
  if (is_one_sided(x)) {
    stop("Cannont extract left hand side of a one-sided formula.")
  } else {
    x[[2]]
  }
}

rhs <- function(x) {
  if (is_one_sided(x)) {
    x[[2]]
  } else {
    x[[3]]
  }
}

make_call <- function(x, collapse) {
  if (length(x) > 1) {
    as.call(
      list(
        as.name(collapse),
        as.name(x[1]),
        make_call(x[-1], collapse = collapse)
      )
    )
  } else {
    as.name(x)
  }
}

reshape_long <- function(data, key_col, value_col,
                         gather_cols, na.rm = FALSE) {
  idvar <- names(data)[! names(data) %in% gather_cols]
  
  ids <- return_ids(data, idvar)
  
  stopifnot(
    all(! duplicated(ids))
  )
  
  d <- data
  d <- d[, ! (names(data) %in% gather_cols), drop = FALSE]
  res <- do.call(
    rbind,
    lapply(gather_cols,
           function(col) {
    d[, key_col] <- col
    d[, value_col] <- data[, col]
    d
  }))
  
  if (na.rm) {
    res <- res[! is.na(res[[value_col]]), ]
  }
  
  return(res)
}

return_ids <- function(data, idvar) {
  if (length(idvar)) {
    tab_id <- data[idvar]
    atomic_id <- unlist(lapply(tab_id, is.atomic))
    for (id in idvar[! atomic_id]) {
      tab_id[id] <- seq_len(nrow(data))
    }
    if (length(idvar) > 1L) {
      ids <- interaction(tab_id[, idvar], drop = TRUE)
    } else {
      ids <- tab_id[[idvar]]
    }
  } else {
    ids <- seq_len(nrow(data))
  }
  ids
}

reshape_wide <- function(data, key_col, value_col, fill = NA) {
  idvar <- names(data)[! names(data) %in% c(key_col, value_col)]
  
  ids <- return_ids(data, idvar)
  
  unique_ids <- ids[! duplicated(ids)]
  
  stopifnot(
    all(! is.na(data[[key_col]]))
  )
  
  res <- data[! duplicated(ids), idvar, drop = FALSE]
  
  cbind(
    res,
    do.call(
      cbind,
      stats::setNames(
        object = lapply(
          unique(data[[key_col]]),
          function(x) {
            ret <- vector(
              mode = class(data[[value_col]]),
              length = nrow(res))
            ret <- fill
            index_key <- data[[key_col]] == x
            ret[unique_ids %in% ids[index_key]] <-
              data[index_key, ][[value_col]]
            ret
          }
        ),
        nm = unique(data[[key_col]])
      )
    )
  )
}

matrix_expand_grid <- function(...){
    nargs <- length(args <- list(...))
    iArgs <- seq_len(nargs)
    rep.fac <- 1L
    d <- lengths(args)
    orep <- prod(d)
    cargs <- matrix(ncol = nargs, nrow = orep)
    for (i in iArgs) {
      x <- args[[i]]
      nx <- length(x)
      orep <- orep/nx
      x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, 
                                                  nx)), orep)]
      cargs[, i] <- x
      rep.fac <- rep.fac * nx
    }
    cargs
}


interp <-  function (x, ..., .values) {
  .dots <- rlang::exprs(...)
  values <- if (length(.dots)){
    all_values(.values, .dots)
  } else {
    .values
  }
  expr <- substitute_(get_expr(x), values)
  x <- set_expr(x, expr)
  x
}

all_values <- function (.values, .dots) 
{
  if (missing(.values)) {
    values <- lapply(.dots,function(x) eval_tidy(x, env = rlang::caller_env(4)))
  }  else {
    values <- c(.values, lapply(.dots, function(x) eval_tidy(x,  env = rlang::caller_env(4))))
  }
  if (is.list(values)) {
    find_quosure <- vapply(values, is_quosure, logical(1))
    values[find_quosure] <- lapply(values[find_quosure], get_expr)
  }
  values
}


substitute_ <- function (x, env) 
{
  if (identical(env, globalenv())) {
    env <- as.list(env)
  } else {
    env <- as.environment(env)
  }
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

deprecated_x_cycle <- function(.dots){
  if(any(grepl("markov_cycle", deparse(.dots)))){
    lifecycle::deprecate_warn("0.16.0", I("markov_cycle"), I("model_time"), user_env = caller_env(3))
  }
  if(any(grepl("state_dcyle", deparse(.dots)))){
    lifecycle::deprecate_warn("0.16.0", I("state_cycle"), I("state_time"), user_env = caller_env(3))
  }
}

detect_dplyr_pipe <- function(x){
  predicate <- function(y) identical(get_expr(y), quote(.))
  if (is.list(x)) {
    purrr::modify_if(x, predicate, alert_pipe)
  } else {
    if (predicate(x)) alert_pipe() else x
  }
}

alert_pipe <- function(...){
  cli::cli_abort(c(x = "dplyr's pipe %>% is not supported for chaining survival operations. ",
       "Please use the new pipe |> instead."), call=NULL)
}

copy_surv_env <- function(){
  new_env <- getOption("heemod.env")
  n <- 0
  repeat({
    n <- n + 1
    env <- rlang::caller_env(n)
    purrr::walk(ls(env, all.names = TRUE), function(y){
      if (!inherits(try(env[[y]], silent = TRUE), "try-error") && 
          inherits(env[[y]], "surv_object")) {
        assign(y, get(y, env), new_env)
      }
    })
    if(identical(env, globalenv())) return(new_env)
  })
}

copy_param_env <- function(param, ...){
  UseMethod("copy_param_env")
}

# copy_param_env.name <- function(param){
#   copy_param_env.default(param)
# }

#' @export
copy_param_env.name <- function(param, ...){
  copy_param_env.default(all.vars(param), ...)
}

#' @export
copy_param_env.call <- function(param, ...){
  copy_param_env.default(all.vars(param), ...)
}

#' @export
copy_param_env.uneval_parameters <- function(param, ...){
  list_vars <- map(param, all.vars) 
  to_find <- unlist(list_vars, use.names = F)
  not_found <- setdiff(to_find, 
                       c(names(list_vars), 
                         "model_time", 
                         "state_time"
                         ))
  copy_param_env.default(not_found, ...)
}

#' @export
copy_param_env.default <- function(param, ...){
  .dots <- list(...)
  overwrite <- ifelse(is.null(.dots$overwrite), TRUE, .dots$overwrite)
  env <- .dots$env %||% getOption("heemod.env")
  
  if(!overwrite) {
    param <- setdiff(param, ls(env))
  }
  new_env <- env

  for (x in param){
    n <- 0
  repeat({
    n <- n + 1
    env <- rlang::caller_env(n)
    if (exists(x, envir = env, inherits = FALSE)) {
      tmp <- get(x, env)
      # if (inherits(tmp, "surv_object")){
      #   unlist(tmp) %>% 
      #     Filter(function(x) !is.numeric(x), .) %>% 
      #     copy_param_env()
      # }
      assign(x, tmp, new_env)
      break
    }
    if (identical(env, globalenv())) break
  }) 
  }
}

#' @export
c.uneval_parameters <- function(...){
  .dots <- list(...)
  stopifnot("all elements must be specified with define_parameters" = all(lapply(.dots, function(x){
    inherits(x, "uneval_parameters")
  }) %>% as.logical()))
  structure(.Data = vctrs::vec_c(...), class = class(.dots[[1]]))
}

setdiff <- function (x, y) {
  u <- as.vector(x)
  v <- as.vector(y)
  u[!duplicated(unclass(u)) & (match(u, v, 0L) == 0L)]
}