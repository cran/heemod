sn <- LETTERS[1:5]
mn <- c("I", "II", "III")

test_that(
  "complete_stl works", {
    
    expect_identical(
      heemod:::complete_stl(NULL, sn, mn, 10),
      structure(list(I = structure(
        c(10, 10, 10, 10, 10),
        .Names = c("A",  "B", "C", "D", "E")),
        II = structure(
          c(10, 10, 10, 10, 10),
          .Names = c("A", 
                     "B", "C", "D", "E")),
        III = structure(
          c(10, 10, 10, 10, 10),
          .Names = c("A",  "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(5, sn, mn, 10),
      structure(list(I = structure(
        c(5, 5, 5, 5, 5), .Names = c("A", "B", "C", "D", "E")),
        II = structure(
          c(5, 5, 5, 5, 5), .Names = c("A", 
                                       "B", "C", "D", "E")),
        III = structure(
          c(5, 5, 5, 5, 5), .Names = c("A", 
                                       "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(c(B = 5), sn, mn, 10),
      structure(list(
        I = structure(
          c(10, 5, 10, 10, 10),
          .Names = c("A",
                     "B", "C", "D", "E")),
        II = structure(
          c(10, 5, 10, 10, 10),
          .Names = c("A", 
                     "B", "C", "D", "E")),
        III = structure(
          c(10, 5, 10, 10, 10), 
          .Names = c("A", 
                     "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(c(A = 5, E = 8), sn, mn, 10),
      structure(list(I = structure(
        c(5, 10, 10, 10, 8), .Names = c("A", 
                                        "B", "C", "D", "E")),
        II = structure(
          c(5, 10, 10, 10, 8), .Names = c("A", 
                                          "B", "C", "D", "E")),
        III = structure(
          c(5, 10, 10, 10, 8), .Names = c("A", 
                                          "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
    
    expect_identical(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 4)
        ),
        sn, mn, 10),
      structure(list(
        I = structure(c(5, 10, 10, 10, 8),
                      .Names = c("A", 
                                 "B", "C", "D", "E")),
        II = structure(c(10, 10, 10, 10, 10),
                       .Names = c("A", 
                                  "B", "C", "D", "E")),
        III = structure(c(10, 2, 4, 10, 10),
                        .Names = c("A", 
                                   "B", "C", "D", "E"))),
        .Names = c("I", "II", "III"))
    )
  }
)

test_that(
  "complete_stl throws errors", {
    
    expect_error(
      heemod:::complete_stl(-1, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(NA, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(11, sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(5.5, sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_stl(c(A = 1, B = -1), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(c(A = 1, B = NA), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(c(A = 1, B = 11), sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(c(A = 1, B = 5.5), sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_stl(c(A = 1, G = 5), sn, mn, 10)
    )
    
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = -1)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = NA)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 11)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, C = 5.5)
        ),
        sn, mn, 10)
    )

    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          III = c(B = 2, G = 5)
        ),
        sn, mn, 10)
    )
    expect_error(
      heemod:::complete_stl(
        list(
          I = c(A = 5, E = 8),
          VIII = c(B = 2, C = 5)
        ),
        sn, mn, 10)
    )
  }
)

test_that(
  "Expansion works.", {
    f <- function(x) abs(sin(x))
    
    tm <- define_transition(
      .5, .5,
      .3, .7
    )
    tm_exp <- define_transition(
      .4, .6,
      C, f(state_time)
    )
    
    sA <- define_state(
      c = 5,
      e = 3
    )
    sB <- define_state(
      c = 3,
      e = 9
    )
    sA_exp <- define_state(
      c = f(state_time),
      e = 7
    )
    
    expect_message(
      res <- run_model(
        define_strategy(
          transition = tm_exp,
          sA, sB
        ),
        cycles = 10,
        cost = c, effect = e
      ),
      "expanding state: B\\."
    )
    expect_equal(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(37697.04, 66908.88),
      ignore_attr = TRUE
    )
    
    expect_message(
      res <- run_model(
          define_strategy(
            transition = tm,
            sA_exp, sB
          ),
          cycles = 10,
          cost = c, effect = e
        ),
      "expanding state: A\\."
    )
    expect_equal(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(20552.39, 81562.5),
      ignore_attr = TRUE
    )
    
    expect_message(
      res <- run_model(
        define_strategy(
          transition = tm_exp,
          sA_exp, sB
        ),
        cycles = 10,
        cost = c, effect = e
      ),
      "expanding states: A, B\\."
    )
    expect_equal(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(21488.12, 82302.96),
      ignore_attr = TRUE
    )
    
    
    res <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(A = 3, B = 8)
    )
    expect_equal(
      round(unlist(res$run_model[c(".cost", ".effect")]), 2),
      c(21487.09, 82302.96),
      ignore_attr = TRUE
    )
    
    res1 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = 5
    )
    res2 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(A = 5, B = 5)
    )
    expect_equal(
      res1$run_model, res2$run_model,
      ignore_attr = TRUE
    )
    
    res1 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(B = 7)
    )
    res2 <- run_model(
      define_strategy(
        transition = tm_exp,
        sA_exp, sB
      ),
      cycles = 10,
      cost = c, effect = e,
      state_time_limit = c(A = 10, B = 7)
    )
    expect_equal(
      res1$run_model, res2$run_model,
      ignore_attr = TRUE
    )
    
    
    f <- function(x) {
      abs(sin(x)) / 2
    }
    mat_mc <- define_transition(
      C, f(model_time),
      0, 1
    )
    mat_sc <- define_transition(
      C, f(state_time),
      0, 1
    )
    sA <- define_state(c = 1, e = 1)
    sB <- define_state(c = 0, e = 0)
    
    strat_mc <- define_strategy(
      transition = mat_mc,
      sA, sB
    )
    strat_sc <- define_strategy(
      transition = mat_sc,
      sA, sB
    )
    res <- summary(run_model(
      strat_sc, strat_mc,
      cycles = 10,
      cost = c, effect = e
    ))
    
    expect_identical(
      res$res_comp$.icer[2], NaN
    )
  }
)
test_that(
  "Expansion works with 3x3 transition matrix", {
    f <- function(x) abs(sin(x))/2
tm_exp <- define_transition(
  .4, .5, .1,
  .5, f(state_time), C,
  0, 0, 1
)

sA <- define_state(
  c = 5,
  e = 3
)
sB <- define_state(
  c = 3,
  e = 9
)
sC <- define_state(
  c = 0,
  e = 0
)

res <- run_model(
  define_strategy(
    transition = tm_exp,
    sA, sB, sC
  ),
  cycles = 2,
  cost = c, effect = e
)
expected_matrix <- matrix(rep(0, 5^2), ncol = 5)
expected_matrix[, 1] <- c(0.4, rep(0.5,3), 0)
expected_matrix[1, 2] <- 0.5
expected_matrix[2,3] <- f(1)
expected_matrix[3,4] <- f(2)
expected_matrix[4,4] <- f(3)
expected_matrix[,5] <- 1-rowSums(expected_matrix[,1:4])

expect_equal(res$eval_strategy_list$I$transition[[1]],
             expected_matrix
             )


})

test_that(
  "Expansion works with 5x5 transition matrix", {
    f <- function(x) abs(sin(x))/4
  tm_exp <- define_transition(
    .3, .3, .2, .1, .1, 
    .2, .2, .4, .1, .1, 
    .2, 0.2, f(state_time), 0.1, C,
    .3, .3, .2, .1, .1, 
    0, 0, 0, 0, 1 
  )
  sA <- define_state(
    c = 5,
    e = 3
  )
  sB <- define_state(
    c = 3,
    e = 9
  )
  sC <- define_state(
    c = 0,
    e = 1
  )
  sD <- define_state(
    c = 4,
    e = 5
  )
  sE<- define_state(
    c = 0,
    e = 0
  )
  res <- run_model(
    define_strategy(
      transition = tm_exp,
      sA, sB, sC, sD, sE
    ),
    cycles = 2,
    cost = c, effect = e
  )
  expected_matrix <- matrix(rep(0, 7^2), ncol = 7)
  expected_matrix[,1] <- expected_matrix[,2] <- c(0.3, rep(0.2,4), 0.3, 0)
  expected_matrix[,3] <- c(0.2, 0.4,  rep(0,3), 0.2, 0)
  expected_matrix[3,4] <- f(1)
  expected_matrix[4, 5] <- f(2)
  expected_matrix[5,5] <- f(3)
  expected_matrix[, 6] <- c(rep(0.1, 6), 0)
  expected_matrix[, 7] <- 1-rowSums(expected_matrix[,1:6])
  expect_equal(res$eval_strategy_list$I$transition[[1]],
               expected_matrix
  )
})
