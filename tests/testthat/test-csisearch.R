# Test examples from:
# S. Tikka, A. Hyttinen, J. Karvanen, Identifying causal effects via
# context-specific independence relations, In Proceedings of the 33rd Annual
# Conference on Neural Information Processing Systems, 2019.

test_that("causal effect of X on Y in fig 6(a) is identified", {
  data <- "P(X,Y,W)"
  query <- "P(Y|X,I_X=1)"
  graph <- "
    I_X -> X
    W -> X : I_X = 1
    Z -> X : I_X = 1; W = 1
    X -> Y
    Z -> Y
    Z -> X
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_identical(
    out$formula,
    "p(Y|X,W = 1)"
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
})

test_that("causal effect of X on Y in fig 6(b) is identified", {
  data <- "P(X,Y,Z)"
  query <- "P(Y|X,I_X=1)"
  graph <- "
    I_X -> X
    I_Z -> Z
    A -> Z : I_Z = 1
    A -> Y
    H -> X : I_X = 1
    H -> Y
    X -> Z : A = 0; I_Z = 1
    Z -> Y : A = 1
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_identical(
    out$formula,
    "p(Y)"
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
})

test_that("causal effect of X on Y in fig 6(c) is identified", {
  data <- "P(X,Y,Z)"
  query <- "P(Y|X,I_X=1)"
  graph <- "
    X -> Y : Z = 1
    Z -> Y
    Z -> X : I_X = 1
    I_X -> X
    H -> X : I_X = 1
    H -> Z
    Q -> Z
    Q -> Y : Z = 0
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{Z}\\left[\\left(p(Y|X,Z = 0)\\left[p(Z)\\right]_{Z = 0}\\right) ",
      "/\\ \\left(p(Y|Z = 1)\\left[p(Z)\\right]_{Z = 1}\\right)\\right]"
    )
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
})

test_that("causal effect of X on Y in fig 6(d) is identified", {
  data <- "P(X,Y,Z,A,W)"
  query <- "P(Y|X,I_X=1)"
  graph <- "
    I_X -> X
    I_Z -> Z
    A -> W
    Z -> Y
    A -> Z
    X -> Z : I_Z = 1; A = 1
    X -> Y : A = 0
    W -> X : I_X = 1
    W -> Y : A = 0
    A -> Y
    U -> X : I_X = 1
    U -> Y : A = 1
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{Z,W,A}\\left[\\left(\\left[p(A)\\right]_{A = 0}",
      "\\left(\\left[p(Z|X,A)\\right]_{A = 0}\\sum_{X}",
      "\\left(\\left[p(X,W|A)\\right]_{A = 0}",
      "\\left[p(Y|X,Z,W,A)\\right]_{A = 0}\\right)\\right)\\right) /\\ ",
      "\\left(p(W)\\left(p(Z,Y|X,W,A = 1)",
      "\\left[p(A|X,W)\\right]_{A = 1}\\right)\\right)\\right]"
    )
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
})

test_that("causal effect of X on Y in fig 6(e) is identified", {
  data <- "P(X,Y,Z,A)"
  query <- "P(Y|X,I_X=1)"
  graph <- "
    I_X -> X
    I_W -> W
    I_Z -> Z
    A -> W : I_W = 1
    A -> Z : I_Z = 1
    W -> Z : I_Z = 1
    Z -> X : I_X = 1
    X -> Y
    L -> W : A = 0; I_W = 1
    L -> X : I_X = 1
    M -> W : I_W = 1
    M -> Y
    N -> Z : I_Z = 1
    N -> Y
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_identical(
    out$formula,
    "\\sum_{Z}\\left(p(Y|X,Z,A = 0)\\left[p(Z|A)\\right]_{A = 0}\\right)"
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
})

test_that("nested csi criterion is applied", {
  data <- "P(Y)"
  query <- "P(Y|X)"
  graph <- "
    X -> Z : A = 0
    A -> Z
    A -> Y
    X -> W : B = 0
    W -> A : B = 1
    B -> A
    B -> W
    Z -> Y : A = 1
  "
  expect_error(
    out <- dosearch(data, query, graph, control = list(cache = FALSE)),
    NA
  )
  expect_identical(
    out$formula,
    "p(Y)"
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
})

test_that("trivial non-identifiability is checked", {
  out <- dosearch("p(x)", "p(y)", "x -> y\nz -> y : x = 1")
  expect_identical(
    out$identifiable,
    FALSE
  )
  expect_identical(
    out$formula,
    ""
  )
})

test_that("trivial identifiability is checked", {
  out <- dosearch("p(y)", "p(y)", "x -> y\nz -> y : x = 1")
  expect_identical(
    out$identifiable,
    TRUE
  )
  expect_identical(
    out$formula,
    "p(y)"
  )
})
