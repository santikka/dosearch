# Test examples from:
# S. Tikka, A. Hyttinen, J. Karvanen, Identifying causal effects via
# context-specific independence relations, In Proceedings of the 33rd Annual
# Conference on Neural Information Processing Systems, 2019.

test_that("causal effect of X on Y is identified in fig 1(e)", {
  data <- "p(A,X,Y)"
  query <- "p(Y|X,I_X=1)"
  graph <- "
    I_X -> X
    X -> Y : A = 1
    L -> X : I_X = 1; A = 0
    L -> Y
    A -> X : I_X = 1
    A -> Y
  "
  expect_error(
    out <- dosearch(data, query, graph, control = list(heuristic = TRUE)),
    NA
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{A}\\left[\\left(p(Y|X,A = 0)\\left[p(A)\\right]_{A = 0}\\right) ",
      "/\\ \\left(p(Y|A = 1)\\left[p(A)\\right]_{A = 1}\\right)\\right]"
    )
  )
})

test_that("causal effect of X on Y is identified in fig 6(a)", {
  data <- "p(X,Y,W)"
  query <- "p(Y|X,I_X=1)"
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
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y|X,W = 1)")
})

test_that("causal effect of X on Y in fig 6(b) is identified", {
  data <- "p(X,Y,Z)"
  query <- "p(Y|X,I_X=1)"
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
    out <- dosearch(data, query, graph, control = list(draw_derivation = TRUE)),
    NA
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y)")
})

test_that("causal effect of X on Y in fig 6(c) is identified", {
  data <- "p(X,Y,Z)"
  query <- "p(Y|X,I_X=1)"
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
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{Z}\\left[\\left(p(Y|X,Z = 0)\\left[p(Z)\\right]_{Z = 0}\\right) ",
      "/\\ \\left(p(Y|Z = 1)\\left[p(Z)\\right]_{Z = 1}\\right)\\right]"
    )
  )
})

test_that("causal effect of X on Y in fig 6(d) is identified", {
  data <- "p(X,Y,Z,A,W)"
  query <- "p(Y|X,I_X=1)"
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
  expect_true(out$identifiable)
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
})

test_that("causal effect of X on Y in fig 6(e) is identified", {
  data <- "p(X,Y,Z,A)"
  query <- "p(Y|X,I_X=1)"
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
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\sum_{Z}\\left(p(Y|X,Z,A = 0)\\left[p(Z|A)\\right]_{A = 0}\\right)"
  )
})

test_that("nested csi criterion is applied", {
  data <- "p(Y)"
  query <- "p(Y|X)"
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
    dosearch(data, query, graph, control = list(cache = FALSE)),
    NA
  )
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y)")
  expect_error(
    out <- dosearch(data, query, graph, control = list(heuristic = TRUE)),
    NA
  )
  expect_true(out$identifiable)
})

test_that("trivial non-identifiability is checked", {
  out <- dosearch("p(x)", "p(y)", "x -> y\nz -> y : x = 1")
  expect_false(out$identifiable)
  expect_identical(out$formula, "")
})

test_that("trivial identifiability is checked", {
  data <- "p(y)"
  query <- "p(y)"
  graph <- "x -> y\nz -> y : x = 1"
  out <- dosearch(data, query, graph)
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y)")
  out <- dosearch(data, query, graph, control = list(heuristic = TRUE))
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y)")
})

test_that("verbose search works", {
  data <- "p(X,Y,Z)"
  query <- "p(Y|X,I_X = 1)"
  graph <- "
    X -> Y
    I_X -> X
    Z -> X : I_X = 1
    Z -> Y
  "
  out <- capture.output(
    dosearch(data, query, graph, control = list(verbose = TRUE))
  )
  out_len <- length(out)
  expect_match(out[1L], "Setting target")
  expect_match(out[2L], "Adding known distribution")
  expect_match(out[3L], "Initializing search")
  for (i in seq.int(4L, out_len - 3L)) {
    expect_match(out[i], "Derived")
  }
  expect_match(out[out_len - 2L], "Target found")
  expect_match(out[out_len - 1L], "Index")
})

test_that("edge vanishes if label is full", {
  graph <- "
    X -> Y
    A -> X
    L -> X : A = 0; A = 1
    L -> Y
  "
  out <- dosearch("p(X,A,Y)", "p(Y)", graph, control = list(cache = FALSE))
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y)")
  graph <- "
    x -> Y
    A -> X
    B -> X
    L -> X : A = 0, B = 0; A = 1, B = 0; A = 0, B = 1; A = 1, B = 1
    L -> Y
  "
  out <- dosearch("p(X,A,Y)", "p(Y)", graph, control = list(cache = FALSE))
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y)")
})

test_that("csisearch derivation works", {
  data <- "p(X,Y,Z,A,W)"
  query <- "p(Y|X,V=0)"
  graph <- "
    V -> X
    I_Z -> Z
    A -> W
    Z -> Y
    A -> Z
    X -> Z : I_Z = 1; A = 1
    X -> Y : A = 0
    W -> X : V = 0
    W -> Y : A = 0
    A -> Y
    U -> X : V = 0
    U -> Y : A = 1
  "
  expect_error(
    dosearch(data, query, graph, control = list(draw_derivation = TRUE)),
    NA
  )
})

test_that("non-primitive conditioning works", {
  data <- "p(X|W,Z) \n p(Y|X,W)"
  query <- "p(X|Y,W)"
  graph <- "X -> Y \n W -> Y : X = 1"
  out <- dosearch(
    data,
    query,
    graph
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\frac{\\left(p(X|W,Z)p(Y|X,W)\\right)}",
      "{\\sum_{X}\\left(p(X|W,Z)p(Y|X,W)\\right)}"
    )
  )
})

test_that("local CSI is derived", {
  data <- "p(A,Y)"
  query <- "p(Y|X,A=1)"
  graph <- "
    X -> Y : A = 1
    A -> Y
  "
  out <- dosearch(data, query, graph)
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y|A = 1)")
  out <- dosearch(data, query, graph, control = list(cache = FALSE))
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y|A = 1)")
  data <- "p(X|A=1)"
  query <- "p(X|Y,A=1)"
  out <- dosearch(data, query, graph)
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(X|A = 1)")
  out <- dosearch(data, query, graph, control = list(cache = FALSE))
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(X|A = 1)")
})

test_that("case-by-case reasoning is correct", {
  out <- dosearch(
    "p(x,z=0) \n p(x,z=1)",
    "p(x,z)",
    "x -> y : w = 0 \n w -> y",
    control = list(draw_derivation = TRUE, rules = c(5))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(x,z)")
  out <- dosearch(
    "p(x,z=1) \n p(x,z=0)",
    "p(x,z)",
    "x -> y : w = 0 \n w -> y",
    control = list(draw_derivation = TRUE, rules = c(-5))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(x,z)")
})

test_that("general-by-case reasoning is correct", {
  out <- dosearch(
    "p(x|w) \n p(x,z=1)",
    "p(x,z=0)",
    "x -> y : z = 0 \n z -> y",
    control = list(draw_derivation = TRUE, rules = c(-3, 6))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\left(p(x|w) - p(x,z = 1)\\right)")
  out <- dosearch(
    "p(x|w) \n p(x,z=1)",
    "p(x,z=0)",
    "x -> y : z = 0 \n z -> y",
    control = list(draw_derivation = TRUE, rules = c(-3, -7))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\left(p(x|w) - p(x,z = 1)\\right)")
  expect_identical(out$formula, "\\left(p(x|w) - p(x,z = 1)\\right)")
  out <- dosearch(
    "p(x|w) \n p(x,z=0)",
    "p(x,z=1)",
    "x -> y : z = 0 \n z -> y",
    control = list(draw_derivation = TRUE, rules = c(-3, 7))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\left(p(x|w) - p(x,z = 0)\\right)")
})

test_that("reverse product rule enumeration is correct", {
  data <- "p(a|b) \n p(b)"
  query <- "p(a,b)"
  graph <- "
    a -> x : b = 0
    c -> x : a = 0
    b -> x : c = 0
  "
  out <- dosearch(data, query, graph, control = list(rules = -2))
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(a,b)")
})

test_that("custom context variables is supported", {
  data <- "
    p(rx, ry)
    p(x, y, rx = 1, ry = 1)
    p(x, rx = 1, ry = 0)
    p(y, rx = 0, ry = 1)
  "
  query <- "p(y | x, rx = 1, ry = 0)"
  graph <- "
    x -> ry
    x -> y
    rx -> ry
  "
  out <- get_derivation_ldag(
    data, query, graph, control = list(con_vars = c("ry", "rx"))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|ry = 1,x,rx = 1)")
  out <- get_derivation_ldag(data, query, graph)
  expect_false(out$identifiable)
})
