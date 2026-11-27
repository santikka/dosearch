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
    "\\sum_{A}\\left[p(A = 0)p(Y|X,A = 0) \\vee p(A = 1)p(Y|A = 1)\\right]"
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
    "\\sum_{Z}\\left[p(Z = 0)p(Y|X,Z = 0) \\vee p(Z = 1)p(Y|Z = 1)\\right]"
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
      "\\sum_{Z,W,A}\\left[p(A = 0)p(Z|X,A = 0)",
      "\\sum_{X'}p(X',W|A = 0)p(Y|X',Z,W,A = 0) ",
      "\\vee p(W)p(A = 1|X,W)p(Z,Y|X,W,A = 1)\\right]"
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
    "\\sum_{Z}p(Z|A = 0)p(Y|X,Z,A = 0)"
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

test_that("conditioning on a product works", {
  data <- "p(X|W,Z) \n p(Y|X,W)"
  query <- "p(X|Y,W)"
  graph <- "X -> Y \n W -> Y : X = 1"
  out <- dosearch(
    data,
    query,
    graph,
    control = list(warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    "\\frac{p(X|W,Z)p(Y|X,W)}{\\sum_{X'}p(X'|W,Z)p(Y|X',W)}"
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
  expect_identical(out$formula, "\\left[p(x,z = 0) \\vee p(x,z = 1)\\right]")
  out <- dosearch(
    "p(x,z=1) \n p(x,z=0)",
    "p(x,z)",
    "x -> y : w = 0 \n w -> y",
    control = list(draw_derivation = TRUE, rules = c(-5))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "\\left[p(x,z = 1) \\vee p(x,z = 0)\\right]")
})

test_that("general-by-case reasoning is correct", {
  out <- dosearch(
    "p(x|w) \n p(x,z=1)",
    "p(x,z=0)",
    "x -> y : z = 0 \n z -> y",
    control = list(draw_derivation = TRUE, rules = c(-3, 6), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(x|w) - p(x,z = 1)")
  out <- dosearch(
    "p(x|w) \n p(x,z=1)",
    "p(x,z=0)",
    "x -> y : z = 0 \n z -> y",
    control = list(draw_derivation = TRUE, rules = c(-3, -7), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(x|w) - p(x,z = 1)")
  expect_identical(out$formula, "p(x|w) - p(x,z = 1)")
  out <- dosearch(
    "p(x|w) \n p(x,z=0)",
    "p(x,z=1)",
    "x -> y : z = 0 \n z -> y",
    control = list(draw_derivation = TRUE, rules = c(-3, 7), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(x|w) - p(x,z = 0)")
  # The assigned variable is the only variable on the left-hand side
  out <- dosearch(
    "p(z=0)",
    "p(z=1)",
    "x -> y : z = 0 \n z -> y",
    control = list(rules = 6, warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "1 - p(z = 0)")
  out <- dosearch(
    "p(z=0|w)",
    "p(z=1)",
    "w -> y\nz -> y : w = 0",
    control = list(rules = c(-3, 6), warn = FALSE)
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "1 - p(z = 0|w)")
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
  expect_identical(out$formula, "p(b)p(a|b)")
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

test_that("case-by-case reasoning works when the first variable is set", {
  # A is the first variable of the graph, so its set representation is 1
  data <- "
    p(A = 1, Y)
    p(A = 0, Y)
  "
  query <- "p(A, Y)"
  graph <- "
    Z -> A
    X -> Y : A = 1
    A -> Y
  "
  out <- dosearch(data, query, graph, control = list(rules = -5))
  expect_true(out$identifiable)
  out <- dosearch(data, query, graph, control = list(rules = 5))
  expect_true(out$identifiable)
})

test_that("variable names containing 'do' are parsed correctly in LDAGs", {
  data <- "p(dose,Y,Z)"
  query <- "p(Y|dose)"
  graph <- "
    dose -> Y : Z = 1
    Z -> Y
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_true(out$identifiable)
  expect_error(
    dosearch("p(dose,Y,Z)", "p(Y|do(dose))", graph),
    "Unable to parse target distribution"
  )
})

test_that("duplicated graph lines do not move edge labels", {
  data <- "p(Z,Y)"
  query <- "p(Y|X,Z=0)"
  graph <- "
    Z -> Y
    X -> Y : Z = 0
    W -> Y
  "
  graph_dup <- "
    Z -> Y
    Z -> Y
    X -> Y : Z = 0
    W -> Y
  "
  out <- dosearch(data, query, graph)
  out_dup <- dosearch(data, query, graph_dup)
  expect_true(out$identifiable)
  expect_identical(out_dup$identifiable, out$identifiable)
  expect_identical(out_dup$formula, out$formula)
})

test_that("labels of the same edge on separate lines are combined", {
  data <- "p(Y,A)"
  query <- "p(Y|X)"
  graph_one <- "
    X -> Y : A = 0; A = 1
    A -> Y
  "
  graph_sep <- "
    X -> Y : A = 0
    X -> Y : A = 1
    A -> Y
  "
  out_one <- dosearch(data, query, graph_one)
  out_sep <- dosearch(data, query, graph_sep)
  expect_true(out_one$identifiable)
  expect_identical(out_sep$identifiable, out_one$identifiable)
  expect_identical(out_sep$formula, out_one$formula)
})

test_that("LDAGs with more than 64 vertices are supported", {
  # X, A and Y are vertices 70, 71 and 68
  chain <- paste0(
    "v", seq_len(67L), " -> v", seq_len(67L) + 1L,
    collapse = "\n"
  )
  graph <- paste0(chain, "\nX -> Y : A = 1\nA -> Y")
  out <- dosearch("p(A,Y)", "p(Y|X,A=1)", graph)
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(Y|A = 1)")
})

test_that("edges that vanish in every context do not affect other contexts", {
  # C -> E vanishes for both values of I_E. Previously, the edge was
  # still listed in the intervention context and restored to the graph
  # after a separation test, which made the result depend on search order
  data <- "
    p(A,F,C = 1)
    p(C,A)
    p(E = 1,I_E|C)
  "
  query <- "p(I_E|F,A,E)"
  graph <- "
    C -> E : I_E = 1; I_E = 0
    C -> A
    C -> F
    E -> A : C = 0
    E -> F
    A -> F : C = 0, E = 1
    I_E -> E : C = 0
  "
  for (heuristic in c(FALSE, TRUE)) {
    for (cache in c(FALSE, TRUE)) {
      out <- dosearch(
        data,
        query,
        graph,
        control = list(heuristic = heuristic, cache = cache)
      )
      expect_true(out$identifiable)
      expect_identical(out$formula, "p(I_E|E = 1,C = 0)")
    }
  }
})

test_that("shared derivation steps are drawn once", {
  data <- "
    p(E,B = 0,D)
    p(F = 0,B = 1|E)
    p(B,D = 1,F)
  "
  query <- "p(B)"
  graph <- "
    D -> F
    E -> B
    B -> F : D = 0
  "
  out <- dosearch(data, query, graph, control = list(draw_derivation = TRUE))
  expect_true(out$identifiable)
  lines <- strsplit(out$derivation, "\n")[[1L]]
  expect_identical(anyDuplicated(lines), 0L)
})

test_that("context variables given via control do not imply independences", {
  # Contexts are also constructed for the additional context variables.
  # Previously, case-by-case reasoning over combinations of them with
  # other context variables was vacuously accepted
  out <- dosearch(
    "p(E = 1,B|F)",
    "p(B,E)",
    "F -> E\nC -> A\nB -> A : C = 1",
    control = list(con_vars = "A")
  )
  expect_false(out$identifiable)
  out <- dosearch(
    "p(C = 0,F)\np(D,C = 1)",
    "p(C|F)",
    "D -> C\nD -> B : C = 0\nC -> B : D = 1\nC -> F",
    control = list(con_vars = "B")
  )
  expect_false(out$identifiable)
  out <- dosearch(
    "p(C = 0,F)\np(D,C = 1)",
    "p(C|F)",
    "D -> C\nD -> B : C = 0\nC -> B : D = 1\nC -> F",
    control = list(con_vars = "B", cache = FALSE)
  )
  expect_false(out$identifiable)
})

test_that("bound variables are primed in LDAGs", {
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
  out <- dosearch(data, query, graph, control = list(heuristic = TRUE))
  expect_true(out$identifiable)
  expect_identical(
    out$formula,
    paste0(
      "\\sum_{A}\\left[p(A = 0)\\sum_{Z}p(Z|X,A = 0)",
      "\\sum_{X'}p(X'|A = 0)p(Y|X',Z,A = 0) \\vee ",
      "\\sum_{W}p(Y|X,W,A = 1)p(W)p(A = 1|X,W)\\right]"
    )
  )
})

test_that("conditional distributions are given as such in LDAGs", {
  # x is deleted in the context a = 0 before y is conditioned on z
  out <- dosearch(
    "p(y,z|x,a = 0)",
    "p(y|z,a = 0)",
    "x -> y : a = 0\na -> y\nz -> y",
    control = list(rules = c(-3, 1))
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(y|x,a = 0,z)")
})
