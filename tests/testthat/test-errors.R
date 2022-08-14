test_that("missing required arguments fails", {
  expect_error(
    dosearch(),
    "Argument `data` is missing."
  )
  expect_error(
    dosearch("p(x)"),
    "Argument `query` is missing."
  )
  expect_error(
    dosearch("p(x)", "p(y)"),
    "Argument `graph` is missing."
  )
})

test_that("invalid input types fail", {
  expect_error(
    dosearch(y ~ x),
    "Argument `data` is of an unsupported type\\."
  )
  expect_error(
    dosearch(c("a", "b")),
    "Argument `data` must be of length 1 when of type `character`\\."
  )
  expect_error(
    dosearch("p(x)", y ~ x),
    "Argument `query` is of an unsupported type\\."
  )
  expect_error(
    dosearch("p(x)", c("a", "b")),
    "Argument `query` must be of length 1 when of type `character`\\."
  )
  expect_error(
    dosearch("p(x)", "p(y)", y ~ x),
    "Argument `graph` is of an unsupported type\\."
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", control = 0L),
    "Argument `control` must be a list."
  )
  specs <- c("transportability", "selection_bias", "missing_data")
  args_init <- list(data = "p(x)", query = "p(y)", graph = "x -> y")
  for (spec in specs) {
    args <- args_init
    args[[spec]] <- list()
    expect_error(
      do.call("dosearch", args = args),
      paste0("Argument `", spec, "` must be a character vector of length 1\\.")
    )
  }
})

test_that("malformed alternative distribution format input fails", {
  query <- "p(y|do(x))"
  graph <- "x -> y"
  expect_error(
    dosearch(c(x = NA_real_), query, graph),
    paste0(
      "Invalid distribution format c\\(x = NA_real_\\): ",
      "all role values must be non-missing and finite"
    )
  )
  expect_error(
    dosearch(c(x = -1), query, graph),
    paste0(
      "Invalid variable roles in distribution format c\\(x = -1\\): ",
      "all role values must be either 0, 1 or 2"
    )
  )
  expect_error(
    dosearch(c(x = 1, y = 1), query, graph),
    paste0(
      "Invalid variable roles in distribution format c\\(x = 1, y = 1\\): ",
      "at least one variable must have role value 0"
    )
  )
  expect_error(
    dosearch(c(0, 0), query, graph),
    paste0(
      "Invalid distribution format c\\(0, 0\\): ",
      "role values must be given as a named vector"
    )
  )
  expect_error(
    dosearch(list(list()), query, graph),
    "Unable to parse distribution format list()"
  )
})

test_that("control arguments of wrong length fail", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", control = list(formula = c(TRUE, TRUE))),
    paste0(
      "All elements of argument `control` ",
      "must be of length 1 \\(except `rules`\\)\\.\n",
      "The following elements have length > 1: formula"
    )
  )
})

test_that("unknown control arguments fail", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", control = list(wrong_arg = 1)),
    "Unknown control arguments: wrong_arg"
  )
})

test_that("wrong control argument types fail", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", control = list(formula = -1)),
    paste0(
      "Some elements of argument `control` have an invalid type\\.\n",
      "Invalid arguments: formula\n",
      "Provided types: double\n",
      "Expected types: logical"
    )
  )
})

test_that("gets can't be got for non-dosearch objects", {
  err <- "Argument `x` must be an object of class `dosearch`\\."
  expect_error(is_identifiable(data.frame()), err)
  expect_error(get_formula(data.frame()), err)
  expect_error(get_derivation(data.frame()), err)
  expect_error(get_benchmark(data.frame()), err)
})

test_that("transportability and selection bias nodes exist", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", transportability = "t"),
    "Transportability nodes t are not present in the graph"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", selection_bias = "s"),
    "Selection bias nodes s are not present in the graph"
  )
})

test_that("empty graph fails", {
  expect_error(
    dosearch("p(x)", "p(y)", ""),
    "Invalid graph, the graph is empty\\."
  )
})

test_that("malformed graph lines fail", {
  graph <- "
    x - > y
    x z w y
    x y
  "
  expect_error(
    dosearch("p(x)", "p(y)", graph),
    "Invalid graph, malformed lines found"
  )
  graph <- "
    x z w y :: z = 1
  "
  expect_error(
    dosearch("p(x)", "p(y)", graph),
    "Invalid graph, malformed lines found"
  )
})

test_that("unknown edge type fails", {
  expect_error(
    dosearch("p(x)", "p(y)", "x edge y"),
    "Invalid graph, unknown edge types found: edge"
  )
})

test_that("self loops fail", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> x"),
    "the graph contains self-loops"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x -> x : y = 1"),
    "the graph contains self-loops"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x <-> x"),
    "the graph contains self-loops"
  )
})

test_that("cyclic graph fails", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> z\nz -> y\ny -> x"),
    "the graph contains cycles"
  )
})

test_that("bidirected edge in an LDAG fails", {
  expect_error(
    dosearch("p(x)", "p(y)", "x <-> y : y = 1"),
    "bidirected edges are not supported for LDAGs"
  )
})

test_that("invalid edge labels fail", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y : x = 1"),
    "x cannot appear in the label"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y : y = 1"),
    "y cannot appear in the label"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y : z = 1, z = 0 \n z -> y"),
    "duplicate assignment"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y : z = 0"),
    "only other parents of y may be assigned"
  )
})

test_that("syntactically incorrect data inputs fail", {
  malformed_inputs <- c(NA, "(", "p(", "p(x", "p(x|y", "p(x|do(x", "p(x|do(x)")
  for (m in malformed_inputs) {
    expect_error(
      dosearch(m, "p(y)", "x -> y"),
      "Unable to parse input distribution"
    )
  }
  for (m in malformed_inputs) {
    expect_error(
      dosearch(m, "p(y)", "x -> y : z = 1 \n z -> y"),
      "Unable to parse input distribution"
    )
  }
})

test_that("syntactically correct but semantically incorrect inputs fail", {
  md <- "r_x : x, r_y : y"
  expect_error(
    dosearch("p(x,x)", "p(y)", "x -> y"),
    "duplicated variables"
  )
  expect_error(
    dosearch("p(x,x)", "p(y)", "x -> y : z = 0 \n z -> y"),
    "duplicated variables"
  )
  expect_error(
    dosearch("p(x = 2)", "p(y)", "x -> y : z = 0 \n z -> y"),
    "Invalid value assignment"
  )
  expect_error(
    dosearch("p(x,r_x=2,r_y=1)", "p(y)", "x -> r_x", missing_data = md),
    "multiple symbols used for missing data mechanisms"
  )
  expect_error(
    dosearch("p(x,r_x=2)", "p(y)", "x -> r_x", missing_data = md),
    "invalid symbol used for a missing data mechanism"
  )
  expect_error(
    dosearch("p(x,x*)", "p(y)", "x -> r_x", missing_data = md),
    "true and proxy versions of the same variable on the left-hand side"
  )
  expect_error(
    dosearch("p(y|x,x*)", "p(y)", "x -> r_x", missing_data = md),
    "true and proxy versions of the same variable on the right-hand side"
  )
  expect_error(
    dosearch("p(x|x*)", "p(y)", "x -> r_x", missing_data = md),
    "true variable of a proxy variable on the left-hand side"
  )
  expect_error(
    dosearch("p(x*|x)", "p(y)", "x -> r_x", missing_data = md),
    "proxy variable of a true variable on the left-hand side"
  )
  expect_error(
    dosearch("p(x = 1)", "p(y)", "x -> r_x", missing_data = md),
    "value assignment of a non-missing data mechanism"
  )
  expect_error(
    dosearch("p(x|x)", "p(y)", "x -> y"),
    "same variable on the left and right-hand side"
  )
  expect_error(
    dosearch("p(x|x)", "p(y)", "x -> y : z = 0 \n z -> y"),
    "same variable on the left and right-hand side"
  )
  expect_error(
    dosearch("p(x|do(x))", "p(y)", "x -> y"),
    "same variable on the left and right-hand side"
  )
  expect_error(
    dosearch("p(t)", "p(y)", "t -> y", transportability = "t"),
    "transportability node on the left-hand side"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "y -> t", transportability = "t"),
    "a transportability node cannot be a child of another node"
  )
  expect_error(
    dosearch("p(y|do(t))", "p(y)", "t -> y", transportability = "t"),
    "intervention on a transportability node"
  )
  expect_error(
    dosearch("p(s)", "p(y)", "y -> s", selection_bias = "s"),
    "selection bias node on the left-hand side"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "s -> y", selection_bias = "s"),
    "selection bias node cannot be a parent of another node"
  )
  expect_error(
    dosearch("p(y|do(s))", "p(y)", "y -> s", selection_bias = "s"),
    "intervention on a selection bias node"
  )
})

test_that("igraph input fails when the package is not available", {
  skip_if_not_installed("mockr")
  skip_if_not_installed("igraph")
  g_igraph <- igraph::graph.formula(
    x -+ z, z -+ y, x -+ y, y -+ x,
    simplify = FALSE
  )
  g_igraph <- igraph::set.edge.attribute(g_igraph, "description", 3:4, "U")
  mockr::with_mock(
    require_namespace = function(...) FALSE,
    {
      expect_error(
        dosearch("p(x)", "p(y)", g_igraph),
        "The `igraph` package is not available"
      )
    }
  )
})

test_that("dagitty input fails when the package is not available", {
  skip_if_not_installed("mockr")
  skip_if_not_installed("dagitty")
  g_dagitty <- dagitty::dagitty("dag{x -> z -> y; x <-> y}")
  mockr::with_mock(
    require_namespace = function(...) FALSE,
    {
      expect_error(
        dosearch("p(x)", "p(y)", g_dagitty),
        "The `dagitty` package is not available"
      )
    }
  )
})

