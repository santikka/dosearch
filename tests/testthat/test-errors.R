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
  for (cr in list(NA, "TRUE", 1L, c(TRUE, FALSE), logical(0L))) {
    expect_error(
      dosearch("p(x)", "p(y)", "x -> y", case_reasoning = cr),
      "Argument `case_reasoning` must be either `TRUE` or `FALSE`\\."
    )
  }
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
      "must be of length 1 \\(except `rules` and `con_vars`\\)\\.\n",
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
  err <- "Argument `x` must be an object of class `dosearch`"
  expect_error(is_identifiable(data.frame()), err)
  expect_error(get_formula(data.frame()), err)
  expect_error(get_derivation(data.frame()), err)
  expect_error(get_benchmark(data.frame()), err)
})

test_that("print and summary fail for non-dosearch objects", {
  err <- "Argument `.+` must be an object of class `(.*?)dosearch`"
  expect_error(print.dosearch(data.frame()), err)
  expect_error(summary.dosearch(data.frame()), err)
  expect_error(print.summary.dosearch(data.frame()), err)
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

test_that("multiple graphs fail", {
  expect_error(
    dosearch("p(x)", "p(y)", c("x -> y", "z -> x")),
    "Argument `graph` must be of length 1 when of `character` type"
  )
})

test_that("special nodes cannot be missing data variables", {
  graph <- "
    x -> y
    r_x -> y
    s -> x
  "
  expect_error(
    dosearch(
      "p(x*,r_x,y)",
      "p(y|do(x))",
      graph,
      transportability = "r_x",
      missing_data = "r_x : x"
    ),
    paste0(
      "Transportability nodes r_x cannot be variables with missing data, ",
      "missing data mechanisms or proxies"
    )
  )
  expect_error(
    dosearch(
      "p(x*,r_x,y)",
      "p(y|do(x))",
      graph,
      selection_bias = "x*",
      missing_data = "r_x : x"
    ),
    paste0(
      "Selection bias nodes x\\* cannot be variables with missing data, ",
      "missing data mechanisms or proxies"
    )
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

test_that("malformed missing data mechanisms fail", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", missing_data = ""),
    "Malformed missing data mechanisms"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "r_x -> y", missing_data = "r_x : x, r_y : y"),
    "A missing data mechanism cannot be a parent of a true variable"
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
    "the value of a missing data mechanism must be either 0 or 1"
  )
  expect_error(
    dosearch("p(x,r_x=a)", "p(y)", "x -> r_x", missing_data = md),
    "the value of a missing data mechanism must be either 0 or 1"
  )
  expect_error(
    dosearch("p(x)", "p(y|r_y=2)", "x -> r_x", missing_data = md),
    "the value of a missing data mechanism must be either 0 or 1"
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
    dosearch("p(x = 0)", "p(y)", "x -> r_x", missing_data = md),
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
  g_igraph <- igraph::graph_from_literal(
    x -+ z, z -+ y, x -+ y, y -+ x,
    simplify = FALSE
  )
  g_igraph <- igraph::set_edge_attr(g_igraph, "description", 3:4, "U")
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

test_that("non-DAG dagitty input fails", {
  skip_if_not_installed("dagitty")
  g_dagitty <- dagitty::dagitty("mag{x -> z -> y; x <-> y}")
  expect_error(
    dosearch("p(x)", "p(y)", g_dagitty),
    "Attempting to use `dagitty`, but the graph type is not `dag`"
  )
})

test_that("variables both conditioned on and intervened on fail", {
  err <- "variables cannot be both conditioned on and intervened on: x"
  expect_error(
    dosearch("p(y|x,do(x))", "p(y)", "x -> y"),
    err
  )
  expect_error(
    dosearch("p(x,y)", "p(y|do(x),x)", "x -> y"),
    err
  )
  expect_error(
    dosearch(
      list(c(y = 0, x = 1, z = 2)),
      "p(y)",
      "x -> y",
      control = list(warn = FALSE)
    ),
    NA
  )
})

test_that("value assignments without missing data mechanisms fail", {
  expect_error(
    dosearch("p(y|do(x),z=0)", "p(y)", "x -> y\nz -> y"),
    "value assignments are only supported for missing data mechanisms"
  )
})

test_that("malformed value assignments fail", {
  md <- "r_x : x"
  expect_error(
    dosearch("p(x*,r_x=)", "p(x)", "x -> y", missing_data = md),
    "malformed value assignment"
  )
  expect_error(
    dosearch("p(x*,r_x,r_x=1)", "p(x)", "x -> y", missing_data = md),
    "duplicated variables r_x"
  )
  graph <- "x -> y : z = 1\nz -> y"
  expect_error(
    dosearch("p(x,y,z=)", "p(y)", graph),
    "Invalid value assignment"
  )
  expect_error(
    dosearch("p(y|x=1,z=2)", "p(y)", graph),
    "Invalid value assignment"
  )
  expect_error(
    dosearch("p(y|z=0,z=1)", "p(y)", graph),
    "duplicated variables z"
  )
})

test_that("invalid edge label values fail", {
  expect_error(
    dosearch("p(x,y,z)", "p(y)", "x -> y : z = 2\nz -> y"),
    "assigned values must be either 0 or 1"
  )
  expect_error(
    dosearch("p(x,y,z)", "p(y)", "x -> y : z\nz -> y"),
    "malformed value assignment"
  )
})

test_that("invalid missing data mechanisms fail", {
  data <- "p(x*,y*,r_x,r_y)"
  query <- "p(x)"
  graph <- "x -> y"
  expect_error(
    dosearch(data, query, graph, missing_data = "r_x : x : y"),
    "Malformed missing data mechanisms: r_x:x:y"
  )
  expect_error(
    dosearch(data, query, graph, missing_data = "r_x : "),
    "Malformed missing data mechanisms: r_x:"
  )
  expect_error(
    dosearch(data, query, graph, missing_data = "r : x, r : y"),
    "A missing data mechanism cannot be shared by multiple variables: r"
  )
  expect_error(
    dosearch(data, query, graph, missing_data = "r_x : x, r_y : x"),
    "Each variable can have only one missing data mechanism: x"
  )
  expect_error(
    dosearch(data, query, graph, missing_data = "r_x : x, x : y"),
    paste0(
      "A variable cannot be both a missing data mechanism ",
      "and a variable with missing data: x"
    )
  )
})

test_that("ambiguous edge types fail", {
  for (e in c("-->", "->>", "<--", "<->>")) {
    expect_error(
      dosearch("p(x,y)", "p(y)", paste("x", e, "y")),
      "Invalid graph, unknown edge types found"
    )
  }
})

test_that("invalid rules fail", {
  expect_error(
    dosearch(
      "p(x,y)",
      "p(y|do(x))",
      "x -> y",
      control = list(rules = c(4, 14))
    ),
    "Invalid rules in argument `control`: 14"
  )
  expect_error(
    dosearch(
      "p(x,y)",
      "p(y|do(x))",
      "x -> y",
      control = list(rules = c(-10, -12))
    ),
    "Invalid rules in argument `control`: -10, -12"
  )
  expect_error(
    dosearch("p(x,y)", "p(y|do(x))", "x -> y", control = list(rules = 0)),
    "Invalid rules in argument `control`: 0"
  )
  expect_error(
    dosearch(
      "p(x,y,z)",
      "p(y)",
      "x -> y : z = 1\nz -> y",
      control = list(rules = -6)
    ),
    "Invalid rules in argument `control`: -6"
  )
})

test_that("unknown context variables fail", {
  expect_error(
    dosearch(
      "p(x,y,z)",
      "p(y)",
      "x -> y : z = 1\nz -> y",
      control = list(con_vars = "w")
    ),
    "Unknown context variables in argument `control`: w"
  )
})

test_that("missing data symbol is deprecated", {
  dep <- "Argument `md_sym` of `control` is deprecated and will be ignored"
  expect_warning(
    out <- dosearch(
      "p(x*,r_x)",
      "p(x*,r_x=1)",
      "x -> r_x",
      missing_data = "r_x : x",
      control = list(md_sym = "a")
    ),
    dep
  )
  expect_true(out$identifiable)
  expect_identical(out$formula, "p(r_x = 1,x^{\\ast})")
  expect_null(out$call$control$md_sym)
  expect_warning(
    dosearch(
      "p(x*,r_x)",
      "p(x*)",
      "x -> r_x",
      missing_data = "r_x : x",
      control = list(md_sym = "1", warn = FALSE)
    ),
    dep
  )
  expect_error(
    suppressWarnings(
      dosearch(
        "p(x*,r_x=a)",
        "p(x)",
        "x -> y",
        missing_data = "r_x : x",
        control = list(md_sym = "a")
      )
    ),
    "the value of a missing data mechanism must be either 0 or 1"
  )
})

test_that("all unknown control arguments are listed", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", control = list(a = 1, b = 2)),
    "Unknown control arguments: a, b"
  )
})

test_that("invalid LDAG queries are reported as queries", {
  expect_error(
    dosearch("p(x,y)", "p(y|y)", "x -> y : z = 1\nz -> y"),
    "Invalid query p(y|y): same variable",
    fixed = TRUE
  )
})
