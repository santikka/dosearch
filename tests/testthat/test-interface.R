test_that("igraph graph format works", {
  skip_if_not_installed("igraph")
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  g_igraph <- igraph::graph_from_literal(
    x -+ z, z -+ y, x -+ y, y -+ x,
    simplify = FALSE
  )
  g_igraph <- igraph::set_edge_attr(g_igraph, "description", 3:4, "U")
  expect_error(
    out <- dosearch(data, query, g_igraph),
    NA
  )
  expect_equal(
    out$formula,
    "\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')"
  )
  expect_equal(
    out$identifiable,
    TRUE
  )
})

test_that("igraph graph without names gets named", {
  skip_if_not_installed("igraph")
  g_igraph <- igraph::delete_vertex_attr(
    igraph::graph_from_literal(
      1 -+ 2, 2 -+ 3
    ),
    "name"
  )
  expect_message(
    parse_graph(g_igraph),
    "Argument `graph` is not named, node names have been assigned"
  )
})

test_that("dagitty graph format works", {
  skip_if_not_installed("dagitty")
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  g_dagitty <- dagitty::dagitty("dag{x -> z -> y; x <-> y}")
  expect_error(
    out <- dosearch(data, query, g_dagitty),
    NA
  )
  expect_equal(
    out$formula,
    "\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')" 
  )
  expect_equal(
    out$identifiable,
    TRUE
  )
})

test_that("dosearch summary and print work", {
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y",
    control = list(
      benchmark = TRUE
    )
  )
  expect_output(
    print(out),
    "p(y|x)",
    fixed = TRUE
  )
  expect_error(
    summ <- summary(out),
    NA
  )
  expect_output(
    print(summ),
    "The query p(y|do(x)) is identifiable.",
    fixed = TRUE
  )
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y\nx <-> y",
    control = list(
      benchmark = TRUE
    )
  )
  expect_output(
    print(out),
    "The query p(y|do(x)) is not identifiable by do-search.",
    fixed = TRUE
  )
  expect_error(
    summ <- summary(out),
    NA
  )
  expect_output(
    print(summ),
    "The query p(y|do(x)) is not identifiable by do-search.",
    fixed = TRUE
  )
})

test_that("summary print truncates long formulas", {
  out <- dosearch(
    "p(x,y,z)",
    "p(y|do(x))",
    "x -> y\nz -> x\nz -> y"
  )
  expect_output(
    print(summary(out), max_chars = 5L),
    "... [formula truncated]",
    fixed = TRUE
  )
  expect_output(
    print(summary(out)),
    "\\sum_{z}p(z)p(y|x,z)\n",
    fixed = TRUE
  )
})

test_that("summary print works without a formula", {
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y",
    control = list(formula = FALSE)
  )
  expect_output(
    print(summary(out)),
    "Formula: NA",
    fixed = TRUE
  )
})

test_that("summary formats bidirected edges", {
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y\nx <-> y"
  )
  expect_identical(
    summary(out)$graph,
    "x -> y\n\tx <-> y"
  )
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y\nx -- y"
  )
  expect_identical(
    summary(out)$graph,
    "x -> y\n\tx <-> y"
  )
})

test_that("summary time units are correct", {
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y",
    control = list(
      benchmark = TRUE
    )
  )
  expect_output(
    print(summary(out)),
    "seconds"
  )
  out$time <- 1e5
  expect_output(
    print(summary(out)),
    "minutes"
  )
  out$time <- 4e6
  expect_output(
    print(summary(out)),
    "hours"
  )
})

test_that("gets can be got", {
  out <- dosearch(
    "p(x,y,z, w)",
    "p(y|do(x))",
    "x -> y \n z -> x \n w -> z \n x <-> w \n w <-> y",
    control = list(
      benchmark = TRUE,
      benchmark_rules = TRUE,
      draw_derivation = TRUE,
      draw_all = TRUE
    )
  )
  expect_identical(
    is_identifiable(out),
    TRUE
  )
  expect_identical(
    get_formula(out),
    get_formula(out, run_again = TRUE)
  )
  expect_identical(
    get_derivation(out),
    get_derivation(out, run_again = TRUE, draw_all = TRUE)
  )
  expect_equal(
    get_benchmark(out),
    get_benchmark(out, run_again = TRUE, include_rules = TRUE),
    tolerance = 1000
  )
  out$rule_times <- NULL
  expect_equal(
    get_benchmark(out),
    get_benchmark(out, run_again = TRUE, include_rules = FALSE),
    tolerance = 1000
  )
  out$time <- NULL
  out$formula <- NULL
  out$derivation <- NULL
  expect_identical(
    get_formula(out),
    NULL
  )
  expect_identical(
    get_derivation(out),
    NULL
  )
  expect_identical(
    get_benchmark(out),
    NULL
  )
})

test_that("variants of formulas can be got", {
  data <- "
    p(y*,r_y = 1)
    p(y,r_y = 0)
  "
  args <- list(
    data,
    "p(y|r_y)",
    "y -> r_y",
    missing_data = "r_y : y",
    case_reasoning = TRUE
  )
  out <- do.call(dosearch, args)
  variants <- c("r_y = 0" = "p(y|r_y = 0)", "r_y = 1" = "p(y^{\\ast}|r_y = 1)")
  expect_identical(
    names(out),
    c("identifiable", "formula", "variants", "call")
  )
  expect_identical(out$variants, variants)
  expect_identical(get_formula(out, variants = TRUE), variants)
  expect_identical(get_formula(out), out$formula)
  expect_identical(
    get_formula(out, run_again = TRUE, variants = TRUE),
    variants
  )
  # Printing shows the general formula
  expect_output(print(out), "\\vee", fixed = TRUE)
  expect_output(print(summary(out)), "\\vee", fixed = TRUE)
  # The variants are given only with the formula
  out <- do.call(dosearch, c(args, list(control = list(formula = FALSE))))
  expect_identical(names(out), c("identifiable", "call"))
  expect_null(get_formula(out, variants = TRUE))
  expect_identical(
    get_formula(out, run_again = TRUE, variants = TRUE),
    variants
  )
  # Without case-by-case reasoning, there are no variants
  out <- dosearch("p(x,y)", "p(y|x)", "x -> y")
  expect_identical(names(out), c("identifiable", "formula", "call"))
  expect_identical(get_formula(out, variants = TRUE), "p(y|x)")
})

test_that("alternative distribution format works", {
  data <- "
    p(x)
    p(y|x)
    p(z|x,y)
  "
  data_alt <- list(
    c(x = 0),
    c(y = 0, x = 2),
    c(z = 0, x = 2, y = 2)
  )
  query <- "p(y|do(x))"
  query_alt <- c(y = 0, x = 1)
  graph <- "
    x -> y
    z -> x
    z -> y
  "
  expect_identical(
    dosearch(data, query, graph)$formula,
    dosearch(data_alt, query_alt, graph)$formula
  )
  data_alt <- list(
    "p(x)",
    "p(y|x)",
    "p(z|x,y)"
  )
  expect_identical(
    dosearch(data, query, graph)$formula,
    dosearch(data_alt, query_alt, graph)$formula
  )
})

test_that("empty return objects can be got", {
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", control = list(empty = TRUE)),
    NA
  )
  expect_error(
    dosearch(
      "p(x)",
      "p(y)",
      "x -> y \n z -> y : x = 0",
      control = list(empty = TRUE)
    ),
    NA
  )
})

test_that("plot method works with Diagrammer", {
  skip_if_not_installed("DiagrammeR")
  out <- dosearch(
    "p(x,y,z, w)",
    "p(y|do(x))",
    "x -> y \n z -> x \n w -> z \n x <-> w \n w <-> y",
    control = list(
      draw_derivation = TRUE,
      draw_all = TRUE
    )
  )
  expect_error(plot(out), NA)
})

test_that("plot method informs if Diagrammer is not installed", {
  skip_if_not_installed("mockr")
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y",
    control = list(
      draw_derivation = TRUE,
      draw_all = TRUE
    )
  )
  mockr::with_mock(
    require_namespace = function(...) FALSE,
    {
      expect_message(
        plot(out),
        "Please install the `DiagrammeR` package to plot derivations"
      )
    }
  )
})

test_that("plot method informs if no derivation is available", {
  expect_message(
    plot(dosearch("p(x,y)", "p(y|do(x))", "x -> y")),
    "No derivation is available to plot"
  )
})

test_that("rule benchmarks are named by rule", {
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y",
    control = list(benchmark_rules = TRUE)
  )
  bm <- get_benchmark(out)
  expect_false(is.null(bm$time))
  expect_named(bm$rule_times, c("4", "5", "-2", "-3", "2", "3", "6", "-6"))
  out <- dosearch(
    "p(x,y)",
    "p(y|do(x))",
    "x -> y",
    control = list(benchmark_rules = TRUE, rules = c(5, 4, 5))
  )
  expect_named(out$rule_times, c("5", "4"))
})

test_that("igraph bidirected edges can be given in one direction", {
  skip_if_not_installed("igraph")
  g_igraph <- igraph::graph_from_literal(
    x -+ z, z -+ y, y -+ x,
    simplify = FALSE
  )
  g_igraph <- igraph::set_edge_attr(g_igraph, "description", 3, "U")
  out <- dosearch("p(x,y,z)", "p(y|do(x))", g_igraph)
  expect_identical(
    out$formula,
    "\\sum_{z}p(z|x)\\sum_{x'}p(x')p(y|z,x')"
  )
})

test_that("igraph graph with a single unnamed vertex gets named", {
  skip_if_not_installed("igraph")
  expect_message(
    parse_graph(igraph::make_empty_graph(1L)),
    "node names have been assigned: a"
  )
})
