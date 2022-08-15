test_that("igraph graph format works", {
  skip_if_not_installed("igraph")
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  g_igraph <- igraph::graph.formula(
    x -+ z, z -+ y, x -+ y, y -+ x,
    simplify = FALSE
  )
  g_igraph <- igraph::set.edge.attribute(g_igraph, "description", 3:4, "U")
  expect_error(
    out <- dosearch(data, query, g_igraph),
    NA
  )
  expect_equal(
    out$formula,
    "\\sum_{z}\\left(p(z|x)\\sum_{x}\\left(p(x)p(y|z,x)\\right)\\right)"
  )
  expect_equal(
    out$identifiable,
    TRUE
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
    "\\sum_{z}\\left(p(z|x)\\sum_{x}\\left(p(x)p(y|z,x)\\right)\\right)"
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
    "p\\(y|x\\)"
  )
  expect_error(
    summ <- summary(out),
    NA
  )
  expect_output(
    print(summ),
    "The query p\\(y|do\\(x\\)\\) is identifiable"
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
    "The query p\\(y|do\\(x\\)\\) is non-identifiable."
  )
  expect_error(
    summ <- summary(out),
    NA
  )
  expect_output(
    print(summ),
    "The query p\\(y|do\\(x\\)\\) is non-identifiable"
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
    tolerance = 10
  )
  out$rule_times <- NULL
  expect_equal(
    get_benchmark(out),
    get_benchmark(out, run_again = TRUE, include_rules = FALSE),
    tolerance = 10
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

test_that("package can be unloaded", {
  expect_error(
    unloadNamespace("dosearch"),
    NA
  )
  expect_false(isNamespaceLoaded("dosearch"))
  library(dosearch)
})
