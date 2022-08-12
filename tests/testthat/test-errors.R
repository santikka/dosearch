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
      "all role values must be non-missing and finite\\."
    )
  )
  expect_error(
    dosearch(c(x = -1), query, graph),
    paste0(
      "Invalid variable roles in distribution format c\\(x = -1\\): ",
      "all role values must be either 0, 1 or 2\\."
    )
  )
  expect_error(
    dosearch(c(x = 1, y = 1), query, graph),
    paste0(
      "Invalid variable roles in distribution format c\\(x = 1, y = 1\\): ",
      "at least one variable must have role value 0\\."
    )
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

test_that("empty graph fails", {
  expect_error(
    dosearch("p(x)", "p(y)", ""),
    "Invalid graph, the graph is empty\\."
  )
})

test_that("malformed lines fail", {
  graph <- "
    x - > y
    x z w y
    x y
  "
  expect_error(
    dosearch("p(x)", "p(y)", graph),
    "Invalid graph, malformed lines found: x - > y, x z w y, x y"
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
    "Invalid graph, no self loops are allowed: x -> x"
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x <-> x"),
    "Invalid graph, no self loops are allowed: x <-> x"
  )
})

