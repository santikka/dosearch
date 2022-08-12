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
    "Argument `data` is of an unsupported type."
  )
  expect_error(
    dosearch("p(x)", y ~ x),
    "Argument `query` is of an unsupported type."
  )
  expect_error(
    dosearch("p(x)", "p(y)", y ~ x),
    "Argument `graph` is of an unsupported type."
  )
  expect_error(
    dosearch("p(x)", "p(y)", "x -> y", control = 0L),
    "Argument `control` must be a list."
  )
})

test_that("get can't be got for non-dosearch objects", {
  expect_error(
    is_identifiable(data.frame()),
    "Argument `x` must be an object of class `dosearch`."
  )
  expect_error(
    get_formula(data.frame()),
    "Argument `x` must be an object of class `dosearch`."
  )
  expect_error(
    get_derivation(data.frame()),
    "Argument `x` must be an object of class `dosearch`."
  )
  expect_error(
    get_benchmark(data.frame()),
    "Argument `x` must be an object of class `dosearch`."
  )
})
