test_that("backdoor formula is identified", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    z -> x
    z -> y
  "
  expect_error(
    out <- dosearch(data, query, graph),
    NA
  )
  expect_identical(
    out$formula,
    "\\sum_{z}\\left(p(z)p(y|x,z)\\right)",
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
})

test_that("frontdoor formula is identified", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> z
    z -> y
    x <-> y
  "
  expect_error(
    out <- dosearch(data, query, graph),
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

test_that("bow-arc is non-identifiable", {
  data <- "p(x,y)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    x <-> y
  "
  expect_error(
    out <- dosearch(data, query, graph, control = list(heuristic = TRUE)),
    NA
  )
  expect_identical(
    out$identifiable,
    FALSE
  )
})

test_that("all rules are needed", {
  data <- "
    p(w|do(x_2),y,x_1)
    p(y|do(x_2),z_1,z_2,x_1)
    p(x_1|do(x_2),w)
    p(z_2,x_2|do(x_1))
    p(z_1|do(x_1,y),x_2)
  "
  query <- "p(y,x_1|do(x_2),w)"
  graph <- "
    x_1 -> z_2
    x_1 -> z_1
    x_1 -> w
    z_1 -> w
    z_2 -> w
    x_2 -> w
    x_2 -> z_1
    x_2 -> z_2
    z_2 -> y
    z_1 -> y
  "
  expect_error(
    out <- dosearch(
      data,
      query,
      graph,
      control = list(heuristic = TRUE, draw_derivation = TRUE)),
    NA
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
  #
  rules <- c(-2, 2, -3, 3, 4, 5, -6, 6)
  for (r in rules) {
    r_mis <- setdiff(rules, r)
    expect_identical(
      dosearch(data, query, graph, control = list(rules = r_mis))$identifiable,
      FALSE
    )
  }
})

test_that("transportability and selection bias are checked", {
  data <- "
    p(x,z,y|s)
    p(y,z|t,do(x))
  "
  query <- "p(y|do(x))"
  graph <- "
    x -> z
    z -> y
    x -> s
    t -> z
    x <-> y
  "
  out <- dosearch(
    data,
    query,
    graph,
    transportability = "t",
    selection_bias = "s",
    control = list(heuristic = TRUE)
  )
  expect_identical(
    out$identifiable,
    TRUE
  )
  expect_identical(
    out$formula,
    "\\sum_{z}\\left(p(y|do(x),z,t)\\sum_{y}p(z,y|x,s)\\right)"
  )
})

test_that("missing data mechanisms are checked", {
  # simple case-control design scenario
  data <- "p(x*,y*,r_x,r_y)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    y -> r_y
    r_y -> r_x
  "
  md <- "r_x : x, r_y : y"
  out <- dosearch(data, query, graph, missing_data = md)
  expect_identical(
    out$identifiable,
    FALSE
  )
  data <- "
    p(x*,y*,r_x,r_y)
    p(y)
  "
  out <- dosearch(data, query, graph, missing_data = md)
  expect_identical(
    out$identifiable,
    TRUE
  )
  expect_identical(
    out$formula,
    paste0(
      "\\frac{\\left(p(y)p(x|r_x = 1,y,r_y = 1)\\right)}",
      "{\\sum_{y} \\left(p(y)p(x|r_x = 1,y,r_y = 1)\\right)}"
    )
  )
  out <- dosearch(
    data,
    query,
    graph,
    missing_data = md,
    control = list(heuristic = TRUE)
  )
})

test_that("trivial non-identifiability is checked", {
  out <- dosearch("p(x)", "p(y)", "x -> y")
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
  out <- dosearch("p(y)", "p(y)", "x -> y")
  expect_identical(
    out$identifiable,
    TRUE
  )
  expect_identical(
    out$formula,
    "p(y)"
  )
})

test_that("verbose search works", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "
    x -> y
    z -> x
    z -> y
  "
  out <- capture.output(
    dosearch(data, query, graph, control = list(verbose = TRUE))
  )
  out_len <- length(out)
  expect_match(
    out[1L],
    "Setting target"
  )
  expect_match(
    out[2L],
    "Adding known distribution"
  )
  for (i in seq.int(3L, out_len - 3L)) {
    expect_match(
      out[i],
      "Derived"
    )
  }
  expect_match(
    out[out_len - 2L],
    "Target found"
  )
})
