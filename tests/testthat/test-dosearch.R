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
    out <- dosearch(data, query, graph),
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
    out <- dosearch(data, query, graph),
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
