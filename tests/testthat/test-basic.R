test_that("backdoor formula is identified", {
  data <- "p(x,y,z)"
  query <- "p(y|do(x))"
  graph <- "x -> y\nz -> x\nz -> y"
  expect_equal(
    dosearch(data, query, graph)$identifiable,
    TRUE
  )
})