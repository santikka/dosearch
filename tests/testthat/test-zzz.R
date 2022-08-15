test_that("package can be unloaded", {
  expect_error(
    unloadNamespace("dosearch"),
    NA
  )
  expect_false(isNamespaceLoaded("dosearch"))
})
