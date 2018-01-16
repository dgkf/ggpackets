context("Private Namespace Dependencies => ggplot2:::.all_aesthetics")

test_that("Test that .all_aesthetics is still in ggplot2 namespace", {
  expect_silent(get('.all_aesthetics', asNamespace('ggplot2'), inherits = FALSE))
  expect_equal(class(get('.all_aesthetics', asNamespace('ggplot2'), inherits = FALSE)), 'character')
})

context("Private Namespace Dependencies => ggplot2:::.base_to_ggplot")

test_that("Test that .base_to_ggplot is still in ggplot2 namespace", {
  expect_silent(get('.base_to_ggplot', asNamespace('ggplot2'), inherits = FALSE))
  expect_equal(class(get('.all_aesthetics', asNamespace('ggplot2'), inherits = FALSE)), 'character')
})