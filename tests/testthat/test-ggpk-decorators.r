context("Default Packets => ggpk_decorators")

test_that("ggpk_decorators can be called without error", {
  expect_silent(ggpk_decorators())
})

test_that("ggpk_decorators can accept arguments", {
  expect_equal(ggpk_decorators(xlab = 'test')@ggcalls$xlab$x, "test")
  expect_equal(ggpk_decorators(xlab = 'test'), ggpk_decorators(xlab.label = 'test'))
  expect_equal(ggpk_decorators(labs.caption = 'test')@ggcalls$labs$caption, "test")
})