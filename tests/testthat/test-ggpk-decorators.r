context("Default Packets => ggpk_decorators")

test_that("ggpk_decorators can be called without error", {
  expect_silent(ggpk_decorators())
})

# for list handling and non-lazy evaluation 
# test_that("ggpk_decorators can accept arguments", {
#   expect_equal(ggpk_decorators(xlab = 'test')@ggcalls$xlab$x, "test")
#   expect_equal(ggpk_decorators(xlab = 'test'), ggpk_decorators(xlab.label = 'test'))
#   expect_equal(ggpk_decorators(labs.caption = 'test')@ggcalls$labs$caption, "test")
# })

# for tibble handling of args & lazy evaluation
test_that("ggpk_decorators can accept arguments", {
  expect_equal(ggpk_decorators(xlab = 'test')[['xlab']]@calldf@args[[1,'val']], "test")
  
  expect_true({
    a <- drop_envs(ggpk_decorators(xlab = 'test'))
    b <- drop_envs(ggpk_decorators(xlab.label = 'test'))
    a == b
  })
  
  expect_equal(
    with(ggpk_decorators(labs.caption = 'test')[['labs']]@calldf@args, val[[name == 'caption']]), 
    "test")
})