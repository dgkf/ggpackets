context("Tidyeval Compatibility => .aes()")

test_that(".aes function accepts !! operator args", {
  expect_true(require(rlang))
  
  f <- function(x, y, z) {
    x <- rlang::enquo(x) 
    y <- rlang::enquo(y) 
    z <- rlang::enquo(z)
    .aes(!!x, !!y, color = !!z)
  }
  
  expect_silent(f(x, y, z))
  
  expect_equal(names(f(x,y,z)), c('x', 'y', 'colour'))
  
})



# context("Tidyeval => enquos")
# 
# test_that("enquos function equivalent to calling enquo for all listed function args", {
#   expect_equal({
#     f <- function(x, y, z) {
#       x <- rlang::enquo(x); y <- rlang::enquo(y); z <- rlang::enquo(z)
#       as.list(environment())
#     }
#     f(x, y, z)
#   }, {
#     f <- function(x, y, z) {
#       enquos(x, y, z)
#       as.list(environment())
#     }
#     f(x, y, z)
#   })
# })



context("Tidyeval => bquote fallback")

test_that("ensure bquote still works if rlang isn't loaded", {
  expect_equal({
    test = 'red'
    eval(bquote(ggpack(ggplot2::geom_point, color = .(test))))@ggcalls[[1]]$aes_params$colour
  }, 'red')
})