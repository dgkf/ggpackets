context("Core => ggpack => parameter passing")

test_that("Parameter passing using mapping parameter works", {
  p <- ggplot2::ggplot() + 
    ggpack(ggplot2::geom_bar, id = 'bar', mapping = ggplot2::aes(x = c(1, 2), fill = 'red'))
  
  expect_true(p$layers[[1]]$aes_params$fill == 'red')
})

test_that("Check parameter passing to nested ggpackets", {
  x <- function(...) { ggpack(ggplot2::geom_point, id = 'point', dots = substitute(...())) }
  y <- function(...) { ggpack(x, id = 'test', ...) }
  
  p1 <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = mpg, y = wt) + x(point.color = 'red')
  p2 <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = mpg, y = wt) + y(test.point.color = 'blue')
  
  expect_true(p1$layers[[1]]$aes_params$colour == 'red')
  expect_true(p2$layers[[1]]$aes_params$colour == 'blue')
})

test_that("Check parameter overriding based on dots position with respect to 'dots' parameter", {
  p_red <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = carb) + 
    ggpack(ggplot2::geom_bar, id = 'test', 
           fill = 'red')
  
  p_blue <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = carb) + 
    ggpack(ggplot2::geom_bar, id = 'test', 
           fill = 'red', dots = list(test.fill = 'blue'))
  
  p_green <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = carb) + 
    ggpack(ggplot2::geom_bar, id = 'test', 
      fill = 'red', dots = list(test.fill = 'blue'), fill = 'green')
  
  expect_true(p_red$layers[[1]]$aes_params$fill == 'red')
  expect_true(p_blue$layers[[1]]$aes_params$fill == 'blue')
  expect_true(p_green$layers[[1]]$aes_params$fill == 'green')
})

test_that("mapping parameter aesthetics are treated like other aesthetic arguments", {
  p_am <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = carb) + 
    ggpack(ggplot2::geom_bar, id = 'test', 
           color = 'red',
           fill = am)
  
  p_vs <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = carb) + 
    ggpack(ggplot2::geom_bar, id = 'test', 
           fill = am, 
           mapping = ggplot2::aes(fill = vs))
  
  p_hp <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = carb) + 
    ggpack(ggplot2::geom_bar, id = 'test', 
           fill = am, 
           mapping = ggplot2::aes(fill = vs), 
           mapping = ggplot2::aes_string(fill = 'hp'))
  
  p_wt <- ggplot2::ggplot(mtcars) + ggplot2::aes(x = carb) + 
    ggpack(ggplot2::geom_bar, id = 'test',
           color = 'red',
           fill = am, 
           mapping = ggplot2::aes(fill = vs),
           mapping = ggplot2::aes_string(fill = 'hp'), 
           fill = wt,
           color = 'green')
  
  expect_true(p_am$layers[[1]]$mapping$fill == 'am')
  expect_true(p_am$layers[[1]]$aes_params$colour == 'red')
  
  expect_true(p_vs$layers[[1]]$mapping$fill == 'vs')
  
  expect_true(p_hp$layers[[1]]$mapping$fill == 'hp')
  
  expect_true(p_wt$layers[[1]]$mapping$fill == 'wt')
  expect_true(p_wt$layers[[1]]$aes_params$colour == 'green')
})

context("Core => ggpack => message display")

test_that('Warning messages are displayed when auto_remove_aes is false and invalid parameters are passed', {
  f <- function(...) ggpack(ggplot2::geom_point, id = list(NULL, 'test'), ...)
  expect_output(show(f(myid.color = 'blue')), '^ggpacket')
  
  # expect_message doesn't work in R v3.1.0, reverting to instead confirm that 
  # ggpack was not successful, by finding that length is 0
  # expect_message(show(f(myid.color = 'blue', auto_remove_aes = FALSE)), 'myid.color')
  #
  # early versions of ggplot throw an error during geom construction with
  # unknown parameters, while later versions will ignore them, ensure that
  # length of ggpack() matches ggplot behavior
  expect_equal({
    length(f(myid.color = 'blue', auto_remove_aes = FALSE)@ggcalls[[1]])
  }, tryCatch(ggplot2::geom_point(myid.color = 3), 
    warning = function(w) 1, # recent ggplot throws warnings (ggpack length -> 1)
    error = function(e) 0 ) # old ggplot threw errors (ggpack length -> 0)
  )
  
  
  f <- function(...) ggpack(ggplot2::geom_point, id = 'test', ...)
  expect_output(show(f(myid.color = 'blue')), '^ggpacket')
  expect_output(show(f(myid.color = 'blue', auto_remove_aes = FALSE)), '^ggpacket')
})

# test_that('Errors get shown if they are caught during ggproto construction', {
#   # expect_message(ggpack(stat_count, y = 'test'), 'must not be used with a y aesthetic')
# 
#   # expect_message doesn't work in R v3.1.0, reverting to instead confirm that 
#   # ggpack was not successful, by finding that length is 0
#     
#   # TODO (testthat): Rebuild this test case for lazy
#   expect_equal(length(ggpack(ggplot2::stat_count, y = 'test')), 0)
# })

context("Core => ggpack => underlying functions")

test_that('Filter down to last arguments works', {
  expect_equal({ 
    a <- list(a = 1, b = 2, c = 3, a = 4, d = 5, c = 6)
    a[last_args(names(a))]
  },
    list(b = 2, a = 4, d = 5, c = 6)
  )
})

# test_that('Warnings get properly produced', {
#   # TODO (testthat): Decide whether these test cases are necessary or if
#   # printing of ggpack is communicative enough
# 
#   expect_warning(
#     last_args(
#       list(a = 1, b = 2, c = 3, a = 4, d = 5, c = 6, d = 7),
#       sources = list('a', 'a', 'a', 'b', 'b', 'c', 'c'),
#       warn = c('a')),
#     'some arguments were overwritten'
#   )
# 
#   expect_warning(
#     last_args(
#       list(a = 1, b = 2, c = 3, a = 4, d = 5, c = 6, d = 7),
#       sources = list('a', 'a', 'a', 'b', 'b', 'c', 'c'),
#       warn = c('a')),
#     '2. '
#   )
# 
#   expect_warning(
#     last_args(
#       list(a = 1, b = 2, c = 3, a = 4, d = 5, c = 6, d = 7),
#       sources = list('a', 'a', 'a', 'b', 'b', 'c', 'c'),
#       warn = c('a'), desc = c(a = 'this_is_a_test')),
#     'this_is_a_test'
#   )
# })