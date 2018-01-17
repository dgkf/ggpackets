

## initialization
context("Class Structure => ggpacket => initialization")

test_that("ggpackets initialize properly", {
  p <- ggpacket()
  expect_true(isS4(p) && class(p) == "ggpacket")
  expect_equal(p@ggcalls, list())
})

test_that("Check ggpacket initialization equal to ggpack()", {
  expect_equal(
    ggpacket() + ggplot2::geom_line(),
    ggpack(ggplot2::geom_line)
  )
  expect_equivalent(
    ggpack() + ggplot2::geom_line(),
    ggpack(ggplot2::geom_line)
  )
})


## indexing into ggcalls list
context("Class Structure => ggpacket => indexing")

test_that("ggpacket can be indexed using [ primitive.", {
  expect_equal(
    (ggpacket() + ggplot2::geom_bar() + ggplot2::geom_point() + ggplot2::geom_line())[c(2, 3)],
    ggpacket() + ggplot2::geom_point() + ggplot2::geom_line()
  )
})

test_that("ggpacket can be indexed using [[ primitive.", {
  expect_equal(
    (ggpacket() + ggplot2::geom_bar() + ggplot2::geom_point() + ggplot2::geom_line())[[2]],
    ggpacket() + ggplot2::geom_point()
  )
})


## global methods
context("Class Structure => ggpacket => global methods")

test_that("ggpacket names method functioning properly", {
  expect_equal(
    names(ggpacket(list(ggplot2::geom_bar(), ggplot2::geom_point()), c('bar', 'point'))),
    c('bar', 'point')
  )
})

test_that("When no data is present, outputs plot info", {
  expect_output(show(ggpacket() + ggplot2::geom_point()),
                "Not all layers have been passed sufficient data")
})

test_that("Ensure show method produces error for insufficient data.", {
  expect_output(show(ggpacket() + ggplot2::geom_point()), 
               "Not all layers have been passed sufficient data")
})

test_that("Ensure show method outputs errors from ggplot", {
  expect_output(show(ggpacket() + ggplot2::geom_point(aes(x = c(1, 2)))), 
               ".*requires the following missing aesthetics:.*")
})


## gg arithmetic 
context("Class Structure => ggpacket => arithmetic operators")

test_that("Check ggpacket + NULL", {
  expect_equal(
    ggpacket() + NULL + ggplot2::geom_line() + ggpack(NULL),
    ggpack(ggplot2::geom_line)
  )
})

test_that("ggpacket can be plotted with ggplot", {
  p <- ggpacket() + ggplot2::geom_line()
  expect_s3_class(
    ggplot(mtcars) + aes(x = wt, y = mpg) + p,
    "gg"
  )
})

test_that("ggpacket can be plotted with a theme", {
  p <- ggpacket() + ggplot2::geom_line()
  expect_s3_class(
    ggplot2::ggplot(mtcars) + ggplot2::aes(x = wt, y = mpg) + p + ggplot2::theme_minimal(),
    "gg"
  )
})



## nesting of ggpacket objects
context("Class Structure => ggpacket => nesting")

test_that("ggpacket objects can be nested", {
  x <- ggpacket() + ggplot2::geom_line() + ggplot2::geom_point()
  y <- function() { ggpacket() + ggplot2::geom_line() + ggplot2::geom_point() }
  
  expect_equal(ggpack(x)[[1]], x)
  expect_equal(ggpack(y)[[1]], y())
  
  expect_equal((ggpack(x) + ggplot2::geom_point())[[2]], ggpacket() + ggplot2::geom_point())
  expect_equal((ggpack(y) + ggplot2::geom_point())[[2]], ggpacket() + ggplot2::geom_point())
  
  expect_equal(
    (ggplot(mtcars) + aes(x = carb, y = mpg) + 
       (ggpack(x) + ggplot2::geom_point()))$layers,
    list(ggplot2::geom_line(), ggplot2::geom_point(), ggplot2::geom_point()))
  expect_equal(
    (ggplot(mtcars) + aes(x = carb, y = mpg) + 
      (ggpack(y) + ggplot2::geom_point()))$layers,
    list(ggplot2::geom_line(), ggplot2::geom_point(), ggplot2::geom_point()))
})

test_that("ggpacket objects can be nested, and arguments cascade", {
  nested <- function(...) { 
    ggpack(ggplot2::geom_point, id = 'point', ...) + 
    ggpack(ggplot2::geom_line, id = 'line', ...) 
  }
  
  my_ggpk <- function(...) {
    ggpack(ggplot2::geom_smooth, id = 'smooth', method = 'lm', ...) + 
    ggpack(nested, id = 'nested', ...) 
  }
  
  p1 <- ggplot(mtcars) + aes(x = wt, y = mpg) + my_ggpk()
  p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + my_ggpk(smooth.method = 'loess')
  p3 <- ggplot(mtcars) + aes(x = wt, y = mpg) + my_ggpk(nested.point.color = 'red')
  
  expect_equal(length(p1$layers), 3)
  expect_equal(p1$layers[[1]]$stat_params$method, 'lm')
  expect_equal(p2$layers[[1]]$stat_params$method, 'loess')
  expect_equal(p3$layers[[2]]$aes_params$colour, 'red')
})







