

## initialization
context("Class Structure => ggpacket => initialization")

test_that("ggpackets initialize properly", {
  p <- ggpacket()
  expect_true(isS4(p) && class(p) == "ggpacket")
  expect_equal(p@ggcalls, list())
})

test_that("Check ggpacket initialization equal to ggpack()", {
  expect_true({ 
    a <- drop_envs(ggpacket() + ggplot2::geom_line())
    b <- drop_envs(ggpack(ggplot2::geom_line))
    a == b
  })
  
  expect_true({
    a <- ggpack() + ggplot2::geom_line()
    b <- ggpack(ggplot2::geom_line)
    a == b
  })
})


## indexing into ggcalls list
context("Class Structure => ggpacket => indexing")

test_that("ggpacket can be indexed using [ primitive.", {
  expect_true({
    a <- (ggpacket() + 
            ggplot2::geom_bar() + 
            ggplot2::geom_point() + 
            ggplot2::geom_line())[c(2, 3)]
    b <- ggpacket() + 
            ggplot2::geom_point() + 
            ggplot2::geom_line()
    a == b
  })
})

test_that("ggpacket can be indexed using [ primitive.", {
  expect_true({
    a <- (ggpacket() + 
            ggplot2::geom_bar() + 
            ggplot2::geom_point() + 
            ggplot2::geom_line())[2]
    b <- ggpacket() + 
            ggplot2::geom_point()
    a == b
  })
})

test_that("ggpacked layers can be indexed using [[ primitive.", {
  expect_true({
    a <- (ggpacket() + ggplot2::geom_bar() + ggplot2::geom_point() + ggplot2::geom_line())[[2]]
    b <- (ggpacket() + ggplot2::geom_point())[[1]]
    a == b
  })
})


## global methods
context("Class Structure => ggpacket => global methods")

test_that("ggpacket names method functioning properly", {
  expect_equal(
    names(setNames(ggpacket() + ggplot2::geom_bar() + ggplot2::geom_point(), 
      list(list('bar'), list('point')))),
    list(list('bar'), list('point'))
  )
  
  expect_equal(
    names(setNames(ggpacket() + ggplot2::geom_bar() + ggplot2::geom_point(), 
      c('bar', 'point'))),
    list(list('bar'), list('point'))
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
  expect_output(
    show(ggpacket() + ggplot2::geom_point(ggplot2::aes(x = c(1, 2)))), 
    ".*requires the following missing aesthetics:.*")
})


## gg arithmetic 
context("Class Structure => ggpacket => arithmetic operators")

test_that("Check ggpacket + NULL", {
  expect_true({
    a <- ggpacket() + NULL + ggplot2::geom_line() + ggpack(NULL)
    b <- ggpack(ggplot2::geom_line)
    a == b
  })
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
  
  expect_true({
    a <- ggpack(x)[[1]]@calldf@call
    b <- x
    a == b
  })
  
  expect_true({
    a <- drop_envs(ggpack(y)[[1]]@calldf@call())
    b <- drop_envs(y())
    a == b
  })
  
  expect_true({
    a <- (ggpack(x) + ggplot2::geom_point())[2]
    b <- ggpacket() + ggplot2::geom_point()
    a == b
  })
  
  expect_true({
    a <- (ggpack(y) + ggplot2::geom_point())[2]
    b <- ggpacket() + ggplot2::geom_point()
    a == b
  })
  
  expect_equal(
    (ggplot(mtcars) + (ggpack(x) + ggplot2::geom_point()))$layers,
    list(ggplot2::geom_line(), ggplot2::geom_point(), ggplot2::geom_point())
  )

  expect_equal(
    (ggplot(mtcars) + (ggpack(y) + ggplot2::geom_point()))$layers,
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







