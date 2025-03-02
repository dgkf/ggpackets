test_that("ggpackets nest", {
  # expect that ggpackets build into gg layers
  expect_equal({
    p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_line()
    class(p1$layers[[1]]$geom)
  }, {
    p2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
      (ggpacket() + geom_line())
    class(p2$layers[[1]]$geom)
  })

  # expect that nested ggpackets build into gglayers
  expect_equal({
    p1 <- ggplot(mtcars) + aes(x = wt, y = mpg) + geom_line()
    class(p1$layers[[1]]$geom)
  }, {
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) +
      (ggpacket() + (ggpacket() + geom_line()))
    class(p2$layers[[1]]$geom)
  })
})

test_that("ggpackets aesthetic propegate through nested ggpackets", {
  # expect that ggpacket layers can reroute aesthetics
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) +
      (ggpacket() + geom_line(aes(x = ..y.., y = ..x..)))
    p2$layers[[1]]$mapping
  }, {
    aes(x = mpg, y = wt)
  })

  # expect that ggpacket call can reroute aesthetics
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) +
      (ggpacket(aes(x = ..y.., y = ..x..)) + geom_line())
    lapply(p2$layers[[1]]$mapping, rlang::quo_squash)
  }, {
    lapply(aes(x = mpg, y = wt), rlang::quo_squash)
  })

  # expect that nested ggpacket call can reroute aesthetics
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + (
      ggpacket() + (
        ggpacket(aes(x = ..y.., y = ..x..)) + geom_line()
      )
    )
    lapply(p2$layers[[1]]$mapping, rlang::quo_squash)
  }, {
    lapply(aes(x = mpg, y = wt), rlang::quo_squash)
  })

  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + (
      ggpacket(aes(colour = ..y..)) + (
        ggpacket(aes(fill = ..color..)) +
          geom_line(aes(x = ..fill.., y = ..x..))
      )
    )
    lapply(p2$layers[[1]]$mapping, rlang::quo_squash)
  }, {
    lapply(aes(x = mpg, y = wt, colour = mpg, fill = mpg), rlang::quo_squash)
  })
})

test_that("ggpackets data propegates through nested ggpackets", {
  # expect that ggpacket layers can reroute aesthetics
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) +
      (ggpacket() + geom_line(data = mtcars * 2))
    p2$layers[[1]]$data
  }, {
    mtcars * 2
  })

  # expect that ggpacket call can reroute aesthetics
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) +
      (ggpacket(data = mtcars * 2) + geom_line())
    p2$layers[[1]]$data
  }, {
    mtcars * 2
  })

  # expect that nested ggpacket call can reroute aesthetics
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + (
      ggpacket() + (
        ggpacket(data = mtcars * 2) + geom_line()
      )
    )
    p2$layers[[1]]$data
  }, {
    mtcars * 2
  })

  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + (
      ggpacket(data = mtcars * 2) + (
        ggpacket(data = mtcars * 3) + geom_line()
      )
    )
    p2$layers[[1]]$data
  }, {
    mtcars * 3
  })

  # expect that data functions compose through nesting
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + (
      ggpacket(data = function(d) d * 2) + (
        ggpacket(data = ~ . * 2) + geom_line()
      )
    )
    p2$layers[[1]]$data
  }, {
    mtcars * 2 * 2
  })

  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + (
      ggpacket(data = function(d) d * 2) + (
        ggpacket(data = mtcars) + geom_line()
      )
    )
    p2$layers[[1]]$data
  }, {
    mtcars
  })

  # expect that last layer gets added as standalone instead of as part of a
  # ggpacket
  expect_equal({
    p2 <- ggplot(mtcars) + aes(x = wt, y = mpg) + (
      ggpacket(data = function(d) d * 2) +  (
        ggpacket(data = ~ . * 2) + geom_line()
      )
    ) +
      geom_point()
    p2$layers[[2]]$data
  }, {
    waiver()
  })
})
