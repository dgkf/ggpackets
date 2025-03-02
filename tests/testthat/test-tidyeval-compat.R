test_that(
  paste(
    "use of bang-bang operator in tidyeval works when propegated through",
    "ggpacket"
  ),
  {
    y <- rlang::quo(mpg)

    f <- function(...) {
      my_aes <- rlang::enquos(...)
      ggpacket() %+% geom_line(aes(!!!my_aes))
    }

    expect_equal({
      p <- ggplot(mtcars) + aes(x = wt) + (ggpacket() + geom_line(aes(y = !!y)))
      p$layers[[1]]$mapping$y
    }, {
      p <- ggplot(mtcars) + aes(x = wt) + geom_line(aes(y = mpg))
      p$layers[[1]]$mapping$y
    })

    expect_equal({
      p <- ggplot(mtcars) + aes(x = wt) + f(y = mpg, color = am)
      p$layers[[1]]$mapping$y
    }, {
      p <- ggplot(mtcars) + aes(x = wt) + geom_line(aes(y = mpg))
      p$layers[[1]]$mapping$y
    })

    expect_equal({
      p <- ggplot(mtcars) + aes(x = wt) + (ggpacket(aes(y = !!y)) + geom_line())
      p$layers[[1]]$mapping$y
    }, {
      p <- ggplot(mtcars) + aes(x = wt) + geom_line(aes(y = mpg))
      p$layers[[1]]$mapping$y
    })

    expect_equal({
      p <- ggplot(mtcars) +
        aes(x = wt) + (
        ggpacket() + (
          ggpacket() +
            geom_line(aes(y = !!y))
        )
      )

      p$layers[[1]]$mapping$y
    }, {
      p <- ggplot(mtcars) + aes(x = wt) + geom_line(aes(y = mpg))
      p$layers[[1]]$mapping$y
    })
  }
)
