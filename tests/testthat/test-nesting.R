test_that("nested ggpackets using parenthesized syntax can be printed", {
  ggpk1 <- ggpacket() + geom_line()
  ggpk2 <- ggpacket() + ggpk1() + geom_point()
  expect_s4_class(ggpk2, "ggpacket")
  expect_no_error(capture.output(ggpk2))
  p <- ggplot(mtcars) + aes(x = wt, y = mpg) + ggpk2()
  expect_s3_class(p, "gg")
})

test_that("nested ggpackets using non-parenthesized syntax can be printed", {
  ggpk1 <- ggpacket(.id = "test") + geom_line()
  ggpk2 <- ggpacket() + ggpk1 + { ggpk1 } + (ggpk1) + geom_point()
  expect_s4_class(ggpk2, "ggpacket")
  expect_no_error(capture.output(ggpk2))
  p <- ggplot(mtcars) + aes(x = wt, y = mpg) + ggpk2()
  expect_s3_class(p, "gg")
})
