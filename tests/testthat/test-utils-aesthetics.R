test_that("base R error for unused arguments suppressed", {
  f <- function(a = "test") a

  # simple arguments
  call <- quote(f(b = 3))
  expect_equal(with_ignore_unused_argument(call), "test")

  # quoted arguments
  call <- quote(f(`testing 123` = 3))
  expect_equal(with_ignore_unused_argument(call), "test")

  call <- quote(f(`testing \`123\`` = 3))
  expect_equal(with_ignore_unused_argument(call), "test")

  call <- quote(f(`testing \`123\`` = 3, a = "hello, world"))
  expect_equal(with_ignore_unused_argument(call), "hello, world")

  # accepted arguments
  call <- quote(f())
  expect_equal(with_ignore_unused_argument(call), "test")

  call <- quote(f(a = "abcdefg"))
  expect_equal(with_ignore_unused_argument(call), "abcdefg")

  # multiple arguments
  call <- quote(f(b = 3, c = 4, d = 5))
  expect_equal(with_ignore_unused_argument(call), "test")
})
