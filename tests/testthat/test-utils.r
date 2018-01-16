context("Utils => %||%")

test_that("Test if-not-null-else operator", {
  l <- list(a = 1, b = 2)
  expect_true(NULL %||% 1 == 1)
  expect_true(1 %||% 2 == 1)
  expect_true(l$a %||% 2 == 1)
  expect_true(l$c %||% 1 == 1)
})

context("Utils => str_format_list")

test_that("Expect string formatting properly format list as english language list", {
  expect_equal(str_format_list(), "")
  expect_equal(str_format_list(letters[1:3]), "a, b and c")
  expect_equal(str_format_list(letters[1:5], oxford = TRUE), "a, b, c, d, and e")
})