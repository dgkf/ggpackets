test_that("ggpackets prints object name in console output", {
  expect_output({
    print(ggpacket())
  }, "<ggpacket>")
})

test_that("ggpackets prints section for bound data in console output", {
  expect_output({
    print(ggpacket())
  }, "Data")
})

test_that("ggpackets without bound data print awaiting message in console output", {
  expect_output({
    print(ggpacket())
  }, "awaiting data")
})

test_that("ggpacket with bound data prints a portion of the bound dataset", {
  expect_output({
    print(ggpacket(mtcars))
  }, paste(colnames(mtcars)[1:5], collapse = "\\W+"))
})

test_that("ggpacket prints section for bound aesthetics mapping in console output", {
  expect_output({
    print(ggpacket())
  }, "Aesthetic Mapping")
})

test_that("ggpacket with bound, empty aesthetic mapping prints empty aesthetics", {
  expect_output({
    print(ggpacket())
  }, trimws(capture.output(aes())[-1]))
})

test_that("ggpacket prints bound aesthetic mapping", {
  # useBytes to avoid non-problematic Windows-specific encoding issues
  expect_output({
    print(ggpacket(mtcars, aes(x = wt)))
  }, utils::capture.output(aes(x = wt))[-1], fixed = TRUE, useBytes = TRUE)
})

test_that("ggpacket prints section for ggcall layers", {
  expect_output({
    print(ggpacket())
  }, "Layers")
})

test_that("ggpacket without ggcall layers displays empty layers in console output", {
  expect_output({
    print(ggpacket())
  }, "Layers:\\n\\s*empty")
})


test_that("ggpacket with a bound ggcall layer prints bound ggcall expression", {
  expect_output({
    print(ggpacket() + geom_line())
  }, "\\[1\\] #line\\W+geom_line()")
})

test_that("ggpacket lacking required aesthetics indicates aesthetic missing", {
  expect_match({
    paste(capture.output({
      ggpacket() + geom_line()
    }), collapse = "\n")
  }, c("`x` -> <missing>"))

  expect_match({
    paste(capture.output({
      ggpacket() + geom_line()
    }), collapse = "\n")
  }, c("`y` -> <missing>"))
})

test_that("ggpacket including required aesthetics indicates value", {
  expect_output({
    print(ggpacket(aes(x = wt)) + geom_line())
  }, c("`x` -> `wt`"))
})

test_that("ggpacket including required aesthetics considers internal remappings", {
  expect_true({
    !any(grepl(
      "missing",
      capture.output(ggpacket(aes(y = test)) + geom_line(aes(x = ..y..)))
    ))
  })
})

test_that("show(<ggpacket>) behaviors identical to print for command line output", {
  expect_identical(
    capture.output(show(ggpacket(aes(y = test)) + geom_line(aes(x = ..y..)))),
    capture.output(print(ggpacket(aes(y = test)) + geom_line(aes(x = ..y..))))
  )
})
