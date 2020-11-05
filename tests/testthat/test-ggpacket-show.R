test_that("ggpackets print as expected", {
  expect_output({
    print(ggpacket())
  }, "<ggpacket>")

  expect_output({
    print(ggpacket())
  }, "Data")

  expect_output({
    print(ggpacket())
  }, "awaiting data")

  expect_output({
    print(ggpacket(mtcars))
  }, paste(colnames(mtcars)[1:5], collapse = "\\W+"))
  
  expect_output({
    print(ggpacket())
  }, "Aesthetic Mapping")

  expect_output({
    print(ggpacket())
  }, trimws(capture.output(aes())[-1]))

  expect_output({
    print(ggpacket(mtcars, aes(x = wt)))
  }, utils::capture.output(aes(x = wt))[-1], fixed = TRUE)

  expect_output({
    print(ggpacket())
  }, "Layers")

  expect_output({
    print(ggpacket())
  }, "empty")

  expect_output({
    print(ggpacket() + geom_line())
  }, "\\[1\\] #line\\W+geom_line()")
})
