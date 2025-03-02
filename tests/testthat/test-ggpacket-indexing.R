test_that("ggpackets can be indexed by numeric index to subset ggcalls", {
  expect_equal({
    ggpk <- ggpacket() + geom_line() + geom_point() + geom_bar()
    ggpk[[2]]
  }, {
    ggpk <- ggpacket() + geom_point()
    ggpk
  })

  expect_equal({
    ggpk <- ggpacket() + geom_line() + geom_point() + geom_bar()
    ggpk[2:3]
  }, {
    ggpk <- ggpacket() + geom_point() + geom_bar()
    ggpk
  })
})

test_that("ggpackets can be indexed by character id to subset ggcalls", {
  expect_equal({
    ggpk <- ggpacket() + geom_line() + geom_point() + geom_bar()
    ggpk["point"]
  }, {
    ggpk <- ggpacket() + geom_point()
    ggpk
  })

  expect_equal({
    ggpk <- ggpacket() + geom_line() + geom_point() + geom_bar()
    ggpk[c("line", "point")]
  }, {
    ggpk <- ggpacket() + geom_line() + geom_point()
    ggpk
  })

  expect_equal({
    ggpk <- ggpacket() %+%
      geom_line() %+%
      geom_point() %+%
      geom_bar(.id = "line")
    ggpk["line"]
  }, {
    ggpk <- ggpacket() %+% geom_line() %+% geom_bar(.id = "line")
    ggpk
  })
})
