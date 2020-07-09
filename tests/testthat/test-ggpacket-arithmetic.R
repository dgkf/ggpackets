test_that("<ggpacket> + <ggplot layer constructor> adds as ggcall", {
  expect_equal({
    ggpk <- ggpacket() + geom_line()
    ggpk@ggcalls[[1]][[1]]
  }, {
    rlang::quo(geom_line)
  })

  expect_equal({
    ggpk <- ggpacket() + geom_line(size = 3L)
    as.list(ggpk@ggcalls[[1]][2])
  }, {
    list(size = rlang::quo(3L))
  })

  expect_equal({
    ggpk <- ggpacket() + geom_line(size = 3L) + geom_point(size = 4L)
    as.list(ggpk@ggcalls[[2]][2])
  }, {
    list(size = rlang::quo(4L))
  })
})

test_that("<ggpacket> + <ggpacket> adds ggpacket as nested ggcall", {
  expect_equal({
    ggpk <- (ggpacket() + geom_line()) + (ggpacket() + geom_line())
    ggpk@ggcalls[[1]]
  }, {
    ggpk <- (ggpacket() + geom_line())
    ggpk@ggcalls[[1]]
  })

  expect_equal({
    ggpk <- (ggpacket() + geom_line()) + (ggpacket() + geom_line())
    ggpk@ggcalls[[2]]
  }, {
    ggpk <- (ggpacket() + (ggpacket() + geom_line()))
    ggpk@ggcalls[[1]]
  })
})


