context("Documentation => document_ggpk")

test_that("document_ggpk properly formats ... param", {
  
  f <- function(...) {
    ggpack(ggplot2::geom_bar, id = list(NULL, 'ggpk1'), color = 'red', ...) + 
    ggpack(ggplot2::geom_point, id = list(NULL, 'ggpk2'), ..., color = 'green') + 
    ggpack(ggplot2::geom_point, id = 'ggpk3', ..., dots = list(color = 'blue'))
  }
  
  expect_equal(length(find_calls(as.expression(f), 'ggpack')), 3)
  expect_output(document_ggpk(f), paste0(rep('\\item\\s.+', 3), collapse = ''))
  expect_output(document_ggpk(f), '\\itemize')
  expect_output(document_ggpk(f), '@param \\.\\.\\.')
  expect_output(document_ggpk(f), "^#'")
  
})