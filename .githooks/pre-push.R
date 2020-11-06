# run package tests
test_results <- devtools::test()

if (any(as.data.frame(test_results)$failed)) {
  message("\n\nErrors encountered while running tests. Push anyways? [y/N] \n")
  resp <- readline(prompt = "")
  if (toupper(resp) != "Y") quit(status = 1L)
}

