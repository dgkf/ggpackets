library(methods)

args <- commandArgs(trailingOnly=TRUE)
cryn <- require(crayon, quietly = TRUE)

cat((if (cryn) blue else cat)(paste0(
'\n\n',
'Running test cases from R command line: \n',
'> devtools::test()\n'
)))

if ((s <- sum(as.data.frame(suppressMessages(devtools::test()))$failed)) > 0) { 
  cat((if (cryn) red else cat)(
    sprintf("\n\n%s tests failed. Please consider resolving test cases.\n\n", s)
  ))
  if ('--stop-on-fail' %in% args) quit(status = 1)
} else cat((if (cryn) green else cat)("\nAll tests passed!\n\n"))