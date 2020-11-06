# `git hooks` 

Automatically bump development version of the package with a new git commit:

```r
usethis::use_git_hook("pre-commit", "Rscript ./.githooks/pre-commit.R")
usethis::use_git_hook("pre-push", "Rscript ./.githooks/pre-push.R")
```
