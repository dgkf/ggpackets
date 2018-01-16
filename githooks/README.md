# Some `git hooks` for helping developers

> **Warning**  
> `git hooks` are quite a powerful tool, allowing you to execute code that is triggered by `git` actions. However, it's important to thoroughly vet the code that will be executed before adding any potentially malicious scripts.

## `run-tests.r`

This script will run `devtools::test()`. It's recommended to run this script during the `pre-commit` hook. It can also be used to intercept `push` requests that introduce failing unit tests.

```r
# recommended running of unit tests during commits
devtools::use_git_hook('pre-commit', 'Rscript ./githooks/run-tests.r')

# optional prevention of unit test failures in push requests
devtools::use_git_hook('pre-push', 'Rscript ./githooks/run-tests.r --stop-on-fail')
```

If you want to ensure the command worked as intended, you should be able to find a new file (named with the string passed as the first argument in the commands above) in the `./.git/hooks/` directory with the command specified inside it. 
