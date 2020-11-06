# make sure dependencies are satisfied
if (!require("usethis")) install.packages("usethis")
if (!require("git2r"))   install.packages("git2r")

# bump dev version
usethis:::use_description_field(
  "Version",
  usethis:::choose_version("dev"),
  overwrite = TRUE)

# add modified DESCIRPTION file prior to commit
git2r::add(path = "DESCRIPTION")

