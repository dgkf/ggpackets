context("Aesthetic Handling => add_eqv_aes")
test_that("Adding base equivalent aesthetics adds Americanized names and base names", {
  expect_true( all(c('color', 'bg', 'fg') %in% add_eqv_aes(c('colour', 'fill'))) )
})

test_that("Ensure that terms separate by multiple relationships are still added (e.g. color => colour => fg)", {
  expect_true( all(c('fg') %in% add_eqv_aes(c('color'))) )
})



context("Aesthetic Handling => allowed_aesthetics")

test_that("Allowed aesthetics returns vector that includes Geom required aesthetics", {
  expect_true( 'label' %in% allowed_aesthetics(GeomLabel) )
})

test_that("Allowed aesthetics returns vector that includes added base equivalent aesthetics", {
  expect_true( all(c('bg', 'fg') %in% allowed_aesthetics(GeomLabel)) )
})

test_that("Allowed aesthetics returns all aesthetcis if geom is NULL", {
  expect_true( length(allowed_aesthetics(NULL)) > 0 )
})



context("Aesthetic Handling => filter_aesthetics")

test_that("inappropriate aesthetics for the specified Geom get filtered and moved to group", {
  expect_equal(
    filter_aesthetics(GeomBar, aes(x = test1, y = test2, linestyle = test3)),
    aes(x = test1, y = test2, group = interaction(test3))
  )
})

test_that("inappropriate Geom aesthetics get converted to interaction term in group", {
  expect_equal(
    filter_aesthetics(GeomBar, aes(x = test1, y = test2, group = test3, linestyle = test4)),
    aes(x = test1, y = test2, group = interaction(test3, test4))
  )
})

test_that("test that flattening aesthetics into group works as intended (dots provided)", { 
  expect_equal(
    flatten_aesthetics_to_group(aes(x = test1, y = test2, color = test3, group = test4, linestyle = test5), 'linestyle'),
    aes(x = test1, y = test2, color = test3, group = interaction(test4, test5), linestyle = test5)
  )
})

test_that("test that flattening aesthetics into group works as intended (no dots provided)", { 
  expect_equal(
    flatten_aesthetics_to_group(aes(x = test1, y = test2, color = test3, group = test4, linestyle = test5)),
    aes(x = test1, y = test2, color = test3, group = interaction(test3, test4, test5), linestyle = test5)
  )
})

test_that("test that aesthetics can be removed and preserved in group", { 
  expect_equal(
    remove_aesthetics(aes(x = test1, y = test2, color = test3, group = test4, linestyle = test5), "linestyle"),
    aes(x = test1, y = test2, color = test3, group = interaction(test4, test5))
  )
})



context("Aesthetic Handling => flatten_aesthetics_to_group")

test_that("Specified aesthetic names can be converted to interaction terms in group.", {
  expect_equal(
    flatten_aesthetics_to_group(aes(x = test1, y = test2, color = test3, fill = test4), 'color', 'fill'),
    aes(x = test1, y = test2, color = test3, fill = test4, group = interaction(test3, test4))
  )
})

test_that("Specified aesthetic names can be specified irrespective of localization.", {
  expect_equal(
    flatten_aesthetics_to_group(aes(x = test1, y = test2, colour = test3, fill = test4), 'color', 'fill'),
    aes(x = test1, y = test2, color = test3, fill = test4, group = interaction(test3, test4))
  )
})

test_that("Required aesthetics x & y are not dropped.", {
  expect_equal(
    flatten_aesthetics_to_group(aes(x = test1, y = test2, color = test3, fill = test4), 'y', 'fill'),
    aes(x = test1, y = test2, color = test3, fill = test4, group = interaction(test4))
  )
})



context("Aesthetic Handling => split_aes_from_dots")

test_that("Splitting aes from dots pulls out common aesthetics", {
  expect_equal(
    split_aes_from_dots(x = test1, test_var = test2, my_var = test3, color = test4, bg = test5),
    list(aes = aes(x = test1, colour = test4, fill = test5),
         not_aes = aes(test_var = test2, my_var = test3))
  )
})

test_that("Splitting aes respects Geom specific refinement", {
  expect_equal(
    split_aes_from_dots(geom = GeomBar, x = test1, y = test2, vjust = test3),
    list(aes = aes(x = test1, y = test2),
         not_aes = aes(vjust = test3))
  )
})



context("Aesthetic Handling => remove_aesthetics")

test_that("Ensure specified aesthetics get removed and moved to group.", {
  expect_equal(
    remove_aesthetics(aes(x = test1, y = test2, color = test3, fill = test4), 'fill'),
    aes(x = test1, y = test2, color = test3, group = interaction(test4))
  )
})

test_that("Ensure specified aesthetics get removed even with mismatched localization.", {
  expect_equal(
    remove_aesthetics(aes(x = test1, y = test2, color = test3), 'colour'),
    aes(x = test1, y = test2, group = interaction(test3))
  )
})



context("Aesthetic Handling => is_uneval")

test_that("Ensure uneval classes are evaluated correctly.", {
  expect_true(is_uneval(as.symbol('test')))   # symbols     TRUE
  expect_true(is_uneval(call('paste')))       # calls       TRUE
  expect_true(is_uneval(quote(y <- x * 10)))  # expressions TRUE
  expect_false(is_uneval('test'))             # char        FALSE
  expect_false(is_uneval(3))                  # numerics    FALSE
})




