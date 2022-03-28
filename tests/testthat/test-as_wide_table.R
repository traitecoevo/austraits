austraits <- austraits_lite

test_that("Function is working", {
  expect_visible(austraits %>% as_wide_table())
  expect_named(austraits %>% as_wide_table())
  expect_type(austraits %>% as_wide_table(), "list")
})

test_that("Output is correct", {
  expect_equal(austraits %>% as_wide_table() %>% nrow(), austraits$traits %>% nrow())
  expect_gt(austraits %>% as_wide_table() %>% ncol(), expected = austraits$traits %>% ncol())
})

test_that("Complains at the right time", {
  expect_error(austraits$traits %>% as_wide_table())
})


  