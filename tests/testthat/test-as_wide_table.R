austraits <- austraits_lite

test_that("Function is working", {
  expect_visible(austraits %>% as_wide_table())
  expect_named(austraits %>% as_wide_table())
  expect_type(austraits %>% as_wide_table(), "list")
})
