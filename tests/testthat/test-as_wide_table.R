austraits <- austraits_lite
austraits_post <- austraits_lite_post

  test_that("Function is working", {
    expect_visible(austraits %>% as_wide_table())
    expect_named(austraits %>% as_wide_table())
    expect_type(austraits %>% as_wide_table(), "list")
    
    expect_visible(austraits_post %>% as_wide_table())
    expect_named(austraits_post %>% as_wide_table())
    expect_type(austraits_post %>% as_wide_table(), "list")
  })
  
  test_that("Output is correct", {
    expect_equal(austraits %>% as_wide_table() %>% nrow(), austraits$traits %>% nrow())
    expect_gt(austraits %>% as_wide_table() %>% ncol(), expected = austraits$traits %>% ncol())
    expect_gt(austraits_post %>% as_wide_table() %>% ncol(), expected = austraits$traits %>% ncol())
  })
  
  test_that("Complains at the right time", {
    expect_error(as_wide_table())
  })


  