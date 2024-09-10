
test_that("Function is working", {
    expect_visible(austraits_5.0.0_lite %>% as_wide_table())
    out <- austraits_5.0.0_lite %>% as_wide_table()
    expect_named(out)
    expect_type(out, "list")
 
    # "Output is correct"
    expect_gt(out %>% ncol(), expected = austraits_5.0.0_lite$traits %>% ncol())
  })

expect_error(as_wide_table())
