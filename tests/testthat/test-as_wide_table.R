
test_that("Function is working", {
    expect_visible(austraits_5.0.0_lite %>% as_wide_table())
    out <- austraits_5.0.0_lite %>% as_wide_table()
    expect_named(out)
    expect_type(out, "list")
 
    # "Output is correct"
    expect_gt(out %>% ncol(), expected = austraits_5.0.0_lite$traits %>% ncol())
  })

expect_error(as_wide_table())


test_that("old versions will complain", {
    expect_error(as_wide_table(austraits_3.0.2_lite))
    expect_error(as_wide_table(austraits_4.2.0_lite))
    expect_error(join_contexts_old(austraits_3.0.2_lite))
    expect_error(join_contexts_old(austraits_4.2.0_lite))
  })


test_that("function `collapse_cols` works", {
    data <- (austraits_5.0.0_lite %>% extract_dataset("Bloomfield_2018"))$locations
    data_collapsed <- (data %>% collapse_cols())[[1]]
    expect_equal(str_count(data_collapsed, "="), 5)
    expect_equal(str_count(data_collapsed, "; "), 4)
})
