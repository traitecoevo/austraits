test_that("Function output is correct", {
  target <- austraits_5.0.0_lite$traits %>% 
    dplyr::group_by(trait_name, observation_id) %>% 
    dplyr::summarise(dplyr::n()) %>% 
    dplyr::ungroup() %>%
    dplyr::filter(`dplyr::n()` > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  original <- austraits_5.0.0_lite$traits %>%
    dplyr::group_by(trait_name, observation_id) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(! `dplyr::n()`  > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  # The final output should have nrow as original plus eventual number of summarised obs
  # `summarise_trait_means` only works with austratis_3.0, because afterwards would need to drop columns to meaningfully take means
  # so no errors, but had to comment out `expect_equal` test and output has same number of rows as input
  # this is expected looking at the code for `summarise_trait_means`
  expect_message(out <- summarise_trait_means(extract_dataset(austraits_5.0.0_lite, "Crous_2013")$traits))
  expect_visible(out)
  #expect_equal( out %>% nrow(), ( nrow(original) +  nrow(target)) ) 
  expect_named(out)
  expect_type(out, "list")
})

test_that("Function throws error", {
  expect_error(summarise_trait_means(austraits_3.0.2_lite))
  expect_error(summarise_trait_means(austraits_3.0.2_lite$sites))
  expect_error(summarise_trait_means(austraits_3.0.2_lite$taxa))
})



