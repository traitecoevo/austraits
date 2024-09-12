test_that("Function output is correct", {
  target <- austraits_3.0.2_lite$traits %>% 
    dplyr::group_by(trait_name, observation_id) %>% 
    dplyr::summarise(dplyr::n()) %>% 
    dplyr::filter(`dplyr::n()` > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  original <- austraits_3.0.2_lite$traits %>%
    dplyr::group_by(trait_name, observation_id) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::filter(! `dplyr::n()`  > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  # The final output should have nrow as original plus eventual number of summarised obs
  expect_message(out <- summarise_trait_means(austraits_3.0.2_lite$traits))
  expect_visible(out)
  expect_equal( out %>% nrow(), ( nrow(original) +  nrow(target)) ) 
  expect_named(out)
  expect_type(out, "list")
})

test_that("Function throws error", {
  expect_error(summarise_trait_means(austraits_3.0.2_lite))
  expect_error(summarise_trait_means(austraits_3.0.2_lite$sites))
  expect_error(summarise_trait_means(austraits_3.0.2_lite$taxa))
})



