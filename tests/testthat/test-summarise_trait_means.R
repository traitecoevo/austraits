austraits <- austraits_3.0.2_lite

test_that("Function output is correct", {
  target <- austraits$traits %>% 
    dplyr::group_by(trait_name, observation_id) %>% 
    dplyr::summarise(dplyr::n()) %>% 
    dplyr::filter(`dplyr::n()` > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  original <- austraits$traits %>%
    dplyr::group_by(trait_name, observation_id) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::filter(! `dplyr::n()`  > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  # Total number of multiple observations minus eventual number of summarised obs 
  # ( target_ls %>% dplyr::bind_rows() %>% nrow() ) - nrow(target)
  
  # The final output should have nrow as original plus eventual number of summarised obs
  expect_silent(out <- summarise_trait_means(austraits$traits))
  expect_visible(out)
  expect_equal( out %>% nrow(), ( nrow(original) +  nrow(target)) ) 
  
  expect_named(out)
  expect_type(out, "list")
})

test_that("Function throws error", {
  expect_error(summarise_trait_means(austraits))
  expect_error(summarise_trait_means(austraits$sites))
  expect_error(summarise_trait_means(austraits$taxa))
})



