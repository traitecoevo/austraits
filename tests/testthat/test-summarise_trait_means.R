austraits <- austraits_lite

test_that("Function output is correct", {
  target <- austraits$traits %>% 
    dplyr::group_by(trait_name, observation_id) %>% 
    dplyr::summarise(dplyr::n()) %>% 
    dplyr::filter(`dplyr::n()` > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  target_ls <- purrr::map2(target$trait_name, target$observation_id,
                           ~ dplyr::filter(austraits$traits, trait_name == .x & observation_id == .y)) 
  
  original <- austraits$traits %>%
    dplyr::group_by(trait_name, observation_id) %>%
    dplyr::summarise(dplyr::n()) %>%
    dplyr::filter(! `dplyr::n()`  > 1) %>%
    dplyr::select(trait_name, observation_id)
  
  original_df <- purrr::map2_dfr(original$trait_name, original$observation_id,
                                 ~ dplyr::filter(austraits$traits, trait_name == .x & observation_id == .y))
  
  # Total number of multiple observations minus eventual number of summarised obs 
  ( target_ls %>% dplyr::bind_rows() %>% nrow() ) - nrow(target)
  
  # The final output should have nrow as original plus eventual number of summarised obs
  expect_equal( summarise_trait_means(austraits$traits) %>% nrow(), ( nrow(original) +  nrow(target)) ) 
  
})

test_that("Function throws error", {
  expect_error(summarise_trait_means(austraits))
  expect_error(summarise_trait_means(austraits$sites))
  expect_error(summarise_trait_means(austraits$taxa))
})

test_that("Function doesn't complain", {
  expect_silent(summarise_trait_means(austraits$traits))
  expect_visible(summarise_trait_means(austraits$traits))
  expect_named(summarise_trait_means(austraits$traits))
  expect_type(summarise_trait_means(austraits$traits), "list")
})

