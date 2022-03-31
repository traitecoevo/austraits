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


test_that("Function works", {
  expect_visible(austraits_lite %>% summarise_austraits("family"))
  expect_visible(austraits_lite %>% summarise_austraits("genus"))
  expect_visible(austraits_lite %>% summarise_austraits("trait_name"))
})

test_that("Throws errors", {
  expect_error(austraits_lite %>% summarise_austraits("observation_id"))
  expect_error(austraits_lite %>% summarise_austraits("trait"))
  expect_error(austraits_lite %>% summarise_austraits("unit"))
  expect_error(austraits_lite %>% summarise_austraits("source"))
  
})

test_that("Output correct", {
  family <- austraits_lite %>% summarise_austraits("family")
  genus <- austraits_lite %>% summarise_austraits("genus")
  trait_nm <- austraits_lite %>% summarise_austraits("trait_name")
  
  expect_length(family, 5)
  expect_length(genus, 5)
  expect_length(trait_nm, 5)
  
  expect_named(family, expected = c("family", "n_records", "n_dataset", "n_taxa", "percent_total"))
  expect_named(genus, expected = c("genus", "n_records", "n_dataset", "n_taxa", "percent_total"))
  expect_named(trait_nm, expected = c("trait_name", "n_records", "n_dataset", "n_taxa", "percent_total"))
  
  family_vec <- austraits_lite$taxa$family %>% unique()
  genus_vec <- austraits_lite$taxa$genus %>% unique()
  
  expect_equal(nrow(family), family_vec[!is.na(family_vec)] %>% length())
  expect_equal(nrow(genus), genus_vec[!is.na(genus_vec)] %>% length())
  expect_equal(nrow(trait_nm), austraits_lite$traits$trait_name %>% unique() %>% length())
})

