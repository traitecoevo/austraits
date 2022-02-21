test_that("Function works", {
  expect_visible(austraits_lite %>% print_austraits("family"))
  expect_visible(austraits_lite %>% print_austraits("genus"))
  expect_visible(austraits_lite %>% print_austraits("trait_name"))
})

test_that("Throws errors", {
  expect_error(austraits_lite %>% print_austraits("observation_id"))
  expect_error(austraits_lite %>% print_austraits("trait"))
  expect_error(austraits_lite %>% print_austraits("unit"))
  expect_error(austraits_lite %>% print_austraits("source"))
  
})

test_that("Output correct", {
  family <- austraits_lite %>% print_austraits("family")
  genus <- austraits_lite %>% print_austraits("genus")
  trait_nm <- austraits_lite %>% print_austraits("trait_name")
  
  expect_length(family, 5)
  expect_length(genus, 5)
  expect_length(trait_nm, 5)
  
  expect_named(family, expected = c("family", "n_records", "n_dataset", "n_taxa", "percent_total"))
  expect_named(genus, expected = c("genus", "n_records", "n_dataset", "n_taxa", "percent_total"))
  expect_named(trait_nm, expected = c("trait_name", "n_records", "n_dataset", "n_taxa", "percent_total"))
  
  expect_equal(nrow(family), austraits_lite$taxa$family %>% unique() %>% length())
  expect_equal(nrow(genus), austraits_lite$taxa$genus %>% unique() %>% length())
  expect_equal(nrow(trait_nm), austraits_lite$traits$trait_name %>% unique() %>% length())
})
