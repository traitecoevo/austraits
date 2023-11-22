library(purrr)

austraits <- list(austraits_3.0.2_lite,
                  austraits_4.2.0_lite, 
                  austraits_5.0.0_lite)

test_summarise <- function(austraits){
  test_that("Function works", {
    expect_visible(austraits %>% summarise_austraits("family"))
    expect_visible(austraits %>% summarise_austraits("genus"))
    expect_visible(austraits %>% summarise_austraits("trait_name"))
  })
}

map(austraits, 
    ~test_summarise(.x))

test_summarise_errors <- function(austraits){
  test_that("Throws errors", {
    expect_error(austraits %>% summarise_austraits("observation_id"))
    expect_error(austraits %>% summarise_austraits("trait"))
    expect_error(austraits %>% summarise_austraits("unit"))
    expect_error(austraits %>% summarise_austraits("source"))
    
  })
}

map(austraits, 
    ~test_summarise_errors(.x))

test_summarise_output <- function(austraits){
  test_that("Output correct", {
    family <- austraits %>% summarise_austraits("family")
    genus <- austraits %>% summarise_austraits("genus")
    trait_nm <- austraits %>% summarise_austraits("trait_name")
    
    expect_length(family, 5)
    expect_length(genus, 5)
    expect_length(trait_nm, 5)
    
    expect_named(family, expected = c("family", "n_records", "n_dataset", "n_taxa", "percent_total"))
    expect_named(genus, expected = c("genus", "n_records", "n_dataset", "n_taxa", "percent_total"))
    expect_named(trait_nm, expected = c("trait_name", "n_records", "n_dataset", "n_taxa", "percent_total"))
    
    actual_family <- austraits$taxa$family %>% unique() 
    actual_genus <- austraits$taxa$genus %>% unique() 
    
    expect_equal(nrow(family), actual_family[! is.na(actual_family)] %>% length())
    expect_equal(nrow(genus), actual_genus[! is.na(actual_genus)] %>% length())
    expect_equal(nrow(trait_nm), austraits$traits$trait_name %>% unique() %>% length())
  })
}

map(austraits, 
    ~ test_summarise_output(.x))
