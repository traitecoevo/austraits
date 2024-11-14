not_supported_austraits <- list(austraits_3.0.2_lite, austraits_4.2.0_lite) 

                austraits_5.0.0_lite


test_that("Function works", {
    expect_visible(austraits_5.0.0_lite %>% summarise_database("family"))
    expect_visible(austraits_5.0.0_lite %>% summarise_database("genus"))
    expect_visible(austraits_5.0.0_lite %>% summarise_database("trait_name"))
  })


test_that("Throws errors", {
    expect_error(austraits_5.0.0_lite %>% summarise_database("observation_id"))
    expect_error(austraits_5.0.0_lite %>% summarise_database("trait"))
    expect_error(austraits_5.0.0_lite %>% summarise_database("unit"))
    expect_error(austraits_5.0.0_lite %>% summarise_database("source"))
    
  })

test_that("Output correct", {
    family <- austraits_5.0.0_lite %>% summarise_database("family")
    genus <- austraits_5.0.0_lite %>% summarise_database("genus")
    trait_nm <- austraits_5.0.0_lite %>% summarise_database("trait_name")
    
    expect_length(family, 5)
    expect_length(genus, 5)
    expect_length(trait_nm, 5)
    
    expect_named(family, expected = c("family", "n_records", "n_dataset", "n_taxa", "percent_total"))
    expect_named(genus, expected = c("genus", "n_records", "n_dataset", "n_taxa", "percent_total"))
    expect_named(trait_nm, expected = c("trait_name", "n_records", "n_dataset", "n_taxa", "percent_total"))
    
    actual_family <- austraits_5.0.0_lite$taxa$family %>% unique() 
    actual_genus <- austraits_5.0.0_lite$taxa$genus %>% unique() 
    
    expect_equal(nrow(family), actual_family[! is.na(actual_family)] %>% length())
    expect_equal(nrow(genus), actual_genus[! is.na(actual_genus)] %>% length())
    expect_equal(nrow(trait_nm), austraits_5.0.0_lite$traits$trait_name %>% unique() %>% length())
  })

